library(magrittr)
library(fOptions)
library(lubridate)
library(xts)
library(dygraphs)
iron3min <- read.csv("hedge/iron3min.csv", stringsAsFactors = F)

#将数据按合约拆分，筛选出整数、半点数据
contract = list()
for (i in 1:8) {
  contract[[i]] = cbind(ymd_hm(iron3min[, (9-i)*5-4]), iron3min[, ((9-i)*5-3):((9-i)*5-2)]) %>% na.omit
  filtTime = contract[[i]][1]
  filtPinInteger = endpoints(filtTime[,1], on = "hours")+1
  filtPinHalf = which(minute(filtTime[,1])==30)
  filtPin = sort(c(filtPinInteger, filtPinHalf))
  contract[[i]] = contract[[i]][filtPin, ] %>% na.omit()
}

#列出期权交易日期在contract变量中的指针
DateTr = ymd_hm(c("2016-11-15 20:59", "2017-03-15 20:59", "2017-07-14 20:59", "2017-11-15 20:59", "2018-03-15 20:59", "2018-07-13 20:59", "2018-11-15 20:59", "2019-03-15 20:59", "2019-05-25 20:59"))  #注意双休日，隔两天
pin = list()     #选出交易期权的日期的指针
# Index = list()  
# contractIndex = list()   #筛选出进行交易的日期的数据
# IndexdateSplit = list()   #交易期权的指针
for (i in 1:8){
  pin[[i]] = contract[[i]][,1] > DateTr[i] & contract[[i]][,1] <= DateTr[i+1] & minute(contract[[i]][,1])==3 & hour(contract[[i]][,1])==21
  pin[[i]] = which(pin[[i]])
}
#contract[[3]][pin[[3]][2],] %>% view()


#计算日内时间间隔相对于一天的百分比
intervalTime = interval(contract[[1]][pin[[1]][1]:(pin[[1]][1]+20), 1], contract[[1]][(pin[[1]][1]+20), 1]) #调仓的时点与当个交易日15:00的间隔
fractionTime = (as.numeric(as.duration(intervalTime)/86400))%%1   #可能中间双休日，取余


#-----------------------------------
#计算期权delta，每个算20次，记录期货价格，期货头寸等于前后delta变化；再分不同时段计算
sigOptionOpen = c()
priceOptionOpen = c()
deltaOptionOpen = c()
sigOptionHolding = list()
priceDayTimeList = list()
pointDayTimeList = list()
deltaDayTimeList = list()
n = 1

for (i in 1:8) {   #i:不同合约
  for (j in 1:length(pin[[i]])) {  #j:不同日期开仓期权
    
    pinForThisOne = pin[[i]][j]
    #先算option开仓的一个delta
    sigOptionOpen[n] = (var(diff(log(contract[[i]][(((rep(pinForThisOne,21)-1)-(20:0)*21)), 3])))*243)^0.5
    priceOptionOpen[n] = contract[[i]][pinForThisOne, 2]
    deltaOptionOpen[n] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceOptionOpen[n], X = priceOptionOpen[n], Time = 19.7479167/243, r = 0.03, b = 0, sigOptionOpen[n])
    
    #再算option随时间变化、不同时点对冲的delta
    sigOptionHolding = matrix(ncol = 1, nrow = 20)   #初始化声明变量
    priceDayTime = matrix(ncol = 21, nrow = 20)
    pointDayTime = data.frame(contract[[1]][1,1])
    deltaDayTime = matrix(ncol = 21, nrow = 20)
    for (k in 1:20) {   #k: 交易后的第几日。每种时间点再算19个delta,19天（最后一个平仓不算）
      sigOptionHolding[k] = (var(diff(log(contract[[i]][(((rep((pinForThisOne+k*21),21)-1)-(21:1)*21)), 3])))*243)^0.5
      #需要sigma，收盘价格
      for (h in 1:21) {    #h: 期货可以在21个不同时间点对冲 
        priceDayTime[k, h] = contract[[i]][pinForThisOne+(k-1)*21+(h-1),2]
        pointDayTime[k, h] = contract[[i]][pinForThisOne+(k-1)*21+(h-1),1]
        deltaDayTime[k, h] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceDayTime[k, h], X = priceOptionOpen[n], Time = (20-k+fractionTime[h])/243, r = 0.03, b = 0, sigOptionHolding[k])
      }
    }
    priceDayTimeList[[n]] = priceDayTime
    #pointDayTimeList[[n]] = pointDayTime
    deltaDayTimeList[[n]] = deltaDayTime
    names(priceDayTimeList)[n] = as.character((pointDayTime[1,1])) 
    n = n+1
  }
}


#计算期货交易价格的波动率，没交易的价格需要去除
VarComputingWithPosition = function(){
  pinOnVar = which(positionFutures[,j]!=0)
  (var(diff(log(priceFutures[pinOnVar,j])))*243)^0.5
}
#计算期货头寸、payoff
payoffFutures = matrix(nrow = 614, ncol = 21)
dataPointDivided = list()
for (i in 1:614) {
  #futuresComputing[[i]] = data.frame(date = names(priceDayTimeList)[i], deltaOption = c(deltaOptionOpen[i], price))
  deltaOption = rbind(deltaOptionOpen[i], deltaDayTimeList[[i]]) #每列是20天的期权delta，行代表不同对冲时间；首行加一行是第一笔交易的delta
  positionFutures = diff(deltaOption) #delta变化的差额是加仓的头寸
  positionFutures[1, 1] = 0; positionFutures[20, 21] = 0  #第一天期货和期权同时对冲，最后一天期权收盘时不再对冲
  positionFutures = rbind(deltaOptionOpen[i], positionFutures) #期权开仓先对冲一笔
  positionFutures = rbind(positionFutures, -apply(positionFutures, 2, sum)) #最后平仓
  
  priceFutures = rbind(priceOptionOpen[i], priceDayTimeList[[i]], priceDayTimeList[[i]][20, 21]) #加上开仓价和平仓价
  priceFutures[positionFutures > 0] = priceFutures[positionFutures > 0] + 0.5  #滑点
  priceFutures[positionFutures < 0] = priceFutures[positionFutures < 0] - 0.5
  
  payoffFutures[i,] = (-apply(positionFutures*priceFutures, 2, sum) - 0.0001*apply(abs(positionFutures), 2, sum))*100
  
  for (j in 1:21) {
    if (i==1) {
      dataPointDivided[[j]] = matrix(nrow = 614*2, ncol = 23, dimnames = list(1:(614*2),1:23))
      colnames(dataPointDivided[[j]]) = c("Open", paste0("T+", 0:19), "Close", "Payoff/Volitality")
    }
    dataPointDivided[[j]][(2*i-1):(2*i),] = rbind(c(positionFutures[,j], payoffFutures[i, j]), c(priceFutures[,j], VarComputingWithPosition())) 
    dateTraded = format(as.Date(names(priceDayTimeList)[i]), format = "%y%m%d")
    rownames(dataPointDivided[[j]])[(2*i-1):(2*i)] = c(paste("N", dateTraded), paste("P", dateTraded))
  }
}

#日期、时间序列名称，命名
pointTiming = contract[[1]][10:30, 1] %>% as.character() %>% substr(., 12, 16)
pointCalendar = names(priceDayTimeList)[1:614] %>% substr(., 3, 10)
dimnames(payoffFutures) = list(pointCalendar, pointTiming)

payoffOption = sapply(1:614, function(x) {priceDayTimeList[[x]][1,1]-priceDayTimeList[[x]][20,21]})*100
payoffOption[payoffOption>0] = 0
payoffFuturesTotal = apply(payoffFutures, 2, sum)
payoffOptionTotal = sum(payoffOption)
sort(payoffFuturesTotal, decreasing = T)

payoffCombined = rbind(c(payoffFuturesTotal,payoffOptionTotal), cbind(payoffFutures, payoffOption))
colnames(payoffCombined)[22] = "Option"; rownames(payoffCombined)[1] = "Total"

#dataPointDivided[[1]][seq(1,1228,2), 23] %>% sum
#payoffFuturesTotal[1]

#计算期货payoff+期权payoff，单独列出波动率
payoffCombinedFO = payoffCombined[, 1:21] + payoffCombined[, 22]
dataPointDividedFO = dataPointDivided; volatilityFutures = payoffCombinedFO[2:615, ]
for (i in 1:21) {
  dataPointDividedFO[[i]][seq(1, 1228, 2), 23] = dataPointDividedFO[[i]][seq(1, 1228, 2), 23] + payoffOption
  volatilityFutures[,i] = dataPointDivided[[i]][seq(2, 1228, 2), 23]
}



#-----------------------------------------------------------
#作图，结果分析
#找出期权换合约的列表下标,画图(期货payoff max)
pointSwift = ymd_hm(c("2016-11-15 20:59", "2017-03-15 20:59", "2017-07-14 20:59", "2017-11-15 20:59", "2018-03-15 20:59", "2018-07-13 20:59", "2018-11-15 20:59", "2019-03-15 20:59", "2019-05-25 20:59")) %>% as.Date()
pointCalendarL = names(priceDayTimeList)[1:614] %>% substr(., 1, 10) %>% as.Date()
pointLabAxis = pointCalendarL %in% pointSwift  

i = which.max(payoffFuturesTotal)
plot(cumsum(payoffFutures[,i]), type = "l", col = "red", xaxt = "n", xlab = "期权开仓日期", ylab = "累积收益", ylim = c(-1750000, 500000),main = paste("对冲时点", pointTiming[i]), sub = "I1705-I1909")#不同时点对冲的期货累积payoff
axis(1,at = seq(1,614)[pointLabAxis], labels = pointCalendar[pointLabAxis]) 
lines(cumsum(payoffOption))
legend("bottomleft", inset=, c("futures","options"),
       lty=c(1,1), col=c("red","black"),cex=0.85, bty = "n")


#画在一张图中
payoffFuturesCumsum = xts(apply(payoffFutures, 2, cumsum), order.by = as.Date(pointCalendarL))
colnames(payoffFuturesCumsum) = pointTiming
dygraph(payoffFuturesCumsum, main = "各时点对冲期货的累积payoff")%>%
  dyLegend(width = 800)

barplot(apply(payoffFutures, 2, sum), names.arg = pointTiming, xlab = "期货对冲时点", ylab = "期货payoff", main = "期货payoff")
# cor(payoffFutures, payoffOption)
# corPayoff = cbind(payoffFuturesTotal, cor(payoffFutures, payoffOption))
# corPayoff = corPayoff[order(corPayoff[,1], decreasing = T), ]

#输出
write.csv(payoffCombinedFO, "E:/R/hedge/delta_hedge_payoffCombined.csv")
for (i in 1:21) {
  write.csv(dataPointDividedFO[[i]], paste0("E:/R/hedge/delta_hedge_daily_",sub(":", "", pointTiming[i]),".csv"))
}
write.csv(volatilityFutures, "E:/R/hedge/delta_hedge_volatility.csv")
