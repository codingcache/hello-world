library(magrittr)
library(fOptions)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(dygraphs)
library(htmlwidgets)
library(webshot)
library(doParallel)
library(foreach)
rm(list = ls(all.names = T))
cl=makeCluster(4)
registerDoParallel(cl)
t1 = Sys.time()
#I=0
outputData = foreach(I = 0:36, .packages = c("magrittr", "fOptions", "lubridate", "xts")) %dopar% {
  outputData = NA
  try({
    # 全体回测 --------------------------------------------------------------------
    
    # 固定时点 --------------------------------------------------------------------
    
    #t1 = Sys.time()
    assign("typeForThisOne", read.csv(paste0("futuresdatas/type", as.character(I),".csv"), stringsAsFactors = F))
    contract = list()
    N = ncol(typeForThisOne)/5
    if (I==28) {
      N = N-1
    }
    for (i in 1:N) {
      contract[[i]] = cbind(ymd_hms(typeForThisOne[, (N+1-i)*5-4]), typeForThisOne[, ((N+1-i)*5-3):((N+1-i)*5-2)]) %>% na.omit
      filtTime = contract[[i]][,1]
      if (I==3 | I==8 | I==28) {   #无夜盘
        deleteTime = hour(filtTime) %in% c(0,1,2,21,22,23)
        filtTime = filtTime[!deleteTime]
        contract[[i]] = contract[[i]][!deleteTime, ]
      }else if (I %in% c(6, 32:36)) {        #一点收盘
        deleteTime = hour(filtTime)==2 | (hour(filtTime)==1 & minute(filtTime)!=0)
        filtTime = filtTime[!deleteTime]
        contract[[i]] = contract[[i]][!deleteTime, ]
      }else if (I %in% c(29, 30)){    #2:30收盘
        contract[[i]] = contract[[i]]
      }else {    #23:30收盘
        deleteTime = hour(filtTime) %in% c(0,1,2) | (hour(filtTime)==23 & minute(filtTime)>30)
        filtTime = filtTime[!deleteTime]
        contract[[i]] = contract[[i]][!deleteTime, ]
      }
      filtPinInteger = endpoints(filtTime, on = "hours")+1
      filtPinHalf = which(minute(filtTime)==30)
      filtPin1030 = which(minute(filtTime)==33 & hour(filtTime)==10)
      filtPin = sort(c(filtPinInteger, filtPinHalf, filtPin1030))
      contract[[i]] = contract[[i]][filtPin, ] %>% na.omit()
    }
    
    #列出期权交易日期在contract变量中的索引 
    #将日期判断间隔存在dateDivide变量
    dateDivide = c(contract[[1]][1,1])
    for (i in 1:N) {
      dateDivide[i] = contract[[i]][i, 1]+months(1)
      day(dateDivide[i]) = 15; hour(dateDivide[i]) = 20
      listingTime = year(contract[[i]][,1])==year(dateDivide[i]) & month(contract[[i]][,1])==month(dateDivide[i]) & hour(contract[[i]][,1])==9
      listingDay = day(contract[[i]][listingTime, 1]) %>% unique()       #筛选出 合约首月的下一个月 的交易日期（不包括周六凌晨）
      definingDay = listingDay %>% "-"(15) %>% "<="(0) %>% table %>% .[2] %>% listingDay[.]
      day(dateDivide[i]) = definingDay
    }
    listingDate = contract[[N]][,1][!(hour(contract[[N]][,1]) %in% c(0,1,2,21,22,23))] %>% as.Date() %>% unique()
    dateDivide[N+1] = listingDate[length(listingDate)-21]
    hour(dateDivide[N+1]) = 20
    
    #dateDivide = ymd_hm(c("2016-11-15 20:59", "2017-03-15 20:59", "2017-07-14 20:59", "2017-11-15 20:59", "2018-03-15 20:59", "2018-07-13 20:59", "2018-11-15 20:59", "2019-03-15 20:59", "2019-05-24 20:59"))  #注意双休日，隔两天
    pin = list()     #选出交易期权的日期的索引
    for (i in 1:N){
      if (I %in% c(3, 8, 28)) {
        pin[[i]] = contract[[i]][,1] > dateDivide[i] & contract[[i]][,1] <= dateDivide[i+1] & minute(contract[[i]][,1])==0 & hour(contract[[i]][,1])==9
      }else {
        pin[[i]] = contract[[i]][,1] > dateDivide[i] & contract[[i]][,1] <= dateDivide[i+1] & minute(contract[[i]][,1])==3 & hour(contract[[i]][,1])==21
      }
      pin[[i]] = which(pin[[i]])
    }
    #contract[[2]][pin[[2]][1],]
    
    ##############################
    # c = sapply(pin, length)
    # Cc = data.frame(x1 = contract[[i]][pin[[i]][1], 1], x2 = contract[[i]][pin[[i]][1], 1], x3 = contract[[i]][pin[[i]][1], 1])
    # c
    # for (i in 1:length(c)) {
    #   Cc[i, 1] = contract[[i]][pin[[i]][1], 1]
    #   Cc[i, 2] = contract[[i]][pin[[i]][2], 1]
    #   Cc[i, 3] = contract[[i]][pin[[i]][c[i]], 1]
    # }
    # Cc
    # contract[[2]][pin[[2]][1],]
    # I=35
    ##############################
    #计算日内时间间隔相对于一天的百分比
    fractionTime = difftime(contract[[1]][pin[[1]][2]-1, 1], contract[[1]][(pin[[1]][1]):(pin[[1]][2]-1), 1], units = "days") %>% as.numeric() %>% "%%"(1)
    L = length(fractionTime)
    fractionTime = rep(0, L)+10^-9
    # I=0
    #---------------------------------------------
    #计算期权delta，每个算20次，记录期货价格，期货头寸等于前后delta变化；再分不同时段计算
    sigOptionOpen = c()
    priceOptionOpen = c()
    deltaOptionOpen = c()
    priceDayTimeList = list()
    #pointDayTimeList = list()
    deltaDayTimeList = list()
    priceOption = c()
    n = 1; skip_count = 0
    #i=1;j=1;k=1;h=1
    for (i in 1:N) {   #i:不同合约
      #i=8
      for (j in 1:(length(pin[[i]])+skip_count)) {  #j:不同日期开仓期权
        if (j==1) {
          skip_run = skip_count   #新合约开始，先将skip值赋给新的一个计数器，用于在索引减去上个合约没算的的天数
          skip_fixed = skip_count   #如果上个合约没有少记，skip全为0，否则此合约内skip_fixed固定为上合约少记天数
        }
        
        if (skip_run!=0){
          pinForThisOne = pin[[i]][1]-L*skip_run
          skip_run = skip_run-1           #当该合约记录完上个合约少记的天数后就重置skip_run变量为零
        }else {
          pinForThisOne = pin[[i]][j-skip_fixed]  #e.g.少记4天，j=5时首次执行该行，skip_fixed=4
        }
        
        
        if (is.na(contract[[i]][pinForThisOne+L*20-1, 1])) {
          skip_count = skip_count+1     #统计有几天要移到后一个合约
          next()
        }
        
        skip_count = 0   #新合约开始后(j=1)，skip_count归零，之后若有NA，skip_count增加，但next已经跳过该循环
        #pinForThisOne = pin[[i]][j-skip_fixed]
        #先算option开仓的一个delta
        sigOptionOpen[n] = (var(diff(log(contract[[i]][(((rep(pinForThisOne,21)-1)-(20:0)*L)), 3])))*243)^0.5       ##################调整公式21天改20天
        priceOptionOpen[n] = contract[[i]][pinForThisOne, 2]
        deltaOptionOpen[n] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceOptionOpen[n], X = priceOptionOpen[n], Time = (19+fractionTime[1])/243, r = 0.03, b = 0.03, sigOptionOpen[n])
        #priceOption[n] = GBSOption(TypeFlag = "c", S = priceOptionOpen[n], X = priceOptionOpen[n], Time = (19+fractionTime[1])/243, r = 0.03, b = 0, sigOptionOpen[n])@price                    ################期权价格
        
        #再算option随时间变化、不同时点对冲的delta
        #sigOptionHolding = matrix(ncol = 1, nrow = 20)   #初始化声明变量
        priceDayTime = matrix(ncol = L, nrow = 20)
        pointDayTime = data.frame(contract[[1]][1,1])
        deltaDayTime = matrix(ncol = L, nrow = 20)
        for (k in 1:20) {   #k: 交易后的第几日。每种时间点再算20个delta,20天（最后一个平仓不算）
          #sigOptionHolding[k] = (var(diff(log(contract[[i]][(((rep((pinForThisOne+k*L),20)-1)-(20:1)*L)), 3])))*243)^0.5      ########################不计算每天sigma，只用sigOptionOpen[n]代替
          #需要sigma，收盘价格
          for (h in 1:L) {    #h: 期货可以在21个不同时间点对冲 
            if (h==1) {                                                                     ######################非21:03对冲改用收盘价
              priceDayTime[k, h] = contract[[i]][pinForThisOne+(k-1)*L+(h-1),2] 
            }else {
              priceDayTime[k, h] = contract[[i]][pinForThisOne+(k-1)*L+(h-1),3]
            }
            pointDayTime[k, h] = contract[[i]][pinForThisOne+(k-1)*L+(h-1),1]
            deltaDayTime[k, h] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceDayTime[k, h], X = priceOptionOpen[n], Time = (20-k+fractionTime[h])/243, r = 0.03, b = 0.03, sigOptionOpen[n])        ##################计算delta时改用sigOptionOpen[n]
          }
        }
        priceDayTimeList[[n]] = priceDayTime
        #pointDayTimeList[[n]] = pointDayTime
        deltaDayTimeList[[n]] = deltaDayTime
        names(priceDayTimeList)[n] = as.character((pointDayTime[1,1])) 
        n = n+1
      }
      #View(priceDayTimeList)
    }
    W = length(priceOptionOpen)  #W=614, L=21
    
    
    #输入每个期货品种的tick、乘数、手续费
    tick = c(0.5,0.5,0.5,2,2,0.2,10,1,1,5,1,1,1,2,2,1,1,1,5,5,2,1,5,1,1,1,1,1,1,1,0.05,2,5,10,10,5,5)
    multiplie = c(100,100,60,5,5,100,1,10,5,5,10,10,10,10,10,10,10,10,10,5,5,5,5,10,20,10,10,10,10,15,1000,10,5,5,1,5,5)
    fee_abs = c(NA,NA,NA,3,3,4,3,3,NA,4.3,1.2,1.5,2,2.5,2.5,2,1.5,1.5,NA,2,3,NA,2,NA,3,2,NA,NA,5,NA,10,NA,3,NA,6,NA,3)
    fee_prop = c(1,0.6,0.6,NA,NA,NA,NA,NA,1.5,NA,NA,NA,NA,NA,NA,NA,NA,NA,0.45,NA,NA,0.6,NA,0.5,NA,NA,1,1,NA,0.5,NA,1,NA,0.5,NA,0.4,NA)
    
    #计算期货头寸、payoff、volatility
    payoffFutures = matrix(nrow = W, ncol = L)
    volatilityFutures = matrix(nrow = W, ncol = L)
    payoffOption = c()
    for (i in 1:W) {
      #futuresComputing[[i]] = data.frame(date = names(priceDayTimeList)[i], deltaOption = c(deltaOptionOpen[i], price))
      deltaOption = rbind(deltaOptionOpen[i], deltaDayTimeList[[i]]) #每列是20天的期权delta，行代表不同对冲时间；首行加一行是第一笔交易的delta
      positionFutures = diff(deltaOption) #delta变化的差额是加仓的头寸
      positionFutures[1, 1] = 0; positionFutures[20, L] = 0  #第一天期货和期权同时对冲，最后一天期权收盘时不再对冲
      positionFutures = rbind(deltaOptionOpen[i], positionFutures) #期权开仓先对冲一笔
      positionFutures = rbind(positionFutures, -apply(positionFutures, 2, sum)) #最后平仓
      
      priceFutures = rbind(priceOptionOpen[i], priceDayTimeList[[i]], priceDayTimeList[[i]][20, L]) #加上开仓价和平仓价
      priceFutures[positionFutures > 0] = priceFutures[positionFutures > 0] + tick[I+1]  #滑点
      priceFutures[positionFutures < 0] = priceFutures[positionFutures < 0] - tick[I+1]
      
      if (I %in% c(0,1,2,8,18,21,23,26,27,29,31,33,35)) {  #计算手续费两种计费方式
        payoffFutures[i,] = (-apply(positionFutures*priceFutures, 2, sum) - 0.0001*fee_prop[I+1]*apply(abs(positionFutures)*priceFutures, 2, sum))*(10000000/priceFutures[2,1])    #手续费单位：总金额的万分之几   #加上名义本金
      }else {
        payoffFutures[i,] = -apply(positionFutures*priceFutures, 2, sum)*(10000000/priceFutures[2,1]) - fee_abs[I+1]*apply(abs(positionFutures), 2, sum)*(10000000/(priceFutures[2,1]*multiplie[I+1]))   #手续费单位：元/每手  #加上名义本金
      }
      
      #计算期货交易价格的波动率，没交易的价格需要去除
      for (j in 1:L) {
        #pinOnVar = which(positionFutures[,j]!=0)
        if (j==1) {
          volatilityFutures[i, j] = (var(diff(log(priceFutures[c(1, 3:21),j])))*243)^0.5         #只算20天的波动率
        }else if (j==L) {
          volatilityFutures[i, j] = (var(diff(log(priceFutures[c(2:20, 22),j])))*243)^0.5         #只算20天的波动率
        }else {
          volatilityFutures[i, j] = (var(diff(log(priceFutures[2:21,j])))*243)^0.5         #只算20天的波动率
        }
      }
      payoffOption[i] = (priceDayTimeList[[i]][1,1]-priceDayTimeList[[i]][20,L])*(10000000/priceDayTimeList[[i]][1,1])
      #priceOption[i] = priceOption[i] *(10000000/priceDayTimeList[[i]][1,1])                        ###################期权价格
    }
    
    
    #日期、时间序列名称，命名
    pointTiming = contract[[1]][pin[[1]][1]:(pin[[1]][1]+L-1), 1] %>% format(., "%H:%M")
    pointCalendar = names(priceDayTimeList)[1:W] %>% substr(., 3, 10)
    dimnames(payoffFutures) = list(pointCalendar, pointTiming)
    dimnames(volatilityFutures) = list(pointCalendar, pointTiming)
    
    #计算期权payoff、总payoff、平均volatility
    #payoffOption = sapply(1:W, function(x) {priceDayTimeList[[x]][1,1]-priceDayTimeList[[x]][20,L]})*multiplie[I+1]
    payoffOption[payoffOption>0] = 0
    payoffFuturesTotal = apply(payoffFutures, 2, sum)
    payoffOptionTotal = sum(payoffOption)
    payoffTotal = payoffFuturesTotal+payoffOptionTotal
    volatilityAverage = apply(volatilityFutures, 2, mean)
    
    #########################################################
    # 
    #     
    #     
    #     
    #     
    #     
    #     
    #     
    #     
    #     
    #########################################################
    
    
    # 15:00再对冲一次 --------------------------------------------------------------
    
    payoffFutures2 = matrix(nrow = W, ncol = L-1)
    volatilityFutures2 = matrix(nrow = W, ncol = L-1)
    #payoffOption = c()
    for (i in 1:W) {
      deltaOption = matrix(ncol = L-1, nrow = 20+1+19)
      #赋值：第一行开盘delta（所有时点全一样）（去除固定15:00对冲），2、4、6……40固定时点，3、5、7……39记15:00; （第41行记头寸总和取负值）
      deltaOptionClose_matirx = deltaDayTimeList[[i]][, L] %>% rep(L-1) %>% matrix(nrow = 20, ncol = L-1, byrow = F)        #复制15:00数据，整行是同一个值
      deltaOption[c(1, seq(2, 40, 2), seq(3, 39, 2)), ] = rbind(deltaOptionOpen[i], deltaDayTimeList[[i]][, 1:(L-1)], deltaOptionClose_matirx[-20, ])
      positionFutures = diff(deltaOption) #delta变化的差额是加仓的头寸
      positionFutures = rbind(deltaOptionOpen[i], positionFutures) #期权开仓先对冲一笔
      positionFutures = rbind(positionFutures, -apply(positionFutures, 2, sum)) #最后平仓
      
      priceOptionClose_matirx = priceDayTimeList[[i]][, L] %>% rep(L-1) %>% matrix(nrow = 20, ncol = L-1, byrow = F)        #复制15:00数据，整行是同一个值
      priceFutures = matrix(ncol = L-1, nrow = 20+2+19)
      priceFutures[c(1, seq(2, 40, 2), seq(3, 41, 2)), ] = rbind(priceOptionOpen[i], priceDayTimeList[[i]][, 1:(L-1)], priceOptionClose_matirx)
      priceFutures[positionFutures > 0] = priceFutures[positionFutures > 0] + tick[I+1]  #滑点
      priceFutures[positionFutures < 0] = priceFutures[positionFutures < 0] - tick[I+1]
      
      if (I %in% c(0,1,2,8,18,21,23,26,27,29,31,33,35)) {  #计算手续费两种计费方式
        payoffFutures2[i,] = (-apply(positionFutures*priceFutures, 2, sum) - 0.0001*fee_prop[I+1]*apply(abs(positionFutures)*priceFutures, 2, sum))*(10000000/priceFutures[2,1])    #手续费单位：总金额的万分之几   #加上名义本金
      }else {
        payoffFutures2[i,] = -apply(positionFutures*priceFutures, 2, sum)*(10000000/priceFutures[2,1]) - fee_abs[I+1]*apply(abs(positionFutures), 2, sum)*(10000000/(priceFutures[2,1]*multiplie[I+1]))   #手续费单位：元/每手  #加上名义本金
      }
      
      #计算期货交易价格的波动率，没交易的价格需要去除
      for (j in 1:(L-1)) {
        #pinOnVar = which(positionFutures[,j]!=0)
        if (j==1) {
          volatilityFutures2[i, j] = (var(diff(log(priceFutures[c(1, 3:41),j])))*243)^0.5         #只算20天的波动率
        }else {
          volatilityFutures2[i, j] = (var(diff(log(priceFutures[2:41,j])))*243)^0.5         #只算20天的波动率
        }
      }
      #payoffOption[i] = (priceDayTimeList[[i]][1,1]-priceDayTimeList[[i]][20,L])*(10000000/priceDayTimeList[[i]][1,1])
      #priceOption[i] = priceOption[i] *(10000000/priceDayTimeList[[i]][1,1])                        ###################期权价格
    }    
    
    
    #日期、时间序列名称，命名
    dimnames(payoffFutures2) = list(pointCalendar, paste0(pointTiming[-L], "+"))
    dimnames(volatilityFutures2) = list(pointCalendar, paste0(pointTiming[-L], "+"))
    
    payoffFutures2Total = apply(payoffFutures2, 2, sum)
    volatility2Average = c(apply(volatilityFutures2, 2, mean, na.rm = T), volatilityAverage[L])
    payoff2Total = c(payoffFutures2Total+payoffOptionTotal, payoffTotal[L])
    payoffFutures2 = cbind(payoffFutures2, payoffFutures[, L]); colnames(payoffFutures2)[L] = "15:00"
    
    # pdf(file = "delta_hedge_I.pdf", width = 7*16/9, height = 7)
    # plot(payoffTotal[c((L+1):(2*L-1), L)], pch = 19, xlab = "strategy", ylab = "total payoff", xaxt = "n", type = "b")
    # axis(1, at = 1:length(payoffTotal), labels = names(payoffTotal), cex.axis = 0.9)
    # points(payoffTotal[1:L], type = "b", lty = 2)
    # legend("bottomleft", inset=0, c("15:00 hedging","original"),
    #        lty=c(1,2), pch = c(19, 21), cex=1, bty = "n")
    # dev.off()
    
    # writePayoff = payoffTotal[1:L] %>% cbind(payoffTotal[c((L+1):(2*L-1), L)]); writePayoff = cbind(writePayoff, apply(writePayoff, 1, diff)); colnames(writePayoff) = c("original data", "15:00 hedging", "difference"); 
    # write.csv(writePayoff, "delta_hedge_铁矿石对冲.csv")
    
    
    # 波动率区间 -------------------------------------------------------------------
    
    #t2 = Sys.time()
    contract = list()
    for (i in 1:N) {
      contract[[i]] = cbind(ymd_hms(typeForThisOne[, (N+1-i)*5-4]), typeForThisOne[, ((N+1-i)*5-3):((N+1-i)*5-2)]) %>% na.omit
      filtTime = contract[[i]][,1]
      if (I==3 | I==8 | I==28) {   #无夜盘
        deleteTime = hour(filtTime) %in% c(0,1,2,21,22,23)
        filtTime = filtTime[!deleteTime]
        contract[[i]] = contract[[i]][!deleteTime, ]
      }else if (I %in% c(6, 32:36)) {        #一点收盘
        deleteTime = hour(filtTime)==2 | (hour(filtTime)==1 & minute(filtTime)!=0)
        filtTime = filtTime[!deleteTime]
        contract[[i]] = contract[[i]][!deleteTime, ]
      }else if (I %in% c(29, 30)){    #2:30收盘
        contract[[i]] = contract[[i]]
      }else {    #23:30收盘
        deleteTime = hour(filtTime) %in% c(0,1,2) | (hour(filtTime)==23 & minute(filtTime)>30)
        filtTime = filtTime[!deleteTime]
        contract[[i]] = contract[[i]][!deleteTime, ]
      }
      contract[[i]] = contract[[i]] %>% na.omit()
    }
    
    #列出期权交易日期在contract变量中的索引
    pin = list()     #选出交易期权的日期的索引
    for (i in 1:N){
      if (I %in% c(3, 8, 28)) {
        pin[[i]] = contract[[i]][,1] > dateDivide[i] & contract[[i]][,1] <= dateDivide[i+1] & minute(contract[[i]][,1])==0 & hour(contract[[i]][,1])==9
      }else {
        pin[[i]] = contract[[i]][,1] > dateDivide[i] & contract[[i]][,1] <= dateDivide[i+1] & minute(contract[[i]][,1])==3 & hour(contract[[i]][,1])==21
      }
      pin[[i]] = which(pin[[i]])
    }
    #contract[[4]][pin[[4]],] %>% View()
    
    
    #-------------------------------------------------------------
    ############计算sigma, delta...
    #计算时间占每日的比例
    intervalTime = difftime(contract[[1]][pin[[1]][2]-1, 1], contract[[1]][(pin[[1]][1]):(pin[[1]][2]-1), 1], units = "days") %>% as.numeric()
    L2 = length(intervalTime)
    intervalTime = rep(0, L2)+10^-9
    
    deltaDayTimeList = list()
    priceDayTimeList = list()
    sig_list = seq(0.6, 2, 0.2); l_sig = length(sig_list)
    n = 1; skip_count=0
    m_list = matrix(ncol = l_sig*2, nrow = W)
    for (i in 1:N) {
      # i=1;j=1;k=1;s=1;n=1
      for (j in 1:(length(pin[[i]])+skip_count)) {  #j:不同日期开仓期权
        if (j==1) {
          skip_run = skip_count   #新合约开始，先将skip值赋给新的一个计数器，用于在索引减去上个合约没算的的天数
          skip_fixed = skip_count   #如果上个合约没有少记，skip全为0，否则此合约内skip_fixed固定为上合约少记天数
        }
        
        if (skip_run!=0){
          pinForThisOne = pin[[i]][1]-L2*skip_run
          skip_run = skip_run-1           #当该合约记录完上个合约少记的天数后就重置skip_run变量为零
        }else {
          pinForThisOne = pin[[i]][j-skip_fixed]  #e.g.少记4天，j=5时首次执行该行，skip_fixed=4
        }
        
        if (is.na(contract[[i]][pinForThisOne+L2*20-1, 1])) {
          skip_count = skip_count+1     #统计有几天要移到后一个合约
          next()
        }
        
        skip_count = 0   #新合约开始后(j=1)，skip_count归零，之后若有NA，skip_count增加，但next已经跳过该循环
        
        # #先算option开仓的一个delta
        # sigOptionOpen[n] = (var(diff(log(contract[[i]][(((rep(pinForThisOne,20)-1)-(19:0)*L2)), 3])))*243)^0.5
        # priceOptionOpen[n] = contract[[i]][pinForThisOne, 2]
        # deltaOptionOpen[n] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceOptionOpen[n], X = priceOptionOpen[n], Time = (19+intervalTime[1])/243, r = 0.03, b = 0, sigOptionOpen[n])
        # 
        # 全部在原有基础上长度乘以二倍，用来记录15:00对冲的回测数据
        deltaDayTimeList[[n]] = matrix(ncol = l_sig*2, nrow = 1000)    #声明变量，三列分别用来记录收益率突破时的价格、平仓价、突破时的期权delta， 突破一次记录一行  
        deltaDayTimeList[[n]][1, 1:(l_sig*2)] = deltaOptionOpen[n]  #将期权交易时的价格、delta记录在第一行
        
        priceDayTimeList[[n]] = matrix(ncol = l_sig*2+1, nrow = 1000)
        priceDayTimeList[[n]][, l_sig*2+1] = contract[[i]][pinForThisOne+(L2*20-1), 3]
        priceDayTimeList[[n]][1, 1:(l_sig*2)] = priceOptionOpen[n]
        
        # rownames(deltaDayTimeList[[n]]) = rownames(deltaDayTimeList[[n]], do.NULL = F)   #行命名
        # rownames(deltaDayTimeList[[n]])[1] = contract[[i]][pinForThisOne, 1] %>% as.character()
        m = rep(2, l_sig*2) 
        
        priceRecorded = rep(priceOptionOpen[n], l_sig*2)    #用于记录上一次调仓时的价格
        for (k in 1:(L2*20-2)) {
          #k%%L2==(L2-1)
          #contract[[i]][pinForThisOne+k, ]
          priceDynamic = contract[[i]][pinForThisOne+k, 3]
          for (s in 1:(l_sig*2)) {
            sigk = sig_list[ifelse(s>l_sig, s-l_sig, s)]             #设定波动率区间参数
            if (log(priceDynamic/priceRecorded[s]) > (sigOptionOpen[n]/(243^0.5))*sigk | log(priceDynamic/priceRecorded[s]) < (-sigOptionOpen[n]/(243^0.5))*sigk) {   #判断收益率突破
              #写入价格、delta
              priceDayTimeList[[n]][m[s], s] = priceDynamic
              intervalForThisOne = intervalTime[k%%L2+1] + (19-k%/%L2)
              deltaDayTimeList[[n]][m[s], s] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceDynamic, X = priceOptionOpen[n], Time = intervalForThisOne/243, r = 0.03, b = 0.03, sigOptionOpen[n])
              priceRecorded[s] = priceDynamic
              # rownames(deltaDayTimeList[[n]])[m] = contract[[i]][pinForThisOne+k, 1] %>% as.character()          ##############################调试验证
              m[s] = m[s]+1
              
              #另一列再写入一次（用来记录15:00对冲的策略）
            }else if (k%%L2==(L2-1) & s>l_sig) {
              priceDayTimeList[[n]][m[s], s] = priceDynamic
              intervalForThisOne = intervalTime[k%%L2+1] + (19-k%/%L2)
              deltaDayTimeList[[n]][m[s], s] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceDynamic, X = priceOptionOpen[n], Time = intervalForThisOne/243, r = 0.03, b = 0.03, sigOptionOpen[n])
              priceRecorded[s] = priceDynamic
              # rownames(deltaDayTimeList[[n]])[m] = contract[[i]][pinForThisOne+k, 1] %>% as.character()          ##############################调试验证
              m[s] = m[s]+1
            }
          }
        }
        names(deltaDayTimeList)[n] = as.character((contract[[i]][pinForThisOne, 1])) 
        m_list[n, ] = m-1
        n = n+1
      }
    }
    m_list_sig = colMeans(m_list)/20; names(m_list_sig) = c(paste0("sig*", sig_list), paste0("sig*", sig_list, "+"))
    
    # adj ---------------------------------------------------------------------
    
    #计算期货调仓时position变化量
    positionFutures = c()
    priceFutures = c()
    payoffFuturesVol = matrix(ncol = l_sig*2, nrow = W)
    volatilityFuturesVol = matrix(ncol = l_sig*2, nrow = W)
    # fee = volatilityFuturesVol
    for (s in 1:(l_sig*2)) {
      for (i in 1:W) {
        positionFutures = diff(deltaDayTimeList[[i]][1:m_list[i, s], s])  #计算每笔期货头寸变动
        positionFutures = c(deltaDayTimeList[[i]][1, s], positionFutures)   #记录第一笔
        positionFutures = c(positionFutures, -sum(positionFutures))   #最后一笔平仓时的头寸
        
        priceFutures = c(priceDayTimeList[[i]][1:m_list[i, s], s], priceDayTimeList[[i]][1, l_sig*2+1])
        priceFutures[positionFutures > 0] = priceFutures[positionFutures > 0] + tick[I+1]  #滑点
        priceFutures[positionFutures < 0] = priceFutures[positionFutures < 0] - tick[I+1]
        
        #fee[i, s] = (- 0.0001*fee_prop[I+1]*sum(abs(positionFutures)*priceFutures))*(10000000/deltaDayTimeList[[i]][1, 1])
        if (I %in% c(0,1,2,8,18,21,23,26,27,29,31,33,35)) {
          payoffFuturesVol[i, s] = (-sum(positionFutures*priceFutures) - 0.0001*fee_prop[I+1]*sum(abs(positionFutures)*priceFutures))*(10000000/priceDayTimeList[[i]][1, 1])      #手续费：万分之几
          #payoffFuturesVol[i, s] = (-sum(positionFutures*priceFutures))*(10000000/deltaDayTimeList[[i]][1, 1]) 
        }else {
          payoffFuturesVol[i, s] = -sum(positionFutures*priceFutures)*(10000000/priceDayTimeList[[i]][1, 1]) - fee_abs[I+1]*sum(abs(positionFutures))*(10000000/(priceDayTimeList[[i]][1, 1]*multiplie[I+1]))     #手续费：元每手数
        }
        
        #计算期货交易价格的波动率，没交易的价格需要去除
        volatilityFuturesVol[i, s] = (var(diff(log(priceFutures)))*243)^0.5
      }
    }
    
    dimnames(payoffFuturesVol) = list(pointCalendar, c(paste0("sig*", sig_list), paste0("sig*", sig_list, "+")))
    dimnames(volatilityFuturesVol) = list(pointCalendar, c(paste0("sig*", sig_list), paste0("sig*", sig_list, "+")))
    
    #将15:00对冲的和原数据拆分成两组分别输出
    payoffFuturesVolTotal = apply(payoffFuturesVol, 2, sum)
    payoffTotal = c(payoffTotal, payoffFuturesVolTotal[1:l_sig]+payoffOptionTotal)
    volatilityVolAverage = apply(volatilityFuturesVol, 2, mean, na.rm = T)
    volatilityAverage = c(volatilityAverage, volatilityVolAverage[1:l_sig])
    
    payoff2Total = c(payoff2Total, payoffFuturesVolTotal[(l_sig+1):(l_sig*2)]+payoffOptionTotal)
    volatility2Average = c(volatility2Average, volatilityVolAverage[(l_sig+1):(l_sig*2)])
    
    payoffFuturesVol2 = payoffFuturesVol[, (l_sig+1):(l_sig*2)]
    payoffFuturesVol = payoffFuturesVol[, 1:l_sig]
    #########################################################
    # 
    #     
    #     
    #     
    #     
    #     
    #     
    #     
    #     
    #     
    #########################################################
    
    # 避险带 ---------------------------------------------------------------------
    
    # t3 = Sys.time()
    # contract = list()
    # for (i in 1:N) {
    #   contract[[i]] = cbind(ymd_hms(typeForThisOne[, (N+1-i)*5-4]), typeForThisOne[, ((N+1-i)*5-3):((N+1-i)*5-2)]) %>% na.omit
    #   filtTime = contract[[i]][,1]
    #   if (I==3 | I==8 | I==28) {   #无夜盘
    #     deleteTime = hour(filtTime) %in% c(0,1,2,21,22,23)
    #     filtTime = filtTime[!deleteTime]
    #     contract[[i]] = contract[[i]][!deleteTime, ]
    #   }else if (I %in% c(6, 32:36)) {        #一点收盘
    #     deleteTime = hour(filtTime)==2 | (hour(filtTime)==1 & minute(filtTime)!=0)
    #     filtTime = filtTime[!deleteTime]
    #     contract[[i]] = contract[[i]][!deleteTime, ]
    #   }else if (I %in% c(29, 30)){    #2:30收盘
    #     contract[[i]] = contract[[i]]
    #   }else {    #23:30收盘
    #     deleteTime = hour(filtTime) %in% c(0,1,2) | (hour(filtTime)==23 & minute(filtTime)>30)
    #     filtTime = filtTime[!deleteTime]
    #     contract[[i]] = contract[[i]][!deleteTime, ]
    #   }
    #   contract[[i]] = contract[[i]] %>% na.omit()
    # }
    # 
    # #列出期权交易日期在contract变量中的索引
    # pin = list()     #选出交易期权的日期的索引
    # for (i in 1:N){
    #   if (I %in% c(3, 8, 28)) {
    #     pin[[i]] = contract[[i]][,1] > dateDivide[i] & contract[[i]][,1] <= dateDivide[i+1] & minute(contract[[i]][,1])==0 & hour(contract[[i]][,1])==9
    #   }else {
    #     pin[[i]] = contract[[i]][,1] > dateDivide[i] & contract[[i]][,1] <= dateDivide[i+1] & minute(contract[[i]][,1])==3 & hour(contract[[i]][,1])==21
    #   }
    #   pin[[i]] = which(pin[[i]])
    # }
    # #contract[[4]][pin[[4]],] %>% View()
    # 
    # ############计算sigma, delta...
    # #计算时间占每日的比例
    # intervalTime = difftime(contract[[1]][pin[[1]][2]-1, 1], contract[[1]][(pin[[1]][1]):(pin[[1]][2]-1), 1], units = "days") %>% as.numeric()
    # L2 = length(intervalTime)
    # intervalTime = rep(0, L2)+10^-9
    
    
    ra_list = c(0.0004, 0.002, 0.004, 0.02, 0.04, 0.1); l_ra = length(ra_list)
    priceDayTimeList = list()
    deltaDayTimeList = list()
    deltaDynamic_up = rep(0, l_ra*2); deltaDynamic_down = rep(0, l_ra*2); fee_unit = rep(0, l_ra*2)
    m_list = matrix(ncol = l_ra*2, nrow = W)
    n = 1; skip_count=0
    for (i in 1:N) {
      #i=1;j=1;s=1;k=1;n=1
      for (j in 1:(length(pin[[i]])+skip_count)) {  #j:不同日期开仓期权
        if (j==1) {
          skip_run = skip_count   #新合约开始，先将skip值赋给新的一个计数器，用于在索引减去上个合约没算的的天数
          skip_fixed = skip_count   #如果上个合约没有少记，skip全为0，否则此合约内skip_fixed固定为上合约少记天数
        }
        
        if (skip_run!=0){
          pinForThisOne = pin[[i]][1]-L2*skip_run
          skip_run = skip_run-1           #当该合约记录完上个合约少记的天数后就重置skip_run变量为零
        }else {
          pinForThisOne = pin[[i]][j-skip_fixed]  #e.g.少记4天，j=5时首次执行该行，skip_fixed=4
        }
        
        if (is.na(contract[[i]][pinForThisOne+L2*20-1, 1])) {
          skip_count = skip_count+1     #统计有几天要移到后一个合约
          next()
        }
        
        skip_count = 0   #新合约开始后(j=1)，skip_count归零，之后若有NA，skip_count增加，但next已经跳过该循环
        
        
        priceDayTimeList[[n]] = matrix(ncol = l_ra*2+1, nrow = 2000)    #声明变量，前lra列用来记录各参数下收益率突破时的价格、最后一列记录平仓价， 突破一次记录一行  
        priceDayTimeList[[n]][, l_ra*2+1] = contract[[i]][pinForThisOne+(L2*20-1), 3]
        priceDayTimeList[[n]][1, 1:(l_ra*2)] = priceOptionOpen[n]         #将期权交易时的价格记录在第一行
        
        
        deltaDayTimeList[[n]] = matrix(ncol = l_ra*2, nrow = 2000)   #记录突破时的期权delta
        deltaDayTimeList[[n]][1, 1:(l_ra*2)] = deltaOptionOpen[n]         #将期权交易时的delta记录在第一行
        # rownames(deltaDayTimeList[[n]]) = rownames(deltaDayTimeList[[n]], do.NULL = F)   #行命名
        # rownames(deltaDayTimeList[[n]])[1] = contract[[i]][pinForThisOne, 1] %>% as.character()
        m = rep(2, l_ra*2) 
        
        deltaRecorded = rep(deltaOptionOpen[n], l_ra*2)
        #gammaOptionOpen = GBSGreeks(Selection = "Gamma", TypeFlag = "c", S = priceOptionOpen[n], X = priceOptionOpen[n], Time = (19+fractionTime[1])/243, r = 0.03, b = 0.03, sigOptionOpen[n])
        
        for (k in 1:(L2*20-2)) {             #每笔开仓之后的各时间点
          #contract[[i]][pinForThisOne+k, ]
          priceDynamic = contract[[i]][pinForThisOne+k, 3]
          intervalForThisOne = intervalTime[k%%L2+1] + (19-k%/%L2)
          deltaDynamic = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceDynamic, X = priceOptionOpen[n], Time = intervalForThisOne/243, r = 0.03, b = 0.03, sigOptionOpen[n])
          gammaDynamic = GBSGreeks(Selection = "Gamma", TypeFlag = "c", S = priceDynamic, X = priceOptionOpen[n], Time = intervalForThisOne/243, r = 0.03, b = 0.03, sigOptionOpen[n])
          fee_unit = 0.001*priceDynamic                     #用于计算边界
          
          for (s in 1:(l_ra*2)) {     #设定risk averison参数
            ra = ra_list[ifelse(s>l_ra, s-l_ra, s)]   #设定风险厌恶参数
            #fee_unit[s] = ifelse(is.na(fee_abs[I+1]), yes = fee_prop[I+1]*0.0001*priceOptionOpen[n], no = fee_abs[I+1])
            # if (k==1){
            #   deltaRecorded_up[s] = deltaOptionOpen[n] + (3/2*exp(-0.03*19)*fee_unit[s]*gammaOptionOpen^2/ra)^(1/3)
            #   deltaRecorded_low[s] = deltaOptionOpen[n] - (3/2*exp(-0.03*19)*fee_unit[s]*gammaOptionOpen^2/ra)^(1/3)
            # }
            deltaDynamic_up[s] = deltaDynamic + (3/2*exp(-0.03*intervalForThisOne/243)*fee_unit*gammaDynamic^2/ra)^(1/3)
            deltaDynamic_down[s] = deltaDynamic - (3/2*exp(-0.03*intervalForThisOne/243)*fee_unit*gammaDynamic^2/ra)^(1/3)
            
            if (deltaRecorded[s] < deltaDynamic_down[s]) {   #判断收益率突破
              #写入价格、delta
              priceDayTimeList[[n]][m[s], s] = priceDynamic
              deltaDayTimeList[[n]][m[s], s] = deltaDynamic_down[s]
              deltaRecorded[s] = deltaDynamic_down[s]
              m[s] = m[s]+1
              
            }else if (deltaRecorded[s] > deltaDynamic_up[s]) {
              priceDayTimeList[[n]][m[s], s] = priceDynamic
              deltaDayTimeList[[n]][m[s], s] = deltaDynamic_up[s]
              deltaRecorded[s] = deltaDynamic_up[s]
              m[s] = m[s]+1
              
            }else if (k%%L2==(L2-1) & s>l_ra) {
              priceDayTimeList[[n]][m[s], s] = priceDynamic
              deltaDayTimeList[[n]][m[s], s] = deltaDynamic
              deltaRecorded[s] = deltaDynamic
              m[s] = m[s]+1
            }
          }
        }
        #raDayTimeList[[n]] = na.omit(raDayTimeList[[n]])
        names(priceDayTimeList)[n] = as.character((contract[[i]][pinForThisOne, 1])) 
        m_list[n, ] = m-1
        n = n+1
      }
    }
    m_list_ra = colMeans(m_list)/20; names(m_list_ra) = c(paste0("ra*", ra_list), paste0("ra*", ra_list, "+"))
    # adj ---------------------------------------------------------------------
    
    #计算期货调仓时position变化量
    positionFutures = c()
    priceFutures = c()
    payoffFuturesRA = matrix(ncol = l_ra*2, nrow = W)
    volatilityFuturesRA = matrix(ncol = l_ra*2, nrow = W)
    # fee = volatilityFuturesVol
    for (s in 1:(l_ra*2)) {
      for (i in 1:W) {
        positionFutures = diff(deltaDayTimeList[[i]][1:m_list[i, s], s])  #计算每笔期货头寸变动
        positionFutures = c(deltaDayTimeList[[i]][1, s], positionFutures)   #记录第一笔
        positionFutures = c(positionFutures, -sum(positionFutures))   #最后一笔平仓时的头寸
        
        priceFutures = c(priceDayTimeList[[i]][1:m_list[i, s], s], c(priceDayTimeList[[i]][1, l_ra*2+1]))
        priceFutures[positionFutures > 0] = priceFutures[positionFutures > 0] + tick[I+1]  #滑点
        priceFutures[positionFutures < 0] = priceFutures[positionFutures < 0] - tick[I+1]
        
        #fee[i, s] = (- 0.0001*fee_prop[I+1]*sum(abs(positionFutures)*priceFutures))*(10000000/raDayTimeList[[i]][1, 1])
        if (I %in% c(0,1,2,8,18,21,23,26,27,29,31,33,35)) {
          payoffFuturesRA[i, s] = (-sum(positionFutures*priceFutures) - 0.0001*fee_prop[I+1]*sum(abs(positionFutures)*priceFutures))*(10000000/priceDayTimeList[[i]][1, 1])      #手续费：万分之几
          #payoffFuturesRA[i, s] = (-sum(positionFutures*priceFutures))*(10000000/raDayTimeList[[i]][1, 1]) 
        }else {
          payoffFuturesRA[i, s] = -sum(positionFutures*priceFutures)*(10000000/priceDayTimeList[[i]][1, 1]) - fee_abs[I+1]*sum(abs(positionFutures))*(10000000/(priceDayTimeList[[i]][1, 1]*multiplie[I+1]))     #手续费：元每手数
        }
        
        #计算期货交易价格的波动率，没交易的价格需要去除
        volatilityFuturesRA[i, s] = (var(diff(log(priceFutures)))*243)^0.5
      }
    }
    
    dimnames(payoffFuturesRA) = list(pointCalendar, c(paste0("ra*", ra_list), paste0("ra*", ra_list, "+")))
    dimnames(volatilityFuturesRA) = list(pointCalendar, c(paste0("ra*", ra_list), paste0("ra*", ra_list, "+")))
    
    payoffFuturesRATotal = apply(payoffFuturesRA, 2, sum)
    payoffTotal = c(payoffTotal, payoffFuturesRATotal[1:l_ra]+payoffOptionTotal)
    volatilityRAAverage = apply(volatilityFuturesRA, 2, mean, na.rm = T)
    volatilityAverage = c(volatilityAverage, volatilityRAAverage[1:l_ra])
    
    payoff2Total = c(payoff2Total, payoffFuturesRATotal[(l_ra+1):(l_ra*2)]+payoffOptionTotal)
    volatility2Average = c(volatility2Average, volatilityRAAverage[(l_ra+1):(l_ra*2)])
    
    payoffFuturesRA2 = payoffFuturesRA[, (l_ra+1):(l_ra*2)]
    payoffFuturesRA = payoffFuturesRA[, 1:l_ra]
    
    
    
    # 总输出 ---------------------------------------------------------------------
    #输出开盘价和开盘头寸数，用来计算保证金
    marginComputing = cbind(priceOptionOpen, deltaOptionOpen)
    dimnames(marginComputing) = list(pointCalendar, c("price Open", "position Open"))
    
    #输出：
    # 1：每种策略的总payoff和平均volatility；（不含15:00对冲）
    # 2：每种策略每一天的期货payoff、期权payoff；（不含15:00对冲）
    # 3：每种策略的总payoff和平均volatility；（15:00对冲）
    # 4：每种策略每一天的期货payoff、期权payoff；（15:00对冲）
    # 5：用于计算保证金的开盘价和开盘头寸数（每单位期货价格）
    # 6：各种策略的数量（累加）
    # 7：后两种策略的平均每日交易频数
    output = list(cbind(payoffTotal, volatilityAverage), cbind(payoffFutures, payoffFuturesVol, payoffFuturesRA, payoffOption), 
                  cbind(payoff2Total, volatility2Average), cbind(payoffFutures2, payoffFuturesVol2, payoffFuturesRA2, payoffOption), 
                  marginComputing, cumsum(c(L, length(sig_list), length(ra_list))), c(m_list_sig, m_list_ra))
    
    # plot(output[[1]][,1], xlab = "", ylab = "total payoff", xaxt = "n", type = "b")
    # axis(1, padj = -0.2, tck = -0.03, at = seq(1, length(output[[1]][,1]), 2), labels = names(output[[1]][,1])[seq(1, length(output[[1]][,1]), 2)], cex.axis = 0.9);     axis(1, at = 1:length(output[[1]][,1]), labels = rep("", output[[4]][3]))
    # axis(1, padj = 1.5, at = seq(2, length(output[[1]][,1]), 2), labels = names(output[[1]][,1])[seq(2, length(output[[1]][,1]), 2)], cex.axis = 0.9)
    # points(output[[3]][,1], pch = 19, lty = 2, type = "b")
    
    # t4 = Sys.time(); t2-t1; t3-t2; t4-t3
    # 结束 ----------------------------------------------------------------------
    
  }, silent = F)
  return(output)
}    

stopCluster(cl)
Sys.time()-t1

#############################################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#############################################
#输出结果
outputDataT = lapply(outputData, "[[", 2)
outputData2 = lapply(outputData, "[[", 3)
outputDataT2 = lapply(outputData, "[[", 4)
outputData_margin = lapply(outputData, "[[", 5)
outputData_num = lapply(outputData, "[[", 6)
outputData_fequ = lapply(outputData, "[[", 7)
outputData = lapply(outputData, "[[", 1)
#write.csv(t(read.csv("Book1.csv")), "Book2.csv")
ID = c("I","J","JM","SF","SM","ZC","SN","SR","JD","CF","C","CS","OI","Y","P","A","M","RM","RU","L","TA","PP","V","FU","FG","MA","RB","HC","AP","AG","AU","BU","AL","CU","NI","PB","ZN")
x = length(outputData)

#数据排序
mxall = matrix(ncol = x*4, nrow = max(sapply(outputData, nrow))) %>% as.data.frame()
for (i in 1:x) {
  #mx = outputData[[i]]
  mx = outputData2[[i]]
  mx = mx[order(mx[,1], decreasing = T), ]
  colnames(mx) = paste0(colnames(mx), "_", ID[i])
  mxall[1:nrow(mx), (4*i-3)] = rownames(mx)
  mxall[1:nrow(mx), (4*i-2)] = mx[,1]
  mxall[1:nrow(mx), (4*i-1)] = mx[,2]
  mxall[1:nrow(mx), (4*i)] = ""
  colnames(mxall)[(4*i-3):(4*i-1)] = c(ID[i], colnames(mx))
  #write.csv(mx, file = paste0("hedge/delta_hedge_", ID[i], ".csv"))
}
write.csv(mxall, file = paste0("hedge/delta_hedge_", "all_types2.csv"))


#输出每日payoff
for (i in  1:37) {
  payoffWrite = outputDataT2[[i]][, -outputData_num[[i]][3]-1] + outputDataT2[[i]][, outputData_num[[i]][3]+1]
  write.csv(payoffWrite, paste0("hedge/payoff/type", as.character(i-1), "-", ID[i], "+.csv"))
}
for (i in  1:37) {
  payoffWrite = outputDataT[[i]][, -outputData_num[[i]][3]-1] + outputDataT[[i]][, outputData_num[[i]][3]+1]
  write.csv(payoffWrite, paste0("hedge/payoff/type", as.character(i-1), "-", ID[i], ".csv"))
}

#作图
pdf(file = "delta_hedge_types2.pdf")
for (i in 1:x) {
  #jpeg(file = paste0("delta_hedge_type", as.character(i-1), "_", ID[i], ".jpeg"), width=480*4, height=480*4, units = "px", res = 72*4, pointsize = 12)
  k = outputData_num[[i]][1]
  p1 = outputData[[i]][1:k,1]
  p2 = outputData[[i]][1:k,2]/outputData[[i]][k,2]
  plot(p2, p1, xlab = "scaled volatility", ylab = "total payoff", main = paste0(as.character(i-1), "-", ID[i]))
  text(p2, p1+0.001*mean(p1), labels = rownames(outputData[[i]])[1:k], cex = 0.8)
  abline(h = mean(p1), lty = 3)
  abline(v = mean(p2), lty = 3)
  lmSP = lm(p1[1:(k)]~p2[1:(k)])
  #lmSP = lm(p1[1:10]~p2[1:10])
  abline(lmSP)
  #text(min(p2), min(p1), paste0("R-square = ",format(summary(lmSP)$adj.r.squared, digits = 4)))
  #dev.off()
}
dev.off()


pdf(file = "delta_hedge_types_payoff2.pdf", width = 7*16/9, height = 7)
for (i in 1:x) {
  plot(outputData[[i]][,1], ylim = c(min(c(outputData[[i]][,1], outputData2[[i]][,1])), max(c(outputData[[i]][,1], outputData2[[i]][,1]))), xlab = "", ylab = "total payoff",  main = paste0(as.character(i-1), "-", ID[i]), xaxt = "n", type = "b")
  # axis(1, padj = -0.2, tck = -0.03, at = seq(1, length(outputData[[i]][,1]), 2), labels = names(outputData[[i]][,1])[seq(1, length(outputData[[i]][,1]), 2)], cex.axis = 0.9);  #axis(1, at = 1:length(outputData[[i]][,1]), labels = rep("", outputData[[4]][3]))
  # axis(1, padj = 1.5, at = seq(2, length(outputData[[i]][,1]), 2), labels = names(outputData[[i]][,1])[seq(2, length(outputData[[i]][,1]), 2)], cex.axis = 0.9)
  points(outputData2[[i]][,1], pch = 19, lty = 2, type = "b")
  text(1:outputData_num[[i]][3], outputData2[[i]][,1]*1.0025, labels = names(outputData[[i]][,1]), cex = 0.85)
  legend("bottomleft", c("15:00 hedging","original"),
                lty=c(2,1), pch = c(19, 21), cex=1, bty = "n")
  abline(v = outputData_num[[i]][1]+0.5, lty = 2, col = "grey")
  abline(v = outputData_num[[i]][2]+0.5, lty = 2, col = "grey")
  abline(h = outputData[[i]][outputData_num[[i]][1], 1], lty = 2, col = "grey")
}
dev.off()



#最好的5个和最低的5个作累积收益图
#统一作图、输出
for (i in 1:37) {
  n = order(outputData2[[i]][,1], decreasing = T)[c(1:5, (outputData_num[[i]][3]-4):outputData_num[[i]][3])]
  payoffPlot = outputDataT2[[i]][,n]-outputDataT2[[i]][,outputData_num[[i]][1]]
  payoffPlot = xts(payoffPlot, order.by = ymd(rownames(payoffPlot)))
  x = dygraph(cumsum(payoffPlot), main = paste0(as.character(i-1), "-", ID[i]))%>% 
    dyLegend(width = 320) %>% 
    dyOptions(titleHeight = 25) %>%
    dyOptions(colors = c(RColorBrewer::brewer.pal(5, "Set1")[1:5], RColorBrewer::brewer.pal(7, "YlGnBu")[3:7]))
  name_type = paste0("type", as.character(i-1), "_", ID[i])
  assign(name_type, x)
  saveWidget(get(name_type), "temp.html", selfcontained = FALSE)
  width<- 1080
  height <- 610
  webshot("temp.html", file = paste0("E:/R/hedge/plot/type",as.character(i-1), "-", ID[i], ".png"),
          cliprect = c(10,30,width+50,height+50)
          ,vwidth = width, vheight = height )
  rm(list = name_type)
}

for (i in 1:37) {
  n = order(outputData[[i]][,1], decreasing = T)[c(1:5, (outputData_num[[i]][3]-4):outputData_num[[i]][3])]
  payoffPlot = outputDataT[[i]][,n]-outputDataT[[i]][,outputData_num[[i]][1]]
  payoffPlot = xts(payoffPlot, order.by = ymd(rownames(payoffPlot)))
  x = dygraph(cumsum(payoffPlot), main = paste0(as.character(i-1), "-", ID[i]))%>% 
    dyLegend(width = 320) %>% 
    dyOptions(titleHeight = 25) %>%
    dyOptions(colors = c(RColorBrewer::brewer.pal(5, "Set1")[1:5], RColorBrewer::brewer.pal(7, "YlGnBu")[3:7]))
  name_type = paste0("type", as.character(i-1), "_", ID[i])
  assign(name_type, x)
  saveWidget(get(name_type), "temp.html", selfcontained = FALSE)
  width<- 1080
  height <- 610
  webshot("temp.html", file = paste0("E:/R/hedge/plot/type",as.character(i-1), "-", ID[i], ".png"),
          cliprect = c(10,30,width+50,height+50)
          ,vwidth = width, vheight = height )
  rm(list = name_type)
}
######################
######################
#各策略逐日payoff是否去除极端值的函数, 否 则默认返回mean，或者返回原数据；若要去除极端值则判断return.v参数，返回mean、留存NA、是则去除NA
WhetherCleanExtremity = function(type_index, answer = F, return.v = c("mean", "raw", "boiled", "sum")){
  if (!answer){
    if ("mean" %in% return.v) {
      return(outputData[[type_index]][,1]/nrow(outputDataT[[type_index]]))    #返回均值 
    }else if("raw" %in% return.v){
      return(outputDataT[[type_index]])
    }else {
      return(outputData[[type_index]][,1]) 
    }
  }else {
    dataDaily = outputDataT[[type_index]]
    l = ncol(dataDaily); w = nrow(dataDaily)
    dataDaily = (dataDaily[, 1:(l-1)]+dataDaily[, l]); l = l-1
    qt = apply(dataDaily, 2, quantile, c(0.05, 0.95))    #去除极端大/小5%的数据
    dataDaily[sapply(1:l, function(x){dataDaily[,x]<qt[1,x]|dataDaily[,x]>qt[2,x]})] = NA
    if ("mean" %in% return.v){
      return(colMeans(dataDaily, na.rm = T))
    }else if("raw" %in% return.v){
      return(dataDaily)
    }else if("boiled" %in% return.v){
      return(na.omit(dataDaily))
    }else {
      return(na.omit(dataDaily) %>% colSums)
    }
  }
}
# WhetherCleanExtremity(type_index = 1, answer = F, return.v = "sum")   #answer = F是原数据，T是去除极端值的数据

#输出最大payoff的六个点，再跨品种比较
payoffAbstracted = matrix(ncol = x*3, nrow = 6) %>% as.data.frame()
colnames(payoffAbstracted)[seq(1, x*3, 3)] = ID     #命名
colnames(payoffAbstracted)[seq(2, x*3, 3)] = paste0("payoff_", ID)
colnames(payoffAbstracted)[seq(3, x*3, 3)] = ""
payoffTop3 = matrix(ncol = 3, nrow = x*3*2); rownames(payoffTop3) = as.character(1:(x*6))
payoffTop1 = matrix(ncol = 3, nrow = x*2); rownames(payoffTop1) = rep("", x*2)
colnames(payoffTop3) = c("payoff", "type", "strategy"); colnames(payoffTop1) = colnames(payoffTop3)
for (i in 1:x) {
  pf = outputData[[i]][,1]   #取出每种商品各策略收益数据
  #pf = WhetherCleanExtremity(type_index = i, answer = T, return.v = "mean")    #取出每种商品各策略收益数据
  l = length(pf)
  pf = -pf/pf[l-3]
  #pf = pf[1:(l-3)]; l = l-3    #是否去除sig的三个策略
  pf_index = order(pf, decreasing = T)[c(1:3, (l-2):l)]
  pf = pf[pf_index]
  payoffAbstracted[, 3*i-2] = names(pf)
  payoffAbstracted[, 3*i-1] = pf
  payoffAbstracted[, 3*i] = ""
  names(pf) = paste0(ID[i], "_", names(pf), c(rep("T", 3), rep("B", 3)))
  payoffTop1[(2*i-1):(2*i), ] = cbind(pf[c(1,6)], i, pf_index[c(1,6)])
  rownames(payoffTop1)[(2*i-1):(2*i)] = names(pf)[c(1,6)]
  payoffTop3[(6*i-5):(6*i), ] = cbind(pf, i, pf_index)
  rownames(payoffTop3)[(6*i-5):(6*i)] = names(pf)
}
payoffTop3_ranked = payoffTop3[order(payoffTop3[, 1], decreasing = T), 1] %>% as.data.frame()
payoffTop1_ranked = payoffTop1[order(payoffTop1[, 1], decreasing = T), 1] %>% as.data.frame()
write.csv(payoffAbstracted, file = "hedge/delta_hedge_types_ranking.csv")
write.csv(payoffTop3_ranked, file = "hedge/delta_hedge_types_ranking_combined.csv")
write.csv(payoffTop1_ranked, file = "hedge/delta_hedge_types_ranking_combined_st.csv")


#选出好的作图
for (i in 1:37) {
  type_i = xts(outputDataT[[payoffTop1[2*i-1, 2]]], order.by = ymd(rownames(outputDataT[[payoffTop1[2*(ifelse(i %in% c(4,9), i-1, i))-1, 2]]]))) #先提取单个品种所有策略数据
  strategy_i = type_i[, payoffTop1[2*i-1, 3]]  #从payoffTop1提取每个产品最优策略的索引，从type_i取出数据
  benchmark_i = type_i[, ncol(type_i)-4]   #以15：00为基准
  if (i==1){
    strategy_top = (strategy_i)-(benchmark_i)  #两种策略payoff的差值越大说明策略越好
  }else {
    strategy_top = merge(strategy_top, (strategy_i)-(benchmark_i))
  }
  colnames(strategy_top)[i] = ID[i]     #包含每种商品的最优策略
}



dateInterval = "2017-02-16/2019-05-14"
strategy_topintop = strategy_top[dateInterval, order(colSums(strategy_top[dateInterval]), decreasing = T)[1:3]] %>% cumsum
strategy_which = cumsum(strategy_top[dateInterval, 7])
#avg = apply(strategy_topintop, 2, cumsum )%>% rowMeans %>% as.xts(., order.by = index(aaa))
strategy_plot = merge(strategy_topintop, strategy_which)
dygraph(strategy_plot)%>%
  dyLegend(width = 300)
#l = ncol(type_i)
# payoffPlot = xts(strategy = )
plot(cumsum(strategy_i)-cumsum(benchmark_i), type = "l", ylab = "payoff difference")
#lines(cumsum(benchmark_i), type = "l", col = "blue")
abline(h = 0, col = "grey", lty = 3)
