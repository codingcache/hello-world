library(magrittr)
library(fOptions)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(dygraphs)
library(doParallel)
library(foreach)
rm(list = ls(all.names = T))
cl=makeCluster(4)
registerDoParallel(cl)
t1 = Sys.time()
#I=2
outputData = foreach(I = 0:36, .packages = c("magrittr", "fOptions", "lubridate", "xts")) %dopar% {
  outputData = NA
  try({
    assign("typeForThisOne", read.csv(paste0("futuresdata/type", as.character(I),".csv"), stringsAsFactors = F))
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
      listingDay = day(contract[[i]][listingTime, 1]) %>% unique() 
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
    
    
    #---------------------------------------------
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
    
    deltaDayTimeListing = list()
    for (s in c(1, 1.5, 2)) {
      deltaDayTimeList = list()
      n = 1; skip_count=0
      for (i in 1:N) {
        
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
          deltaDayTimeList[[n]] = matrix(ncol = 3, nrow = 1000)    #声明变量，三列分别用来记录收益率突破时的价格、平仓价、突破时的期权delta， 突破一次记录一行  
          deltaDayTimeList[[n]][, 2] = contract[[i]][pinForThisOne+(L2*20-1), 3]
          deltaDayTimeList[[n]][1, c(1,3)] = c(priceOptionOpen[n], deltaOptionOpen[n])  #将期权交易时的价格、delta记录在第一行
          # rownames(deltaDayTimeList[[n]]) = rownames(deltaDayTimeList[[n]], do.NULL = F)   #行命名
          # rownames(deltaDayTimeList[[n]])[1] = contract[[i]][pinForThisOne, 1] %>% as.character()
          m = 2 
          priceRecorded = priceOptionOpen[n]    #用于记录上一次调仓时的价格
          for (k in 1:(L2*20-2)) {
            #contract[[i]][pinForThisOne+k, ]
            priceDynamic = contract[[i]][pinForThisOne+k, 3]
            if (log(priceDynamic/priceRecorded) > (sigOptionOpen[n]/(243^0.5))*s | log(priceDynamic/priceRecorded) < (-sigOptionOpen[n]/(243^0.5))*s) {   #判断收益率突破
              #写入价格、delta
              deltaDayTimeList[[n]][m, 1] = priceDynamic
              intervalForThisOne = intervalTime[k%%L2+1] + (19-k%/%L2)
              deltaDayTimeList[[n]][m, 3] = GBSGreeks(Selection = "Delta", TypeFlag = "c", S = priceDynamic, X = priceOptionOpen[n], Time = intervalForThisOne/243, r = 0.03, b = 0.03, sigOptionOpen[n])
              priceRecorded = priceDynamic
              # rownames(deltaDayTimeList[[n]])[m] = contract[[i]][pinForThisOne+k, 1] %>% as.character()          ##############################调试验证
              m = m+1
            }
          }
          deltaDayTimeList[[n]] = na.omit(deltaDayTimeList[[n]])
          names(deltaDayTimeList)[n] = as.character((contract[[i]][pinForThisOne, 1])) 
          n = n+1
        }
      }
      deltaDayTimeListing[[s*2-1]] = deltaDayTimeList
    }
    
    #计算期货调仓时position变化量
    positionFutures = c()
    priceFutures = c()
    payoffFuturesVol = matrix(ncol = 3, nrow = W)
    volatilityFuturesVol = matrix(ncol = 3, nrow = W)
    # fee = volatilityFuturesVol
    for (s in 1:3) {
      deltaDayTimeList = deltaDayTimeListing[[s]]
      for (i in 1:W) {
        positionFutures = diff(deltaDayTimeList[[i]][, 3])  #计算每笔期货头寸变动
        positionFutures = c(deltaDayTimeList[[i]][1, 3], positionFutures)   #记录第一笔
        positionFutures = c(positionFutures, -sum(positionFutures))   #最后一笔平仓时的头寸
        
        priceFutures = c(deltaDayTimeList[[i]][, 1], c(deltaDayTimeList[[i]][1, 2]))
        priceFutures[positionFutures > 0] = priceFutures[positionFutures > 0] + tick[I+1]  #滑点
        priceFutures[positionFutures < 0] = priceFutures[positionFutures < 0] - tick[I+1]
        
        #fee[i, s] = (- 0.0001*fee_prop[I+1]*sum(abs(positionFutures)*priceFutures))*(10000000/deltaDayTimeList[[i]][1, 1])
        if (I %in% c(0,1,2,8,18,21,23,26,27,29,31,33,35)) {
          payoffFuturesVol[i, s] = (-sum(positionFutures*priceFutures) - 0.0001*fee_prop[I+1]*sum(abs(positionFutures)*priceFutures))*(10000000/deltaDayTimeList[[i]][1, 1])      #手续费：万分之几
          #payoffFuturesVol[i, s] = (-sum(positionFutures*priceFutures))*(10000000/deltaDayTimeList[[i]][1, 1]) 
        }else {
          payoffFuturesVol[i, s] = -sum(positionFutures*priceFutures)*(10000000/deltaDayTimeList[[i]][1, 1]) - fee_abs[I+1]*sum(abs(positionFutures))*(10000000/(deltaDayTimeList[[i]][1, 1]*multiplie[I+1]))     #手续费：元每手数
        }
        
        #计算期货交易价格的波动率，没交易的价格需要去除
        volatilityFuturesVol[i, s] = (var(diff(log(priceFutures)))*243)^0.5
      }
    }
    
    dimnames(payoffFuturesVol) = list(pointCalendar, c("sig*1", "sig*1.5", "sig*2"))
    dimnames(volatilityFuturesVol) = list(pointCalendar, c("sig*1", "sig*1.5", "sig*2"))
    
    payoffFuturesVolTotal = apply(payoffFuturesVol, 2, sum)
    payoffTotal = c(payoffTotal, payoffFuturesVolTotal+payoffOptionTotal)
    volatilityVolAverage = apply(volatilityFuturesVol, 2, mean, na.rm = T)
    volatilityAverage = c(volatilityAverage, volatilityVolAverage)
    
    #输出：每种策略的总payoff和平均volatility，以及每一天的期货payoff、期权payoff
    output = list(cbind(payoffTotal, volatilityAverage), cbind(payoffFutures, payoffFuturesVol, payoffOption))
    
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
outputData = lapply(outputData, "[[", 1)
#write.csv(t(read.csv("Book2.csv")), "Book1.csv")
ID = c("I","J","JM","SF","SM","ZC","SN","SR","JD","CF","C","CS","OI","Y","P","A","M","RM","RU","L","TA","PP","V","FU","FG","MA","RB","HC","AP","AG","AU","BU","AL","CU","NI","PB","ZN")
x = length(outputData)
#数据
mxall = matrix(ncol = x*4, nrow = max(sapply(outputData, nrow))) %>% as.data.frame()
for (i in 1:x) {
  mx = outputData[[i]]
  mx = mx[order(mx[,1], decreasing = T), ]
  colnames(mx) = paste0(colnames(mx), "_", ID[i])
  mxall[1:nrow(mx), (4*i-3)] = rownames(mx)
  mxall[1:nrow(mx), (4*i-2)] = mx[,1]
  mxall[1:nrow(mx), (4*i-1)] = mx[,2]
  mxall[1:nrow(mx), (4*i)] = ""
  colnames(mxall)[(4*i-3):(4*i-1)] = c(ID[i], colnames(mx))
  #write.csv(mx, file = paste0("hedge/delta_hedge_", ID[i], ".csv"))
}
write.csv(mxall, file = paste0("hedge/delta_hedge_", "all_types.csv"))


#作图
pdf(file = "delta_hedge_types.pdf")
for (i in 1:x) {
  #jpeg(file = paste0("delta_hedge_type", as.character(i-1), "_", ID[i], ".jpeg"), width=480*4, height=480*4, units = "px", res = 72*4, pointsize = 12)
  k = nrow(outputData[[i]])-3
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


pdf(file = "delta_hedge_types_payoff.pdf", width = 7*16/9, height = 7)
for (i in 1:x) {
  p1 = outputData[[i]][,1]
  plot(p1, xlab = "strategy", ylab = "total payoff", main = paste0(as.character(i-1), "-", ID[i]), xaxt = "n", type = "b")
  axis(1, at = 1:nrow(outputData[[i]]), labels = rownames(outputData[[i]]), cex.axis = 0.9)
}
dev.off()

######################
#是否去除极端值的函数, 否则默认返回mean；若要去除极端值则判断return.v参数，返回mean、留存NA、是则去除NA
WhetherCleanExtremity = function(type_index, answer = F, return.v = c("mean", "raw", "boiled")){
  if (!answer){
    if ("mean" %in% return.v) {
      return(outputData[[type_index]][,1]/nrow(outputDataT[[type_index]]))    #返回均值 
    }else if("raw" %in% return.v){
      return(outputData[[type_index]][[2]])
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
