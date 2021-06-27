require("IBrokers");require("data.table");require("lubridate")
require("quantmod");require("odbc");require("stringr");require("pbapply")
require("tseries"); require("jsonlite")

newData2 = function(symbol,MINUTES)
{
  # get Account details and check connection
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  
  security = twsSTK(symbol=symbol)
  dat <- reqHistoricalData(tws,Contract = security,whatToShow = "TRADES",
                           barSize = paste0(MINUTES," mins"),duration = "10 D",
                           useRTH = '1',tzone = "America/Los_Angeles")
  twsDisconnect(tws)
  dat <- Cl(dat)
  colnames(dat) <- paste0(symbol,".Close")
  # only want this weeks
  dat <- dat[paste0(format(findMonday(daytoday = daytoday), "%Y%m%d"),"::")]
  assign(paste(symbol),dat,envir = .GlobalEnv)
  dat
}


getQuoteTD = function(ticker,TimeStamp)
{
  url = paste0("https://api.tdameritrade.com/v1/marketdata/quotes?apikey=",PASS$TDAPIKEY,"&symbol=",ticker,"")
  tmp = read_json(url, simplifyVector = TRUE)
  tmp = as.data.frame(tmp)
  colnames(tmp) = gsub(paste0(ticker,"."),"",names(tmp))
  CLOSE = xts(round((as.numeric(tmp[,"bidPrice"])+as.numeric(tmp[,"askPrice"]))/2,2),
              order.by = TimeStamp)
  #  CLOSE = xts(round((as.numeric(tmp[,"bidPrice"])+as.numeric(tmp[,"askPrice"]))/2,2),
  #              order.by = as.POSIXlt(tmp$quoteTimeInLong/1000, origin = "1970-01-01"))
  colnames(CLOSE) = paste0(ticker,".Close")
  CLOSE
}


fetchData = function(symbol)
{
  con <- dbConnect(odbc(), Driver = "/usr/local/mysql-connector-odbc-8.0.11-macos10.13-x86-64bit/lib/libmyodbc8a.so", 
                   Server = "localhost", Database = "DATA", UID = "root", PWD = PASS$PWD, 
                   Port = 3306)
  df <- data.frame(dbGetQuery(con,paste0("SELECT * FROM barChartStocks20190617 WHERE Symbol='",symbol,"';")))
  dbDisconnect(con)
  idx <- as.POSIXct(as.character(df$Timestamp), format="%Y-%m-%d %H:%M:%S", tz="America/New_York") - hours(7)
  df <- as.xts(df[,c("Open","High","Low","Close","Volume")], order.by=idx)
  colnames(df) <- paste0(symbol,".",names(df))
  df
}

ReShape = function(symbol,MINUTES)
{
  df <- fetchData(symbol=symbol)
  df <- to.period(df,period="minutes",k=MINUTES,indexAt = "startof")
  colnames(df) <- paste0(symbol,".",gsub("df.","",names(df)))
  df
}

# find the beginning of the week
findMonday = function(daytoday)
{
  dtoday <- as.Date(daytoday,format="%Y%m%d")
  
  if(weekdays(dtoday) == "Monday")
  {
    return(dtoday)
  }
  if(weekdays(dtoday) == "Tuesday")
  {
    return(dtoday-days(1))
  }
  if(weekdays(dtoday) == "Wednesday")
  {
    return(dtoday-days(2))
  }
  if(weekdays(dtoday) == "Thursday")
  {
    return(dtoday-days(3))
  }
  if(weekdays(dtoday) == "Friday")
  {
    return(dtoday-days(4))
  }
}
daytoday = format(Sys.Date(),"%Y%m%d")
# Request data from Alpha Vantage
newData = function(symbol, MINUTES)
{
  url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",symbol,"&interval=",MINUTES,
                "min&apikey=",PASS$APIKEY,"&datatype=csv&outputsize=full")
  dat <- fread(url,header = TRUE, sep=",")
  # BARS from BARCHART END WITH THE START TIME -> Ex: A bar that closes at 16:00 == 15:30
  # ADJUST BARS FROM ALPHA ADVANTAGE because they End @ their Ending times -> Ex: a bar that closes at 16:00 == 16:00
  idx = as.POSIXct(dat$timestamp, format = "%Y-%m-%d %H:%M:%S", tz="America/New_York") - minutes(MINUTES)
  dat <- xts(dat[,"close"], order.by = idx)
  colnames(dat) <- paste0(symbol,".Close")
  dat <- dat["T09:30/T16:00"]
  # only wnt his week's data
  #dat <- dat[paste0(findMonday(daytoday = daytoday),"::")]
  if(nrow(dat[paste0(findMonday(daytoday = daytoday),"::")]) == 0)
  {
    dat = tail(dat)
    assign(paste(symbol),dat,envir = .GlobalEnv)
    dat
  }else{
    dat <- dat[paste0(findMonday(daytoday = daytoday),"::")]
    assign(paste(symbol),dat,envir = .GlobalEnv)
    dat
  }
  
}

# get data from global environment
get4rmGlobal = function(symbol)
{
  if(paste(symbol) %in% ls(envir = .GlobalEnv))
  {
    get(paste(symbol), envir = .GlobalEnv)
  }else{
    cat("Symbol not found!!")
  }
}

# create a new environment for spreads
e <- new.env()

# Calculate the Pair
PairStrat = function(symbol1, symbol2, TF)
{
  A <- symbol1
  B <- symbol2
  
  RETS1 <- merge.xts(Cl(ReShape(A,MINUTES=TF)),Cl(ReShape(B,MINUTES=TF)))
  RETS1 <- na.locf(RETS1)
  colnames(RETS1) <- c(paste(A),paste(B))
  
  ASSET1 <- RETS1[,A]
  ASSET2 <- RETS1[,B]
  
  index(ASSET1) <- as.POSIXct(index(ASSET1), format= "%Y-%m-%d %H:%M:%S", tz= "America/New_York")
  index(ASSET2) <- as.POSIXct(index(ASSET2), format= "%Y-%m-%d %H:%M:%S", tz= "America/New_York")
  
  # ************************************************
  # INSERT NEW ROW DATA
  # ************************************************
  ASSET1 <- rbind(ASSET1, get4rmGlobal(symbol1)); ASSET1 <- make.index.unique(ASSET1, drop=TRUE, fromLast=FALSE)
  ASSET2 <- rbind(ASSET2, get4rmGlobal(symbol2)); ASSET2 <- make.index.unique(ASSET2, drop=TRUE, fromLast=FALSE)
  # in case the number of rows do not match, use na.approx()
  if(nrow(ASSET1) != nrow(ASSET2))
  {
    tmp <- merge.xts(ASSET1,ASSET2)
    tmp <- na.approx(tmp)
    ASSET1 <- tmp[,A]
    ASSET2 <- tmp[,B]
  }
  m <- lm(ASSET1 ~ ASSET2 + 0)
  beta1 <- coef(m)[1]
  
  sprd <- ASSET1 - (beta1*ASSET2)
  colnames(sprd) <- paste(c(A,B),collapse = "-")
  assign(names(sprd),sprd,envir = e)
  p <- try(suppressWarnings(adf.test(na.omit(sprd), alternative="stationary",k=0)$p.value),silent=TRUE)
  if(!inherits(p,'try-error'))
  {
    last(as.data.frame(cbind(paste(A),paste(B), round(beta1,4),round(p,4)),stringsAsFactors=FALSE))
  }
}

BBandStrat = function(symbol1,symbol2,EQT,SPRDS)
{
  ticker <- paste0(symbol1,".",symbol2)
  data <- SPRDS
  data <- na.locf(data)
  colnames(data) <- gsub(" ","",paste(names(data),".Close"))
  x <- na.omit(merge(data, BBands(data)))
  
  x$sig <- NA
  
  # Flat where Close crossed the mavg
  x$sig[c(FALSE, diff(sign(Cl(x) - x$mavg), na.pad=FALSE) != 0)] <- 0
  x$sig[Cl(x) > x$up] <- -1 # short when Close is above up
  x$sig[Cl(x) < x$dn] <- 1 # long when Close is below dn
  x$sig[1] <- 0 # flat on the first day
  x$sig[nrow(x)] <- 0 # flat on the last day
  
  # Fill in the signal for other times
  x$sig <- na.locf(x$sig) # wherever sig is NA, copy previous value to next row
  
  # Now Lag your signal to reflect that you can't trade on the same bar that 
  # your signal fires
  x$sig <- Lag(x$sig)
  x$sig[1] <- 0 # replace NA with zero position on first row
  assign("SIGNAL",x$sig,envir = .GlobalEnv)
  
  tic <- as.character(do.call(c,str_split(ticker,pattern = "\\.")))

  AA <- rbind(Cl(ReShape(symbol=tic[1],MINUTES=TF)),get4rmGlobal(symbol1))
  AA <- make.index.unique(AA,drop=TRUE,fromLast = FALSE)
  BB <- rbind(Cl(ReShape(symbol=tic[2],MINUTES=TF)),get4rmGlobal(symbol2))
  BB <- make.index.unique(BB,drop=TRUE,fromLast = FALSE)
  
  x <- na.omit(na.approx(merge.xts(AA,BB,x)))
  x$ASSET1 <- NA
  x$ASSET2 <- NA
  
  x$ASSET1[1] <- x[1,1]
  x$ASSET2[1] <- x[1,2]
  # TRADE PRICE FOR ASSET 1
  for(k in 2:(nrow(x))){
    x[k,"ASSET1"] <- ifelse(coredata(x[(k-1),"sig"])==coredata(x[k,"sig"]), x[(k-1),"ASSET1"],x[k,1])
  }
  # TRADE PRICE FOR ASSET 2
  for(k in 2:(nrow(x))){
    x[k,"ASSET2"] <- ifelse(coredata(x[(k-1),"sig"])==coredata(x[k,"sig"]), x[(k-1),10],x[k,2])
  }
  ## P&L POINTS FOR ASSET 1
  x$POINTS1 <-0
  for(k in 2:nrow(x)){
    x[k,"POINTS1"] <- round(ifelse(coredata(x[(k),"ASSET1"])!=coredata(x[(k-1),"ASSET1"]),
                                   reclass(((coredata(x[k,"ASSET1"])-coredata(x[(k-1),"ASSET1"]))*coredata(x[k-1,"sig"])), match.to=x)
                                   ,reclass(0,match.to=x)),4)
  }
  # P&L POINTS FOR ASSET 2
  x$POINTS2 <-0
  SIGN <- ifelse(x[,"sig"] > 0,1,-1)
  for(k in 2:nrow(x)){
    x[k,"POINTS2"] <- round(ifelse(coredata(x[(k),"ASSET2"])!=coredata(x[(k-1),"ASSET2"]),
                                   reclass((((coredata(x[k,"ASSET2"]) - coredata(x[(k-1),"ASSET2"]))*coredata(x[k-1,"sig"]*-1))), match.to=x)
                                   ,reclass(0,match.to=x)),4)
  }
  ## SHARE SIZE FOR ASSET 1
  SPLIT <- EQT/2 
  x$SHARES1 <- 0
  for(k in 2:nrow(x)){
    x[k,"SHARES1"] <- round(SPLIT/coredata(x[k,"ASSET1"]),0)
  }
  ## SHARE SIZE FOR ASSET 2
  x$SHARES2 <- 0
  for(k in 2:nrow(x)){
    x[k,"SHARES2"] <- round(SPLIT/coredata(x[k,"ASSET2"]),0)
  }
  # CALCULATE GROSS PROFIT
  x$GROSS <- round(((x[,"POINTS1"])*lag(x[,"SHARES1"])) + ((x[,"POINTS2"])*lag(x[,"SHARES2"])),2) 
  # CALCULATE NET PROFIT
  # 4 * 1.00 == COMMISSIONS
  x$NET <- 0
  for(k in 2:nrow(x)){
    x[k,"NET"] <- ifelse(coredata(x[k,"GROSS"]) != 0, reclass((coredata(x[k,"GROSS"]) - (1.00*4)), match.to=x[k,"GROSS"]), 0)
  }
  # CALCULATE EQUITY
  x$EQT <- EQT
  for(k in 2:nrow(x)){
    x[k,"EQT"] <- ifelse(coredata(x[(k),"NET"])!=0,
                         reclass(coredata(x[k,"NET"]) + coredata(x[(k-1),"EQT"]), match.to=x)
                         ,x[(k-1),"EQT"])
  }
  
  # CALCULATE DRAWDOWNS
  x$DD <- round(-(cummax(x[,"EQT"])-x[,"EQT"])/x[,"EQT"],4)
  
  # COUNT THE NUMBER TRADES
  x$Trades <- 0
  for(k in 2:nrow(x)){
    x[k,"Trades"] <- ifelse(coredata(x[k,"NET"]) != 0 , reclass(1+coredata(x[k-1,"Trades"]),match.to=x), x[k-1,"Trades"])
  }
  # COUNT DRAWDOWN DURATION
  x$duration <- 0
  for(k in 2:nrow(x)){
    x[k,"duration"] <- ifelse(coredata(x[k,"DD"]) != 0, reclass(sum(coredata(x[(k-1),"duration"])+1), match.to=x[k,1]), 0)
  }
  return(x)
}


BUY.SELL.CLOSE = function(CURRENT.TICK,PREVIOUS.TICK, tmp, stock1, stock2)
{
  if(CURRENT.TICK != PREVIOUS.TICK)
  {
    # *******************************************************
    # OPEN A NEW POSITION if CURRENT.TICK is 1 
    # *******************************************************
    if(CURRENT.TICK == 1)
    {
      # Check to see if previous position existed & CLOSE THE POSITIONS
      if(PREVIOUS.TICK == -(CURRENT.TICK))
      {
        ZERO.POSITIONS()
        SEND.LONG(tmp,stock1=stock1,stock2=stock2)
      }else{
        SEND.LONG(tmp,stock1=stock1,stock2=stock2)
      }
    }
    # *******************************************************
    # OPEN A NEW POSITION if CURRENT.TICK is -1 
    # *******************************************************
    if(CURRENT.TICK == -1)
    {
      # Check to see if previous position existed & CLOSE THE POSITIONS
      if(PREVIOUS.TICK == -(CURRENT.TICK))
      {
        ZERO.POSITIONS()
        SEND.SHORT(tmp,stock1=stock1,stock2=stock2)
      }else{
        SEND.SHORT(tmp,stock1=stock1,stock2=stock2)
      }
    }
    # *******************************************************
    # CLOSE A POSITION if CURRENT TICK is 0
    # *******************************************************
    if(CURRENT.TICK == 0)
    {
      ZERO.POSITIONS()
    }
  }else{
    doNothing()
  }
}



doNothing = function()
{
  cat(paste0(" NOTHING TO BUY SELL OR CLOSE: ", Sys.time(), "\n"))
}

ZERO.POSITIONS = function()
{
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  positions = twsPortfolioValue(ac,zero.pos=FALSE)
  #**********************************************************************
  #  CLOSING OUT
  #**********************************************************************
  # test to see if we have any positions to close
  if(!is.null(positions))
  {
    current.tickers <- as.character(positions$local)
    current.POS     <- as.character(positions$position)
    security1       <- twsSTK(current.tickers[1])
    security2       <- twsSTK(current.tickers[2])
    security1shrs   <- as.character(abs(as.numeric(current.POS[1])))
    security2shrs   <- as.character(abs(as.numeric(current.POS[2])))
    
    # BUY OR SELL
    ifelse((as.numeric(current.POS[1]) < 0), ACT1 <- "BUY", ACT1 <- "SELL")
    ifelse((as.numeric(current.POS[2]) < 0), ACT2 <- "BUY", ACT2 <- "SELL")
    orderId1 = as.numeric(reqIds(tws))
    orderId2 = as.numeric(reqIds(tws))
    
    # SEND THE ORDER
    myorder1 = twsOrder(orderId1, orderType = "MKT", outsideRTH = "0",action=ACT1, totalQuantity = security1shrs, transmit=TRUE)
    myorder2 = twsOrder(orderId2, orderType = "MKT", outsideRTH = "0",action=ACT2, totalQuantity = security2shrs, transmit=TRUE)
    
    placeOrder(tws,security1, myorder1)
    placeOrder(tws,security2, myorder2)
  }
  twsDisconnect(tws)
}

SEND.LONG = function(tmp,stock1,stock2)
{
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  positions = twsPortfolioValue(ac,zero.pos=FALSE)
  #**********************************************************************
  #  LONG SPREAD
  #**********************************************************************
  security1       <- twsSTK(paste(stock1))
  security2       <- twsSTK(paste(stock2))
  security1shrs   <- as.character(as.numeric(last(tmp$SHARES1)))
  security2shrs   <- as.character(as.numeric(last(tmp$SHARES2)))
  
  ACT1 <- "BUY"
  ACT2 <- "SELL"
  orderId1 = as.numeric(reqIds(tws))
  orderId2 = as.numeric(reqIds(tws))
  
  myorder1 = twsOrder(orderId1, orderType = "MKT", outsideRTH = "0",action=ACT1, totalQuantity = security1shrs, transmit=TRUE)
  myorder2 = twsOrder(orderId2, orderType = "MKT", outsideRTH = "0",action=ACT2, totalQuantity = security2shrs, transmit=TRUE)
  
  placeOrder(tws,security1, myorder1)
  placeOrder(tws,security2, myorder2)
  twsDisconnect(tws)
}

SEND.SHORT = function(tmp,stock1,stock2)
{
  tws = twsConnect(port=7497)
  ac <- reqAccountUpdates(tws)
  positions = twsPortfolioValue(ac,zero.pos=FALSE)
  #**********************************************************************
  #  SHORT SPREAD
  #**********************************************************************
  security1       <- twsSTK(paste(stock1))
  security2       <- twsSTK(paste(stock2))
  security1shrs   <- as.character(as.numeric(last(tmp$SHARES1)))
  security2shrs   <- as.character(as.numeric(last(tmp$SHARES2)))
  
  ACT1 <- "SELL"
  ACT2 <- "BUY"
  orderId1 = as.numeric(reqIds(tws))
  orderId2 = as.numeric(reqIds(tws))
  
  myorder1 = twsOrder(orderId1, orderType = "MKT", outsideRTH = "0",action=ACT1, totalQuantity = security1shrs, transmit=TRUE)
  myorder2 = twsOrder(orderId2, orderType = "MKT", outsideRTH = "0",action=ACT2, totalQuantity = security2shrs, transmit=TRUE)
  
  placeOrder(tws,security1, myorder1)
  placeOrder(tws,security2, myorder2)
  twsDisconnect(tws)
}


# DETERMINE THE NEXT DAY
DAYTODAY = function()
{
  NOW <- Sys.time()
  if(NOW > as.POSIXct(paste0(Sys.Date(), " 13:00:00")) & NOW < as.POSIXct(paste0(Sys.Date(), " 23:59:59")))
  {
    daytoday = format(Sys.Date()+1, "%Y%m%d")
  }else{
    daytoday = format(Sys.Date(),"%Y%m%d")
  }
}


# SLEEP UNTIL MARKET OPENS
SLEEEP = function(xx)
{
  ttt <- tmz[xx] - Sys.time()
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ", tt, " hours")); cat("\n")
    print(paste0("STARTING AT: ", tmz[xx], "")); cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ", tt, " minutes")); cat("\n")
    print(paste0("STARTING AT: ", tmz[xx], "")); cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ", tt, " seconds")); cat("\n")
    print(paste0("STARTING AT: ", tmz[xx], "")); cat("\n")
    Sys.sleep(tt)
  }
}

display = function(tmp,SPRDS,stock1, stock2)
{
  cat("\n")
  index(tmp) <- index(tmp) - hours(3) + minutes(30)
  print(tmp)
  index(SPRDS) <- index(SPRDS) - hours(3) + minutes(30)
  ct <- SPRDS
  colnames(ct) <- gsub(paste0("",stock1,"-",stock2,""), paste0("",stock1,".Close"),names(ct))
  chartSeries(ct[paste0(Sys.Date() - days(7),"::")], name = paste0("",stock1,"-",stock2,""))
  pdf(paste0("~/Desktop/",paste0("",stock1,"-",stock2,""),".pdf"), width = 6, height = 4, paper = "special")
  print(addBBands())
  dev.off()
}











