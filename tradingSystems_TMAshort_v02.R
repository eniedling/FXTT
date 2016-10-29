##########################################
## Trading systems
## eric.niedling@gmail.com - Aug. 2016
##########################################

require(plyr)

thePath = "data\\"
theTAFiles = list.files(path=thePath,pattern="^TA")
tradesFile = "Trades.dat"

recentTrades = data.frame(matrix(nrow = 1,ncol = 8))
colnames(recentTrades)<-c("Date","System","Symbol","Position","Price","ATR14","SL","Action")
recentTrade = recentTrades

tradeData = read.csv(paste(thePath,tradesFile,sep=""))
colnames(tradeData)<-colnames(recentTrades)
#ptm <- proc.time()
#for (x in 20:0) {

  # fear each symbol
  for (ii in theTAFiles) {
#    ii = "TA_XAU_USD.csv" 
    #load indicator data
    indicatorData = read.csv(paste(thePath,ii,sep=""))
#    print(paste("Processing:",ii,sep = " "))
    
    lastRecord = tail(indicatorData,1)
    lastIndicatorDate = as.Date(lastRecord$Index,format="%Y-%m-%d")
#    lastRecord = data[nrow(data)-x,]
#  lastRecord = data[data$Index == "2016-01-01",]
#  }

  tradeSymbol = substr(ii,4,nchar(ii)-4)
  allSystems = subset(tradeData, Symbol=tradeSymbol, select = System )
  uniqueSystems = na.omit(unique(allSystems))
  
  for (vSystem in uniqueSystems) {
#    print(paste("For each:",vSystem,sep = " "))
    
    if ( vSystem == "TMAshort" ) { 
#      print("If vSystem == TMAshort")
     vSystem = "TMAshort"
      #System: TMAshort
      systemTrades = subset(tradeData, System == vSystem & Symbol == tradeSymbol )
      lastTrade = tail(systemTrades,1)
      lastTradeDate = as.Date(lastTrade$Date,format="%Y-%m-%d")
    
      while ( lastTradeDate < lastIndicatorDate ) {
        
        tempSL = NULL
        
        recentTrade$Date = lastTradeDate + 1
        vOffset = as.numeric(lastIndicatorDate - lastTradeDate)
        newIndicator = head(tail(indicatorData,vOffset),1)
        
        recentTrade$ATR14 = newIndicator$ATR14
        recentTrade$Price = newIndicator$close
        recentTrade$System = vSystem
        recentTrade$Symbol = tradeSymbol
        recentTrade$Action = ""
        #new long signal
        if ( newIndicator$SMA10 > newIndicator$SMA20   && 
             newIndicator$SMA20 > newIndicator$SMA50   &&
             recentTrade$Price > newIndicator$SMA10    &&
             lastTrade$Position == 'flat')  {
          
          recentTrade$Position = 'long'
          recentTrade$SL = newIndicator$close - ( 2*newIndicator$ATR14 )
          # recentTrade$OPrice = recentTrade$Price
           recentTrade$Action = 'open'
        
        }  
        # current position gets stopped out
        else if (lastTrade$Position == 'long' && 
                 recentTrade$Price < lastTrade$SL ) {
          recentTrade$Position = 'flat'
          recentTrade$Action = 'StopLoss'
         
        }
        # current position exited by system 
        else if (lastTrade$Position == 'long' && 
                 newIndicator$SMA10 < newIndicator$SMA20 ) {
          recentTrade$Position = 'flat'
           recentTrade$Action = 'Exit'
        }
        # trend continues, adjusting SL?
        else if (lastTrade$Position == 'long' && 
                 newIndicator$SMA10 > newIndicator$SMA20 ) {
          recentTrade$Position = 'long'
          tempSL = newIndicator$close - ( 2*newIndicator$ATR14 )
          if (tempSL > lastTrade$SL) {
            recentTrade$SL = tempSL
          }
          else { recentTrade$SL = lastTrade$SL }
        }

        #new short signal
        else if ( newIndicator$SMA10 < newIndicator$SMA20   && 
             newIndicator$SMA20 < newIndicator$SMA50   &&
             recentTrade$Price < newIndicator$SMA10    &&
             lastTrade$Position == 'flat' )  {
          
          recentTrade$Position = 'short'
          recentTrade$SL = newIndicator$close + ( 2*newIndicator$ATR14 )
          # recentTrade$OPrice = recentTrade$Price
           recentTrade$Action = 'open'
          
        }  
        # current position gets stopped out
        else if ( lastTrade$Position == 'short' && 
                  recentTrade$Price > as.numeric(paste(lastTrade$SL)) ) {
          recentTrade$Position = 'flat'
          recentTrade$Action = 'StopLoss'
        }
        # current position exited by system 
        else if (lastTrade$Position == 'short' && 
                 newIndicator$SMA10 > newIndicator$SMA20 ) {
          recentTrade$Position = 'flat'
          recentTrade$Action = 'Exit'
        }
        # trend continues, adjusting SL?
        else if (lastTrade$Position == 'short' && 
                 newIndicator$SMA10 < newIndicator$SMA20 ) {
          recentTrade$Position = 'short'
          tempSL = newIndicator$close + ( 2*newIndicator$ATR14 )
          if (tempSL < lastTrade$SL) {
            recentTrade$SL = tempSL
          }
          else { recentTrade$SL = lastTrade$SL }
        }          
        # no new entry, previous position was flat
        else if (lastTrade$Position == 'flat' ){
          recentTrade$Position = 'flat'
          recentTrade$SL <- "0"
        }
        # catch unmanaged cases, if any
        else {
          print(paste(recentTrade$Date,recentTrade$Symbol, 
                      recentTrade$System,"not managed", sep = " "))
        }
    
        #wrapping it up 
        recentTrade$Date = as.character(recentTrade$Date)
        recentTrade$Price <- round(recentTrade$Price,4)
        if ( !is.na(recentTrade$SL)  )
          { recentTrade$SL <- round(as.numeric(recentTrade$SL),4) }
        tradeData = rbind(tradeData, recentTrade)
        lastTrade = recentTrade
        lastTradeDate = as.Date(lastTrade$Date,format="%Y-%m-%d")
      
        }  #  while (lastTradeDate < lastIndicatorDate)
    } # if ( vSystem == "TMAshort" )
    
  } # for (vSystem in uniqueSystems)
  
} # for (ii in theTAFiles)

  
#  print(paste(lastRecord$Index, substr(ii,4,nchar(ii)-4), recentTrade$Position,sep=" "))
#  recentTrade = c(lastRecord$Index,"TMAshort",substr(ii,4,nchar(ii)-4),TMAShortPos,TMAShortPosSL)
#  colnames(recentTrade) <- c("Date","System","Symbol","Position","SL")

#  recentTrade <- recentTrade[NULL,]
#  print(paste(recentTrade$Date,"done",sep = " "))
#}

write.csv(tradeData,paste(thePath,"Trades.dat",sep = ""),row.names=FALSE)

