
#recentTrade = recentTrades

source("listOfSymbols.r")
#initialize variable theInstruments

InitTrades <- function(FileName,TradeSystem) {

  dfTrades = data.frame(matrix(nrow = 1,ncol = 8))
  colnames(dfTrades)<-c("Date","System","Symbol","Position","Price","ATR14","SL","Action")
    
  dummyRecord <- dfTrades #inherit structure
  dummyRecord$Date <- Sys.Date() - 100
  dummyRecord$System <- TradeSystem 
  dummyRecord$Position <- "flat"
  
  if (!file.exists(FileName))  {file.create(FileName) }  
  
  for (TradeSymbol in theInstruments) {
    
    dummyRecord$Symbol <- TradeSymbol
    dfTrades <- rbind(dfTrades,dummyRecord)
  }
  
  write.csv(dfTrades,FileName,row.names=FALSE)
}