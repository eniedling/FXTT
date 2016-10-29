##########################################
## Trend Trading Machine
## eric.niedling@gmail.com - Aug. 2016
##########################################
rm(list=ls())
theCodePath = "D:\\temp\\MyProjects\\"

UpdateHistoricalData <- function() {
  
  require(quantmod)
  startDate = "2013-01-01"
  
  source("listOfSymbols.r")
  
  for (ii in theInstruments){
    print(ii)
    data = getSymbols(Symbols = ii, 
                      src = "oanda", 
                      from = startDate, 
                      #                    to = endDate,
                      auto.assign = FALSE)
    colnames(data) = c("close")
    nameofFile = gsub("/", "_", ii)
    write.zoo(data,paste("data\\",nameofFile,".csv",sep=""),sep=",",row.names=FALSE)
  }
  print("Initialized files.")
  
}

UpdateIndicators <- function() {
  
  require(TTR)
  require(plyr)
  
#  thePath = "D:\\temp\\MyProjects\\"
  theFiles = list.files(path="data\\",pattern=".csv")
  
  #ptm <- proc.time()
  for (ii in theFiles){
    
    data = read.csv(paste("data\\",ii,sep=""))
    data$high = data$close
    data$low = data$close
    
    # calc SMA10  
    sma10 <- EMA(data[c('close')],n=10)
    sma10 <- data.frame(sma10)
    colnames(sma10) = c("SMA10")
    # calc SMA10  
    sma20 <- EMA(data[c('close')],n=20)
    sma20 <- data.frame(sma20)
    colnames(sma20) = c("SMA20")
    # calc SMA50  
    sma50 <- EMA(data[c('close')],n=50)
    sma50 <- data.frame(sma50)
    colnames(sma50) = c("SMA50")
    # calc SMA200  
    sma200 <- EMA(data[c('close')],n=200)
    sma200 <- data.frame(sma200)
    colnames(sma200) = c("SMA200")
    # calc SMA800  
    sma800 <- EMA(data[c('close')],n=800)
    sma800 <- data.frame(sma800)
    colnames(sma800) = c("SMA800")
    
    # calc ATR  
    ATR14 <- ATR(data[,c("high","low","close")],n=14)
    ATR14 <- data.frame(ATR14)
    #  colnames(ATR14) = c("ATR14")
    
    data = data.frame(data,sma10,sma20,sma50,sma200,sma800,ATR14$atr)
    data = rename(data, c("ATR14.atr"="ATR14"))
    
    write.csv(data,paste("data\\TA_",ii,sep = ""),row.names=FALSE)
    
  } #end for
} # end function

#print(paste("Updating historical prices",Sys.time(),sep = " "))
#source("historicalDataFX.R")
#print("Done")

# print(paste("Updating indicators",Sys.time(),sep = " "))
# source(paste(theCodePath,"code\\indicatorUpdateFX.R",sep=""))
# print("Done")

TMA_short <- function() {
   source("tradingSystems_TMAshort_v02.R")
   return()
 }

SendEmailUpdate <- function(GUserName, GPassword, EmailReceivers) {
  
  require(plyr)
  require(htmlTable)
  require(mailR)
  
#  thePath = "D:\\temp\\MyProjects\\TA\\"
#  tradesFile = "Trades.dat"
  
  tradeData = data.frame(matrix(nrow = 1,ncol = 8))
  colnames(tradeData)<-c("Date","System","Symbol","Position","Price","ATR14","SL","Action")
  recentTrade = tradeData
  
  tradeData = read.csv("data\\Trades.dat")
  
  keyDate = Sys.Date() - 1
  keyDate = as.character(keyDate)
  
  tradeUpdate = subset(tradeData, Date == keyDate, select = -ATR14)
  emailBody = htmlTable(tradeUpdate)
  
  sender <- GUserName
#  recipients <- c("eric.niedling@gmail.com","eric_niedling@hotmail.com")
  recipients <- EmailReceivers
    send.mail(from = sender,
            to = recipients,
            subject = "Daily trade updates",
            body = emailBody,
            html = TRUE, 
            smtp = list(host.name = "smtp.gmail.com", port = 465,
                        user.name = GUserName,
                        passwd = GPassword, ssl = TRUE),
            authenticate = TRUE,
            send = TRUE)
  
}


## Main: calling the functions
# prep
UpdateHistoricalData()
UpdateIndicators()

# Systems
TMA_short()

# email the result
#SendEmailUpdate("user@gmail.com","password","eric.niedling@gmail.com")

