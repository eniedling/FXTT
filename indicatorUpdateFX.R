##########################################
## Calc indicator values 
## eric.niedling@gmail.com - Aug. 2016
##########################################


library(TTR)
library(plyr)

thePath = "D:\\temp\\MyProjects\\"
theFiles = list.files(path=thePath,pattern=".csv")

#ptm <- proc.time()
for (ii in theFiles){

  data = read.csv(paste(thePath,ii,sep=""))
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
  
  write.csv(data,paste(thePath,"TA\\TA_",ii,sep = ""),row.names=FALSE)
  
}
#CodeTime = proc.time() - ptm
#print(CodeTime)
#print("Indicators calculated.")