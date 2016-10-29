##########################################
## Daily prices from Yahoo 
## thertrader@gmail.com - Nov. 2015
##########################################
library(quantmod)

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