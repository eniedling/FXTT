##########################################
## Emailing today's trades and positions
## eric.niedling@gmail.com - Sep. 2016
##########################################

library(plyr)
library(htmlTable)
library(mailR)

thePath = "D:\\temp\\MyProjects\\TA\\"
tradesFile = "Trades.dat"

tradeData = data.frame(matrix(nrow = 1,ncol = 8))
colnames(tradeData)<-c("Date","System","Symbol","Position","Price","ATR14","SL","Action")
recentTrade = tradeData

tradeData = read.csv(paste(thePath,tradesFile,sep=""))

keyDate = Sys.Date() - 1
keyDate = as.character(keyDate)

tradeUpdate = subset(tradeData, Date == keyDate, select = -ATR14)
emailBody = htmlTable(tradeUpdate)

sender <- "eric.niedling@gmail.com"
recipients <- c("eric.niedling@gmail.com","eric_niedling@hotmail.com")
send.mail(from = sender,
          to = recipients,
          subject = "Daily trade updates",
          body = emailBody,
          html = TRUE, 
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "eric.niedling@googlemail.com",
                      passwd = "Gina.1909", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
