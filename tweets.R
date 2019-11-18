library(rtweet)
library(dplyr)
library(taskscheduleR)
library(httpuv)
library(jsonlite)

setwd("C:/Users/doshi/OneDrive/Everything PC/Study/Grad School/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Scripts and Output/")

#Creating token to query twitter API
token <-create_token(app = fromJSON("token.json")$app,
                     consumer_key = fromJSON("token.json")$consumer_key,
                     consumer_secret = fromJSON("token.json")$consumer_secret,
                     access_token = fromJSON("token.json")$access_token,
                     access_secret = fromJSON("token.json")$access_secret)


#Symbols for which tweets are collected.
symb <- c("TMUS","General Motors","Tesla","Berkshire Hathaway","Paypal","Twitter","Facebook","Exxon","Walmart","Wells Fargo",
          "Verizon","United Postal","Procter Gamble","Merkle","Fedex","Boeing","McDonalds","Kimberly Clark","JP Morgan","Johnson Johnson",
          "General Electric","Ford","AMEX","Capital One","Bank of America","Best Buy","Microsoft","Coca Cola","Amazon","Apple")

setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project")

#Script to get tweets, scheduler via taskscheduleR to run every 15 minutes.
gettweets<- function(symb){
  recentTweets <- search_tweets(symb, n=600, lang = "en")
  removecol <- c(4,8:11,18,19,21,22,24:27,29,30,32:34,38,39,42:44,47:49,53,54,57:59,61:71,73:75,78,83:88)
  recentTweets <- recentTweets[,-removecol]
  
  date <- as.character(Sys.time())
  date <- gsub(":","_",date)
  
  setwd("twitterdata")
  filename <- paste0(symb,"-",date,".RDS")
  saveRDS(recentTweets, filename)
  setwd("..")
  
  print(paste(symb,"Tweets collected: ",date))
}

#Looping through all the symbols
for (x in symb){
  gettweets(x)
}
