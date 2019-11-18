library(jsonlite)
#Symbols for which stock prices are collected.
symb <- c("TMUS","GM","TSLA","BRK.B","PYPL","TWTR","FB","XOM","WMT","WFC","VZ","UPS","PG","MRK","FDX",
          "BBY","MCD","JPM","KMB","JNJ","GE","F","AXP","COF","BAC","BA","MSFT","COKE","AMZN","AAPL")

setwd("C:/Users/doshi/OneDrive/Everything PC/Study/Grad School/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Scripts and Output/")
tok = fromJSON("stocks_token.json")$token

setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project")

#Script to get tweets, scheduler via taskscheduleR to run every 15 minutes.
getstocks<- function(sy){
  req = paste0("https://cloud.iexapis.com/beta/stock/",sy,"/quote?token=",tok)
  #Recieving response as a JSON, and parsing the relevant information and saving it the csv file
  cpl <- fromJSON(req)
  if(is.null(cpl$open)){
    cpl$open=0
  }
  if(is.null(cpl$peRatio)){
    cpl$peRatio=0
  }
  info <- data.frame(cpl$companyName,cpl$open,cpl$close,cpl$latestPrice,cpl$latestVolume,cpl$peRatio,cpl$marketCap,cpl$change,date())
  setwd("stocks")
  na <- paste0(sy,".csv")
  write.table(info,na,sep=",",append = T,col.names = FALSE, row.names = FALSE)
  setwd("..")
} 


for (x in symb){
  getstocks(x)
}

