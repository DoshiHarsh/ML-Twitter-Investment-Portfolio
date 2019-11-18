library(tidyr)
library(sentimentr)
library(readxl)
library(dplyr)
library(stringr)

#Merging all the tweet files into one. 
ticks <- c("Tesla","Twitter","Facebook","Walmart","Wells Fargo","Verizon","Boeing","McDonalds","Paypal",
           "Ford","Best Buy","Microsoft","Coca Cola","Amazon","Apple")
#Link to the data that needs to combined, in the wd below related to merging all the validation data.
setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Validation Data/")
wd<-getwd()
for (x in ticks){
  mergedat <- do.call('rbind', lapply(list.files(path=wd, pattern=paste0(x,"*"), full.names = TRUE), readRDS))
  setwd("agg")
  saveRDS(mergedat,x)
  setwd("..")
}

#calcuating return values for SP500 data
sp <- read_excel('C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/stocks/sp500val.xlsx')
#Reording the file from reverse chronological to chronological
sp<- sp[seq(dim(sp)[1],1),]
#Calulating the return variable for stocks based on change in percent from the last time period.
sp$return <- 0
for (x in (2:nrow(sp))){
  sp$return[x] = ((sp$`Last Price`[x]/sp$`Last Price`[x-1])-1)*100
}
sp$return[1]<-NA
sp<- sp[,-c(2,3)]
colnames(sp) <- c("Date","baseline")
#Formatting the dates to correct formats
sp$date <- format(sp$Date,'%d')
sp$month <-format(sp$Date,'%m')
sp$year <-format(sp$Date,'%Y')
sp$hour <-format(sp$Date,'%H')
sp$minute <-format(sp$Date,'%M')
sp <- sp[,-1]

#Creating win-lose variables and cleaning the stock files for merging.

setwd('C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/stocks/')
path= getwd()
files <- list.files(path=path, pattern="*.csv", recursive=FALSE)
lapply(files, function(x) {
  st <- read.csv(files[20]) # load file
  #Formatting the dates to correct formats
  st$date.. <- strptime(st$date..,'%a %b %d %H:%M:%S %Y')
  st$date <- format(st$date..,'%d')
  st$month <-format(st$date..,'%m')
  st$year <-format(st$date..,'%Y')
  st$hour <-format(st$date..,'%H')
  st$minute <-format(st$date..,'%M')
  #Choosing the dates required from the stock set.
  last <- ISOdate(2019,04,29,22,10)
  first<- ISOdate(2019,04,07,22,50)
  last<- strptime(last,'%Y-%m-%d %H:%M:%S')
  first<- strptime(first,'%Y-%m-%d %H:%M:%S')
  st=st[!st$date..>last,]
  st=st[!st$date..<first,]
  #Segmenting the time to the required bins
  st$minute <- ifelse(st$minute<15,0,
                      ifelse(st$minute<30,15,
                             ifelse(st$minute<45,30,45)))
  st$date..<- as.POSIXct(st$date..)
  
  st <- st[,-c(1:3,6:9)]
  #Calculating the return from the last period
  st$return <- 0 
  for (x in (2:nrow(st))){
    st$return[x] <- ((st$cpl.latestPrice[x]/st$cpl.latestPrice[x-1])-1)*100
  }
  #Removing the null values(Sometimes a null row showed up)
  st <- st[complete.cases(st),] 
  #Left join on stocks and the sp baseline
  st <- left_join(st,sp, by = c("date"="date","month"='month','hour'='hour',"minute"='minute','year'="year"))
  
  #For rows where there are no values for baseline, replacing them with 0, because the change would be 0.
  for (z in (1:length(st$baseline))){
    if(is.na(st$baseline[z])){
      st$baseline[z]="0"
    }}
  #Creating the win-lose variable based on what the stock does based on the difference between baseline 
  st$win_lose <- 1
  for (y in 1:length(st$cpl.companyName)){
    st$win_lose[y] <- ifelse(st$return[y+1]>st$baseline[y+1],1,0)
  }
  setwd("val stocks")
  write.csv(st,files[20])
  setwd("..")
})

# Cleaning, sentiment analysis an data pre-processing on tweets data.

setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Validation Data/Merged Files")
path=getwd()
files <- list.files(path=path, pattern="*.RDS", recursive=FALSE)
lapply(files, function(x) {
  print(x)
  strt <- Sys.time()
  df <- readRDS(x)
  print("read")
  #Basic text cleaning based on information in the tweets.
  df$text = gsub("#.*? ","",df$text)
  df$text = gsub("@.*? ","",df$text)
  df$text = gsub(" RT "," ",df$text)
  df$text = gsub("\n"," ",df$text)
  df$text = gsub("\t"," ",df$text)
  df$text = gsub("https.*? ","",df$text)
  df$text = gsub("https.*","",df$text)
  print("replaced")
  #Creating required variables and flattening out lists inside dataframes.
  df$urls_url=ifelse(is.na(df$urls_url)==TRUE,0,1)
  df$mentions_user_id=ifelse(is.na(df$mentions_user_id)==TRUE,0,1)
  df$media_url=ifelse(is.na(df$media_url)==TRUE,0,1)
  df$quoted_status_id=ifelse(is.na(df$quoted_status_id)==TRUE,0,1)
  df$hashtags <- sapply(df$hashtags, paste, collapse = ",")
  df$symbols <- sapply(df$symbols, paste, collapse = ",")
  print("cleaned")
  #Sentiment analysis using sentimentr and merging that with the dataframe
  d <-get_sentences(df$text)
  c <-sentiment_by(d)
  df[ , "sentiment"] <- c$ave_sentiment
  df[,"std dev"] <- c$sd
  print("sentiment")
  #formatting columns as correct one
  df$source<- as.factor(df$source)
  df$verified<-as.factor(df$verified)
  #formatting date as seperate columns
  df$account_created_at<- strptime(df$account_created_at, "%Y-%m-%d %H:%M",tz= "UTC")
  df$created_at <- strptime(df$created_at, "%Y-%m-%d %H:%M",tz= "UTC")
  df$created_at <- df$created_at-14400
  df$date <- format(df$created_at,'%d')
  df$month <-format(df$created_at,'%m')
  df$year <-format(df$created_at,'%Y')
  df$hour <-format(df$created_at,'%H')
  df$minute <-format(df$created_at,'%M')
  #Segmenting the time to the required bins
  df$minute <- ifelse(df$minute<15,0,
                      ifelse(df$minute<30,15,
                             ifelse(df$minute<45,30,45)))
  print("variables")
  setwd("updated")
  saveRDS(df,x)
  setwd("..")
  end <- Sys.time()
  print(end-strt)
})

#Merging stocks and twitter files.

setwd('C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Validation Data/Merged Files/updated/')
path=getwd()
files <- list.files(path=path, pattern="*.RDS", recursive=FALSE)
lapply(files, function(x) {
  df<-readRDS(x)
  #Creating the count for the hashtags using the number of ',' in the flattened list.
  cnt <- str_count(df$hashtags, ",")+1
  cnt <- ifelse(df$hashtags=='NA',0,cnt)
  df$hashtags <- cnt
  #Creating the count for the symbols using the number of ',' in the flattened list.
  cnt1 <- str_count(df$symbols, ",")+1
  cnt1 <- ifelse(df$symbols=='NA',0,cnt1)
  df$symbols <- cnt1
  df$media_type<-NULL
  nam<-gsub(".RDS",".csv",x)
  #Reading the csv files with the same name.
  st <-read.csv(nam)
  st <- st[,-c(1,2)]
  #formatting date as seperate columns and correct format
  df$date<-as.integer(df$date)
  df$month<-as.integer(df$month)
  df$hour<-as.integer(df$hour)
  df$minute <- as.integer(df$minute)
  df$year <- as.integer(df$year)
  #merfing the stock and twitter files
  df <- inner_join(df,st, by = c("date"="date","month"='month','hour'='hour',"minute"='minute','year'="year"))
  #creating the market trading variables where the sp500 baseline is non zero and hours are trading hours.
  df$markettrading <- 0
  df$markettrading <- ifelse(df$hour>16,0,ifelse(df$hour==16 & df$minute>0,0,ifelse(df$hour<9,0,ifelse(df$hour==9 & df$minute<30,0,1))))
  df$markettrading = ifelse(df$baseline!=0,df$markettrading,0)
  setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Validation Data/")
  saveRDS(df,x)
  setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Validation Data/Merged Files/updated/")
})



#Aggregating the data to single row per time period
setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Validation Data/")
path=getwd()
files <- list.files(path=path, pattern="*.RDS", recursive=FALSE)
lapply(files, function(x) {
  print(x)
  df <- readRDS(x)
  
  
  df_chas <- df[,-c(1:5,16:22,25,29,34)]
  #aggregating the data set
  basic_summ = group_by(df_chas, year, month,date, hour, minute )
  basic_summ = summarise_all(basic_summ, list(mean), na.rm=TRUE)
  
  basic_summ = basic_summ[with(basic_summ, order(year, month, date, hour, minute)), ]
  
  #Creating the trdaing day variables to account for hours between 2 trading day (normal or weekends)
  df$tradingday <- 0
  coun <- 0
  for (y in 1:length(basic_summ$year)){
    if(basic_summ$hour[y]==9 & basic_summ$minute[y]==30 & basic_summ$baseline[y]!=0){
      coun <- coun+1
    }
    basic_summ$tradingday[y] <- coun
  }
  print("agg")
  non_trad <- basic_summ[basic_summ$markettrading==0,]
  #summarize not trading hours by the trading day
  trad <- non_trad %>% group_by(tradingday) %>% summarise_at(vars(-date), mean)
  tradd <- non_trad %>% group_by(tradingday) %>% summarise_at(vars(date), max)
  trad <- merge.data.frame(trad,tradd,by = 'tradingday')
  trad$minute=15
  trad$hour=9
  trad$month = ceiling(trad$month)
  #created a seperate df from non trading hours and now merging them back
  basic_summ = basic_summ[basic_summ$markettrading!=0,]
  basic_summ = rbind.data.frame(basic_summ,trad)
  basic_summ = basic_summ[with(basic_summ, order(year, month, date, hour, minute)), ]
  
  #recaluclating the win lose values, in case of rows that had to be skipped.
  for (y in 1:length(basic_summ$year)){
    basic_summ$win_lose[y] <- ifelse(basic_summ$return[y+1]>basic_summ$baseline[y+1],1,0)
  }
  print("saving")
  setwd("agg data")
  y = str_remove(x,".RDS")
  write.csv(basic_summ,paste0(y,".csv"))
  setwd("..")
  
})

#Checking unqique tweets for all the comapanies.
setwd("C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Training Data/updated")

lt <- list.files(path=getwd(),pattern='*.RDS')
for (x in lt){
  df <- readRDS(x)
  cnt <- data.frame(df %>% group_by(status_id) %>% summarize(count=n()))
  print(paste0("For ",x," the number of tweets are",length(df$status_id),"",length(cnt$status_id)))
}

