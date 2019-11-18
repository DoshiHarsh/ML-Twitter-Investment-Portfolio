#importing the relevant packages
library(randomForest)
library(caret)
library(class)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(xgboost)

## Specifying the file paths
model_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Models/"

train_agg_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Training Data/agg data/"
valid_agg_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Validation Data/agg data/"
test_agg_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Test Data/agg data/"

#train_unagg_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Train Data/"
#valid_unagg_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Validation Data/"
#test_unagg_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Modelling Data/Final/Final Test Data/"

output_path<-"C:/Users/doshi/OneDrive/2019 Spring Semester/Data Mining and Predictive Analysis/Project/Scripts and Output/"

setwd(model_path)

##Table to Calculate Accuracy Rates
aggregated<-matrix(nrow=16,ncol=5)
aggregated[1,1]<-"Company"
aggregated[1,2]<-"Logistic (Aggregated)"
aggregated[1,3]<-"Random Forest (Aggregated)"
aggregated[1,4]<-"KNN (Aggregated)"
aggregated[1,5]<-"XGBoost"

bestmodel<-matrix(nrow=16,ncol=2)
bestmodel[1,1]<-"Company"
bestmodel[1,2]<-"Best Model"

set.seed(12345)

#Listing all files in the directory
file_list <- list.files(path=train_agg_path, pattern="*.csv", full.names=TRUE, recursive=FALSE)

i<-0

for (file in file_list){
  name<-sub("*.csv","",sub("/","",sub(train_agg_path, "", file)))
  valid_name<-paste(valid_agg_path,name,".csv", sep="")
  i<-i+1
  aggregated[i+1,1]<-name
  
  ## Beginning Logistic Regression
  df<- read.csv(file)
  
  df$verified<- as.logical(df$verified)
  
  df$X <- NULL
  df$year <- NULL
  df$month <- NULL
  df$date <- NULL
  df$minute <- NULL
  df$verified <- NULL
  df$cpl.latestPrice <- NULL
  df$cpl.latestVolume <- NULL
  df$retweet_favorite_count <- NULL
  df$retweet_retweet_count <- NULL
  df$retweet_followers_count <- NULL
  df$retweet_friends_count <- NULL
  df$retweet_verified <- NULL
  df$tradingday<- NULL
  df[is.na(df)]<-0
  df$favorite_count<-df$sentiment*df$favorite_count
  df$retweet_count<-df$sentiment*df$retweet_count
  df$media_url<-df$sentiment*df$media_url
  df$followers_count<-df$sentiment*df$followers_count
  
  log1<-glm(win_lose~., data=df, family = "binomial",maxit = 100)
  
  df_valid<-read.csv(valid_name)
  
  df_valid$yhat<-predict(log1,df_valid, type="response")
  df_valid$pred<-ifelse(df_valid$yhat>.5,1,0)
  
  cm<-table(df_valid$pred,df_valid$win_lose)
  
  accuracy<-ifelse(nrow(cm)==2,(cm[1,1]+cm[2,2])/sum(cm),(cm[1,1])/sum(cm))
  
  aggregated[i+1,2]<-accuracy
  
  
  ##Beginning RF
  
  df<-read.csv(file)
  df_valid<-read.csv(valid_name)
  
  df$verified<- as.logical(df$verified)
  
  df$sentiment_x_favorite_count<-df$sentiment*df$favorite_count
  df$sentiment_x_retweet_count<-df$sentiment*df$retweet_count
  df$sentiment_x_media_url<-df$sentiment*df$media_url
  df$sentiment_x_followers_count<-df$sentiment*df$followers_count
  df$sentiment_x_verified<-df$sentiment*df$verified
  
  df_valid$sentiment_x_favorite_count<-df_valid$sentiment*df_valid$favorite_count
  df_valid$sentiment_x_retweet_count<-df_valid$sentiment*df_valid$retweet_count
  df_valid$sentiment_x_media_url<-df_valid$sentiment*df_valid$media_url
  df_valid$sentiment_x_followers_count<-df_valid$sentiment*df_valid$followers_count
  df_valid$sentiment_x_verified<-df_valid$sentiment*df_valid$verified
  
  df[,c("X","display_text_width","is_retweet","account_created_at","favorite_count","retweet_count","hashtags","symbols",
        
        "urls_url","media_url","mentions_user_id","quoted_status_id","retweet_favorite_count","retweet_retweet_count",
        
        "retweet_followers_count","retweet_friends_count","retweet_verified","followers_count","friends_count"
        
        ,"statuses_count","favourites_count","verified","sentiment","std dev","markettrading","tradingday")]<-NULL
  
  df$win_lose<-as.factor(df$win_lose)
  
  df[is.na(df)]<-0
  df_valid[is.na(df_valid)]<-0
  
  rf_model <- randomForest(win_lose ~., data = df, mtry=4,ntree=1000,importance = TRUE)
  
  df_valid$rf_preds<-predict(rf_model,newdata=df_valid)  
  
  cm<-table(df_valid$rf_preds,df_valid$win_lose)
  accuracy<-(cm[1,1]+cm[2,2])/sum(cm)
  
  aggregated[i+1,3]<-accuracy
  
  ## KNN
  
  df_valid<-read.csv(valid_name)
  
  df <- read.csv(file)
  df[,1] <- NULL
  df$verified <- NULL
  df <- na.omit(df)
  #  str(df)
  
  df_valid[,1] <- NULL
  df_valid$verified <- NULL
  df_valid<-na.omit(df_valid)
  #  str(df_valid)
  
  fun <- function(x){
    a <- mean(x)
    b <- sd(x)
    (x - a)/(b)
  }
  
  inTrain <- createDataPartition(df$win_lose, p=0.7, list=FALSE)
  #
  dftrain <- data.frame(df[inTrain,])
  dftemp <- data.frame(df[-inTrain,])
  inVal <- createDataPartition(dftemp$win_lose, p=0.6, list=FALSE)
  dfvalidation <- data.frame(dftemp[inVal,])
  dftest <- data.frame(dftemp[-inVal,])
  
  #
  winnum<-which(colnames(dftrain)=="win_lose" )
  train_input <- as.matrix(dftrain[,-winnum])
  train_output <- as.vector(dftrain[,winnum])
  validate_input <- as.matrix(dfvalidation[,-winnum])
  test_input <- as.matrix(dftest[,-winnum])
  #
  kmax <- 15
  ER1 <- rep(0,kmax)
  ER2 <- rep(0,kmax)
  
  
  for (v in 1:kmax){
    prediction <- knn(train_input, train_input,train_output, k=v)
    prediction2 <- knn(train_input, validate_input,train_output, k=v)
    prediction3 <- knn(train_input, test_input,train_output, k=v)
    #
    # The confusion matrix for training data is:
    CM1 <- table(prediction, dftrain$win_lose)
    # The training error rate is:
    ER1[v] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
    # The confusion matrix for validation data is:
    CM2 <- table(prediction2, dfvalidation$win_lose)
    ER2[v] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
  }
  
  z <- which.min(ER2)
  
  winnum<-which(colnames(df_valid)=="win_lose" )
  valid_input <- as.matrix(df_valid[,-winnum])
  
  prediction3 <- knn(train_input, valid_input,train_output, k=z)
  #
  CM3 <- table(prediction3, df_valid$win_lose)
  
  accuracy<-(CM3[1,1]+CM3[2,2])/sum(CM3)
  aggregated[i+1,4]<-accuracy
  
  ##XGBoost - Aggregated data
  
  df <- read.csv(file)
  df = df[-nrow(df),]
  df[is.na(df)]<-0
  
  df_valid<-read.csv(valid_name)
  df_valid = df[-nrow(df_valid),]
  df_valid[is.na(df_valid)]<-0
  
  #using one hot encoding 
  
  train_data= setDT(df)
  train_data[,verified := NULL]
  
  test_data= setDT(df_valid)
  test_data[,verified := NULL]
  
  
  ## partition
  
  new_tr <- model.matrix(~.+0,data = train_data[,-c("win_lose")],with = F) 
  new_ts <- model.matrix(~.+0,data = test_data[,-c("win_lose"),with=F])
  
  ts_label <- as.numeric(test_data$win_lose)
  labels <- as.numeric(train_data$win_lose)
  ts_label <- as.numeric(ts_label)
  
  #preparing matrix 
  dtrain <- xgb.DMatrix(data = new_tr,label = labels) 
  dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
  
  #model with default parameters
  params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, 
                 gamma=1, max_depth=6, min_child_weight=1, 
                 subsample=1, colsample_bytree=1)
  #using cross validation to find best number of iteration
  xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 1, 
                   nfold = 5, showsd = T, stratified = T, 
                   print.every.n = 10, early.stop.round = 20, maximize = F)
  
  xgbcv$best_iteration
  #2
  
  xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 1,
                     maximize = F , eval_metric = "error")  #watchlist = list(val=dtest,train=dtrain)
  
  xgbtestpred <- predict (xgb1,dtest)
  xgbtestpred <- ifelse (xgbtestpred > 0.5,1,0)
  
  
  #confusion matrix
  cm1<-table(xgbtestpred,ts_label)
  
  accuracy<-(CM1[1,1]+CM1[2,2])/sum(CM1)
  aggregated[i+1,5]<-accuracy
  
  ## XGBoost - Unaggregated data
  # 
  # df_file<-paste(train_unagg_path,name,".RDS", sep="")
  # df <- readRDS(df_file)
  # df_valid_file<-paste(valid_unagg_path,name,".RDS", sep="")
  # df_valid<-readRDS(df_valid_file)
  # 
  # # All Models must be run, and have their accuracy sent
  ## to aggregated before this point   
  bestmodel[i+1,1]<-name  
  bestnum<-which.max(aggregated[i+1,])
  bestmodel[i+1,2]<-aggregated[1,bestnum]
  
  if (bestnum==2){
    saveRDS(log1, paste("log-",name,".RDS",sep=""))
  }
  
  if (bestnum==3){
    saveRDS(rf_model, paste("RF-",name,".RDS",sep=""))
  }
  
  if (bestnum==4){
    saveRDS(z, paste("k-",name,".RDS",sep=""))
  }
  
  if (bestnum==5){
    saveRDS(xgb1, paste("XGB_un-",name,".RDS",sep=""))
  }
  ##
}
