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

set.seed(12345)

library(randomForest)
library(caret)
library(class)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(xgboost)

model_list <- list.files(path=model_path, pattern="*.RDS", full.names=TRUE, recursive=FALSE)

i<-0

for (model in model_list){
  model<-model_list[1]
  
  model1<-readRDS(model)
  name1<-sub("*.RDS","",sub("/","",sub(model_path, "", model)))
  names<-strsplit(name1,"-")
  
  
  name<-names[[1]][2]
  mod_type<-names[[1]][1]
  outfile<-paste(output_path,name,".csv",sep="")
  
  
  testdata<-read.csv(paste(test_agg_path,name,".csv", sep=""))
  
  #If logistic regression is best:
  
  if (mod_type=="log"){
    
    testdata$prob<-predict(model1,testdata, type="response")
    testdata$pred<-ifelse(testdata$prob>.5,1,0)
    
    cm<-table(testdata$pred,testdata$win_lose)
    
    accuracy<-ifelse(nrow(cm)==2,(cm[1,1]+cm[2,2])/sum(cm),(cm[1,1])/sum(cm))
    
    #
    
    print(testdata$prob)
    print(testdata$pred)
    
    cutoff <- seq(0, 1, length = 100)
    fpr <- numeric(100)  # Creates a vector of 100 0's
    tpr <- numeric(100)
    
    ## We'll collect it in a data frame.  
    roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
    ## At this point we just have a column for cutoffs and two columns of 0's
    
    ## TPR is the Sensitivity; FPR is 1-Specificity. We enter these numbers in each row.
    for (i in 1:100) {
      roc.table$FPR[i] <- sum(testdata$prob > cutoff[i] & testdata$win_lose == 0)/sum(testdata$win_lose == 0)
      roc.table$TPR[i] <- sum(testdata$prob > cutoff[i] & testdata$win_lose == 1)/sum(testdata$win_lose == 1)
    }
    
    
    plot(TPR ~ FPR, data = roc.table, type = "s",xlab="1 - Specificity",ylab="Sensitivity",col="blue",main= "ROC Curves")
    
    probb<-cbind(testdata$prob,testdata$year,testdata$month, testdata$date, testdata$hour, testdata$minute)
    
    
  }
  
  #If Knn was best:
  
  if (mod_type=="k"){
    
    #  str(df)
    
    df <- read.csv(paste(train_agg_path,name,".csv",sep=""))
    df[,1] <- NULL
    df$verified <- NULL
    df <- na.omit(df)
    
    
    testdata[,1] <- NULL
    testdata$verified <- NULL
    testdata <- na.omit(testdata)

    
    dftrain<-df
    #
    winnum<-which(colnames(dftrain)=="win_lose" )
    train_input <- as.matrix(dftrain[,-winnum])
    train_output <- as.vector(dftrain[,winnum])
    
    winnum<-which(colnames(test_data)=="win_lose" )
    test_input <- as.matrix(dftest[,-winnum])
    #
    z <- model
    
    knn_prob <- knn(train_input, test_input,train_output, k=z, prob = TRUE)
    prob <- attr(knn_prob,"prob")
    pred <- knn(train_input, test_input,train_output, k=z)
    
    CM <- table(prediction, testdata$win_lose)
    
    accuracy<-(CM[1,1]+CM[2,2])/sum(CM)
    
    print(prob)
    print(pred)
    
    probb<-cbind(dftest$prob,dftest$year,dftest$month, dftest$date, dftest$hour, dftest$minute)
    
  }
  
  #If RF was best:
  
  if (mod_type=="RF"){
    
    testdata$sentiment_x_favorite_count<-testdata$sentiment*testdata$favorite_count
    testdata$sentiment_x_retweet_count<-testdata$sentiment*testdata$retweet_count
    testdata$sentiment_x_media_url<-testdata$sentiment*testdata$media_url
    testdata$sentiment_x_followers_count<-testdata$sentiment*testdata$followers_count
    testdata$sentiment_x_verified<-testdata$sentiment*testdata$verified
    
    testdata[is.na(testdata)]<-0
    
    #get probablity and classification prediction
    
    testdata$pred<-predict(model1,newdata=testdata)
    testdata$prob<-predict(model1,newdata=testdata,type = "prob")[,2] 
    
    probb<-cbind(testdata$prob,testdata$year,testdata$month, testdata$date, testdata$hour, testdata$minute)
    
    cm<-table(testdata$pred,testdata$win_lose)
    accuracy<-(cm[1,1]+cm[2,2])/sum(cm)
    print(testdata$pred)
    print(testdata$prob)
    
    cutoff <- seq(0, 1, length = 100)
    fpr <- numeric(100)  # Creates a vector of 100 0's
    tpr <- numeric(100)
    
    ## We'll collect it in a data frame.
    roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
    # ## At this point we just have a column for cutoffs and two columns of 0's
    #
    # ## TPR is the Sensitivity; FPR is 1-Specificity. We enter these numbers in each row.
    for (i in 1:100) {
      roc.table$FPR[i] <- sum(testdata$prob > cutoff[i] & testdata$win_lose == 0)/sum(testdata$win_lose == 0)
      roc.table$TPR[i] <- sum(testdata$prob > cutoff[i] & testdata$win_lose == 1)/sum(testdata$win_lose == 1)
    }
    #
    #
    plot(TPR ~ FPR, data = roc.table, type = "s",xlab="1 - Specificity",ylab="Sensitivity",col="blue",main= paste(name,"Random Forest Test ROC Curves"))
    #
    #
    
    
    ## lift charts
    df1 <- data.frame(testdata$prob,testdata$win_lose)
    colnames(df1) <- c("predicted_probability","actual")
    df1S <- df1[order(-testdata$prob),]
    df1S$Gains <- cumsum(df1S$actual)
    plot(df1S$Gains,type="n",main=paste(name,"Random Forest Lift Chart"),xlab="Number of Cases",ylab="Cumulative Success")
    lines(df1S$Gains)
    abline(0,sum(df1S$actual)/nrow(df1S),lty = 2, col="red")
    
  }
  
  ## If XGBoost is best
  
  if (mod_type=="XGB_un"){
    
    df_valid<-testdata
    df_valid = df_valid[-nrow(df_valid),]
    df_valid[is.na(df_valid)]<-0
    
    #using one hot encoding 
    
    test_data= setDT(df_valid)
    test_data[,verified := NULL]
    
    ## 
    new_ts <- model.matrix(~.+0,data = test_data[,-c("win_lose"),with=F])
    
    ts_label <- as.numeric(test_data$win_lose)
    ts_label <- as.numeric(ts_label)
    
    #preparing matrix 
    dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
    
    test_data$prob <- predict(model1,dtest)
    test_data$pred <- ifelse(test_data$prob > 0.5,1,0)
    print(test_data$pred)
    print(test_data$prob)
    
    probb<-cbind(test_data$prob,test_data$year,test_data$month, test_data$date, test_data$hour, test_data$minute)
    
    cutoff <- seq(0, 1, length = 100)
    fpr <- numeric(100)  # Creates a vector of 100 0's
    tpr <- numeric(100)
    
    ## We'll collect it in a data frame.
    roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
    # ## At this point we just have a column for cutoffs and two columns of 0's
    #
    # ## TPR is the Sensitivity; FPR is 1-Specificity. We enter these numbers in each row.
    for (i in 1:100) {
      roc.table$FPR[i] <- sum(test_data$prob > cutoff[i] & test_data$win_lose == 0)/sum(test_data$win_lose == 0)
      roc.table$TPR[i] <- sum(test_data$prob > cutoff[i] & test_data$win_lose == 1)/sum(test_data$win_lose == 1)
    }
    #
    #
    plot(TPR ~ FPR, data = roc.table, type = "s",xlab="1 - Specificity",ylab="Sensitivity",col="blue",main= paste(name,"XGBoost Test ROC Curves"))
    #
    #
    
    
    ## lift charts
    df1 <- data.frame(test_data$prob,test_data$win_lose)
    colnames(df1) <- c("predicted_probability","actual")
    df1S <- df1[order(-test_data$prob),]
    df1S$Gains <- cumsum(df1S$actual)
    plot(df1S$Gains,type="n",main=paste(name,"XGBoost Lift Chart"),xlab="Number of Cases",ylab="Cumulative Success")
    lines(df1S$Gains)
    abline(0,sum(df1S$actual)/nrow(df1S),lty = 2, col="red")
    
  }
  
  probb<-as.data.frame(probb)
  colnames(probb)<-c(paste(name,"_prob",sep=""),"test_data$year","test_data$month", "test_data$date", "test_data$hour", "test_data$minute")
  
  write.csv(probb, outfile)
  
  print(name)
  print(mod_type)
  print(cm)
  print(accuracy)
}