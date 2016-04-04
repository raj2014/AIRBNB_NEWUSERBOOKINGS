#All Clear for Objects
rm(list=ls())
#featureEngineering on History Log
library(readr)
library(entropy)
sessions = read_csv("sessions.csv")
sessions$secs_elapsed[which(is.na(sessions$secs_elapsed))]<-0
library(plyr)
# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
source('scripts/air_definitions.R')
source('scripts/customfeval.R')
set.seed(1)
actions<-sort(unique(sessions$action))
actions<-actions[actions!=""]
devices<-sort(unique(sessions$device_type))
action_types<-sort(unique(sessions$action_type))
action_types<-action_types[action_types!=""]

#names(actions)<-actions

uniqueID<-sort(unique(sessions$user_id))
splits<-split(uniqueID,ceiling(1:length(uniqueID)/10000))
str(splits)

for (i in 1:length(splits))
{
      sessions_features<-ddply(sessions[which(sessions$user_id %in% splits[[i]]),],c('user_id'),
                          function(x) 
                            c(action_count=length(x$action),
                              unique_action_count=length(unique(x$action)),
                              unique_actiontype_count=length(unique(x$action_type)),
                              unique_action_detail=length(unique(x$action_detail)),
                              unique_device_count=length(unique(x$device_type)),
                              devices_=generateDevices(x),
                              actions_=generateActions(x),
                              actionTypes_=generateActionTypes(x),
                              total_time=sum(x$secs_elapsed)/10000,
                              max_time=max(x$secs_elapsed),
                              mean_time=mean(x$secs_elapsed),
                              bins_time=generatebins(x)
                              
                                        
                          ),.progress = "text"
      )
      
      write.csv(file=paste("temp/sessions_temp_",i,".csv"),sessions_features,row.names = FALSE)
      print(paste(i*100/length(splits)," % done"))

}

#removing sessions file
rm(sessions)

for (i in (1:length(splits)))
{
  temp<-read.csv(paste("temp/sessions_temp_",i,".csv"))
  if (i==1)
  {
    sessions_features<-temp
    
  }
  else
  {
    print (i)
    colnames(temp)<-colnames(sessions_features)
    sessions_features<-rbind(sessions_features,temp)
  }
  
}
rm(temp)
write.csv(file="sessions_features.csv",sessions_features,row.names=FALSE)

#sessions_features<-read_csv("sessions_features.csv")
# load data
df_train = read_csv("train_users_2.csv")
df_test = read_csv("test_users.csv")
#filtering train and test
f<-which(df_train$id %in% sessions_features$user_id)
df_train<-df_train[f,]

#code to get the necessary test ids(hist+No history)
hist_testids<-df_test$id[which(df_test$id %in% sessions_features$user_id)]
unhist_testids<-df_test$id[-which(df_test$id %in% sessions_features$user_id)]

#labels = df_train['country_destination']
#df_train = df_train[,-c(16)]

df_test$country_destination<-c("NDF")

# combine train and test data
df_all = rbind(df_train,df_test)
# remove date_first_booking
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]

#check for the NA columns
cat("NA columns are ...")
names(which(sapply(df_all, function(x)(any(is.na(x))))==TRUE))


# replace missing values
df_all[is.na(df_all)] <- -1

# split date_account_created in year, month and day
dac = as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] = as.character(dac[,1])
df_all['dac_month'] = as.numeric(dac[,2])
df_all['dac_day'] = as.numeric(dac[,3])
date_account_created<-as.Date(df_all$date_account_created)
df_all['week_ID']<-as.numeric(format(as.POSIXct(date_account_created), "%U"))
df_all['accountdayType']<-as.factor(weekdays(date_account_created))
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]
df_all$dac_year<-as.numeric(df_all$dac_year)

#k<-which(df_all$dac_year==2014)
#k<-k[k<=nrow(df_train)]

labels<-unlist(labels)

#labels<-labels[k]
#df_all=df_all[which(df_all$dac_year==2014),]


# split timestamp_first_active in year, month and day
df_all['tfa_year'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 1, 4))
df_all['tfa_month'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 5, 6))
df_all['tfa_day'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 7, 8))

#as.Date(strDates, "%m/%d/%Y")
activeDates<-as.Date(substring(as.character(df_all[,'timestamp_first_active']), 1, 8),"%Y%m%d")
df_all['activetime_day']<-as.factor(weekdays(activeDates))

df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]



# clean Age by removing values
df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1

# one-hot-encoding features
ohe_feats = c('gender', 'accountdayType','activetime_day','signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender +accountdayType+activetime_day+signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

#removing the old datasets for 
rm(df_all)
rm(df_all_ohe)

colnames(df_all_combined)[1]<-"user_id"
df_all_combined<-merge(df_all_combined,sessions_features,by="user_id")

# split train and test
X_train = df_all_combined[df_all_combined$user_id %in% df_train$id,]
#X_train = df_all_combined[k,]
y <- recode(X_train$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$user_id %in% df_test$id,]

#names(which(sapply(df_all, function(x)(any(is.na(x))))==TRUE))

rm(df_all_combined)

trainIndex<-generatefolds(y,0.5,10)
k<-as.vector(trainIndex[[1]])

dval<-xgb.DMatrix(data=data.matrix(X_train[-k,-c(1,3)]),label=y[-k])
dtrain<-xgb.DMatrix(data=data.matrix(X_train[k,-c(1,3)]),label=y[k])
watchlist<-list(val=dval,train=dtrain)
param <- list("objective" = "multi:softprob",
              #"eval_metric" = "mlogloss",
              "booster" = "gbtree",
              "eta"=0.01,
              "max.depth"=5,
              "nthread" = -1,
              "num_class" = 12,
              "min_child_weight"=1,
              "colsample_bytree"=0.8,
              "subsample"=0.8
)

xgb<-xgb.train(   params              = param, 
                  data                = dtrain, 
                  nrounds             = 800, #1500, 
                  verbose             = 1,  #1
                  #early.stop.round    = 20,
                  feval=evalerror,
                  print.every.n = 5,
                  watchlist           = watchlist,
                  maximize            = TRUE
)




# xgb.cv(params=param,nrounds=1500,data=data.matrix(X_train[,-1]),nfold=12,label=y,
#        metrics={"mlogloss"},
#        verbose=TRUE,
#        showsd=FALSE,
#        print.every.n = 20,
#        maximize=TRUE)

# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test[,-c(1,3)])) #ntreelimit=500

# extract the 5 classes with highest probabilities
predictions <- as.data.frame(t(matrix(y_pred, nrow=12)))
colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')

#writing out the prediction set
id<-X_test$user_id
predictions<-cbind(id,predictions)
predictions<-predictions[order(id),]
write.csv(predictions, "hist/xgboost_w3.csv", quote=FALSE, row.names = FALSE)
