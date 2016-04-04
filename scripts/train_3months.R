#Working on the months for only user only profile data

#All Clear for Objects
rm(list=ls())
#featureEngineering on History Log
library(readr)
library(entropy)
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

#names(actions)<-actions


# load data
df_train = read_csv("train_users_2.csv")
df_test = read_csv("test_users.csv")



#Dummy variable in the test ,to make rbind easy
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


date_account_created<-as.Date(df_all$date_account_created,format="%Y-%m-%d")
# split date_account_created in year, month and day
df_all['dac_year'] = as.numeric(format(date_account_created, format="%Y"))
df_all['dac_month'] = as.numeric(format(date_account_created, format="%m"))
df_all['dac_day'] = as.numeric(format(date_account_created, format="%d"))

#df_all['week_ID']<-as.numeric(format(as.POSIXct(date_account_created), "%U"))

weekdays1<-c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday','Saturday','Sunday')
df_all['accountdayType']<-as.integer(factor(weekdays(date_account_created),levels=weekdays1))
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]


#k<-which(df_all$dac_year==2014)
#k<-k[k<=nrow(df_train)]

# labels<-unlist(labels)
# labels<-labels[k]

#Already filtered for 2014,no worries
#df_all=df_all[which(df_all$dac_year==2014),]


# split timestamp_first_active in year, month and day
df_all['tfa_year'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 1, 4))
df_all['tfa_month'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 5, 6))
df_all['tfa_day'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 7, 8))

#as.Date(strDates, "%m/%d/%Y")
activeDates<-as.Date(substring(as.character(df_all[,'timestamp_first_active']), 1, 8),"%Y%m%d")
df_all['activetime_day']<-as.integer(factor(weekdays(activeDates),levels=weekdays1))
df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]


#Removing the temporary variables created
rm(activeDates)
rm(date_account_created)



# clean Age by removing values
df_all[df_all$age < 14 | df_all$age > 100,'age'] <- 120

# one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender +signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

#removing the old datasets to overcome memory constraints
rm(df_all)
rm(df_all_ohe)

colnames(df_all_combined)[1]<-"user_id"


# split train and test
X_train = df_all_combined[df_all_combined$user_id %in% df_train$id,]
#X_train = df_all_combined[k,]
y <- recode(X_train$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$user_id %in% df_test$id,]




# Only the months on and after July for each year 
X_train_v2 = df_all_combined[(df_all_combined$user_id %in% df_train$id) & (df_all_combined$tfa_month>=7 & df_all_combined$tfa_month<=9 ),]
y <- recode(X_train_v2$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$user_id %in% df_test$id,]


trainIndex<-generatefolds(y,0.7,10)
k<-as.vector(trainIndex[[1]])

dval<-xgb.DMatrix(data=data.matrix(X_train_v2[-k,-c(1,3)]),label=y[-k])
dtrain<-xgb.DMatrix(data=data.matrix(X_train_v2[k,-c(1,3)]),label=y[k])
watchlist<-list(val=dval,train=dtrain)
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "booster" = "gbtree",
              "eta"=0.025,
              "max.depth"=6,
              "nthread" = 2,
              "num_class" = 12,
              "min_child_weight"=1,
              "colsample_bytree"=0.8,
              "subsample"=0.8
)

xgb<-xgb.train(   params              = param, 
                  data                = dtrain, 
                  nrounds             = 200, #1500, 
                  verbose             = 1,  #1
                  #early.stop.round    = 20,
                  #feval=evalerror,
                  print.every.n = 5,
                  watchlist           = watchlist,
                  maximize            = TRUE
)


-----------------------------------------------------------------------------------
  
  dtrain<-xgb.DMatrix(data=data.matrix(X_train_v2[,-c(1,3)]),label=y)
  watchlist<-list(train=dtrain)

  param <- list("objective" = "multi:softprob",
                "eval_metric" = "mlogloss",
                "booster" = "gbtree",
                "eta"=0.025,
                "max.depth"=6,
                "nthread" = 3,
                "num_class" = 12,
                "min_child_weight"=1,
                "colsample_bytree"=0.6,
                "subsample"=0.6
  )
  
  set.seed(1)

xgb<-xgb.train(   params              = param, 
                  data                = dtrain, 
                  nrounds             = 100, #1500, 
                  verbose             = 1,  #1
                  #early.stop.round    = 20,
                  #feval=evalerror,
                  print.every.n = 5,
                  watchlist           = watchlist,
                  maximize            = TRUE
)

y_pred <- predict(xgb, data.matrix(X_test[,-c(1,3)])) #ntreelimit=500

# extract the 5 classes with highest probabilities
predictions <- as.data.frame(t(matrix(y_pred, nrow=12)))
colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')

#writing out the prediction set
id<-X_test$user_id
predictions<-cbind(id,predictions)
predictions<-predictions[order(id),]
write.csv(predictions, "hist/xgboost_t2.csv", quote=FALSE, row.names = FALSE)