# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
source('scripts/air_definitions.R')
source('scripts/customfeval.R')
set.seed(1)

# load data
df_train = read_csv("train_users_2.csv")
df_test = read_csv("test_users.csv")
labels = df_train['country_destination']
df_train = df_train[,-c(16)]

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

k<-which(df_all$dac_year==2014)
k<-k[k<=nrow(df_train)]

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

# split train and test
X_train = df_all_combined[df_all_combined$id %in% df_train$id,]
#X_train = df_all_combined[k,]
y <- recode(labels,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

#names(which(sapply(df_all, function(x)(any(is.na(x))))==TRUE))

rm(df_all_combined)

trainIndex<-generatefolds(y,0.5,10)
k<-as.vector(trainIndex[[1]])

dval<-xgb.DMatrix(data=data.matrix(X_train[-k,-c(1)]),label=y[-k])
dtrain<-xgb.DMatrix(data=data.matrix(X_train[k,-c(1)]),label=y[k])
watchlist<-list(val=dval,train=dtrain)
param <- list("objective" = "multi:softprob",
              #"eval_metric" = "mlogloss",
              "booster" = "gbtree",
              "eta"=0.05,
              "max.depth"=9,
              "nthread" = -1,
              "num_class" = 12,
              "min_child_weight"=1,
              "colsample_bytree"=0.8,
              "subsample"=0.8
)

xgb<-xgb.train(   params              = param, 
             data                = dtrain, 
             nrounds             = 150, #1500, 
             verbose             = 1,  #1
             #early.stop.round    = 20,
             feval=evalerror,
             print.every.n = 1,
             watchlist           = watchlist,
             maximize            = TRUE
)




# xgb.cv(params=param,nrounds=1500,data=data.matrix(X_train[,-1]),nfold=12,label=y,
#        metrics={"mlogloss"},
#        verbose=TRUE,
#        showsd=FALSE,
#        print.every.n = 20,
#        maximize=TRUE)

cv<-1
#crossvalidation of NCDG score
if(cv==1)
{
  y_pred <- predict(xgb, data.matrix(X_train[-k,-1]))
  
  # extract the 5 classes with highest probabilities
  predictions <- as.data.frame(t(matrix(y_pred, nrow=12)))
  colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
  predictions_top5 <- matrix(as.vector(apply(predictions, 1, function(x) names(sort(x)[12:8]))),nrow=length(labels[-k]), byrow= TRUE)
  #true_obs=recode(labels[-k],"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
  true_obs<-labels[-k]
  score <- score_predictions( predictions_top5, true_obs)
  print(mean(score))
}


# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test[,-1]))

# extract the 5 classes with highest probabilities
predictions <- as.data.frame(t(matrix(y_pred, nrow=12)))
colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')

#writing out the prediction set
id<-X_test$id
predictions<-cbind(id,predictions)
predictions<-predictions[order(id),]
write.csv(predictions, "outputs/xgboost_e1.csv", quote=FALSE, row.names = FALSE)

predictions_top5 <- as.vector(apply(predictions[1:10,-1], 1, function(x) names(sort(x)[12:8])))

# create submission 
ids <- NULL
for (i in 1:NROW(X_test)) {
  idx <- X_test$id[i]
  ids <- append(ids, rep(idx,5))
}
submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "outputs/submission_v2.csv", quote=FALSE, row.names = FALSE)