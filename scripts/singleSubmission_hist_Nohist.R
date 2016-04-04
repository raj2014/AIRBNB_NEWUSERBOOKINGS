#Single submissions History +NO History

#History version of 

agg_pred<-read.csv(file="hist/xgboost_k4.csv",header=TRUE)
#number of entries to be choosen
k<-5
ids <- NULL
for (i in 1:nrow(agg_pred)) {
  idx <- as.character(agg_pred[i,1])
  ids <- append(ids, rep(idx,k))
}
predictions_top <- as.vector(apply(agg_pred[,-1], 1, function(x) names(sort(x)[12:(12-k+1)])))

submission_hist <- NULL
submission_hist$id <- ids
submission_hist$country <- predictions_top

submission_hist <- as.data.frame(submission_hist)

#featureEngineering on History Log
library(readr)
sessions = read_csv("sessions.csv")
uniqueUsers<-unique(sessions$user_id)

# load data
df_test = read_csv("test_users.csv")

#code to get the necessary test ids(hist+No history)
hist_testids<-df_test$id[which(df_test$id %in% uniqueUsers)]
unhist_testids<-df_test$id[-which(df_test$id %in% uniqueUsers)]

df_test<-df_test[-which(df_test$id %in% uniqueUsers),]



#Taking care of nohistory users

noHist_pred<-read.csv(file="NoiseClassifiers/xgboost_e1.csv",header=TRUE)
filterrows<-which(noHist_pred$id %in% unhist_testids)
noHist_pred<-noHist_pred[filterrows,]



k<-5
ids <- NULL
for (i in 1:nrow(noHist_pred)) {
  idx <- as.character(noHist_pred[i,1])
  ids <- append(ids, rep(idx,k))
}
predictions_top <- as.vector(apply(noHist_pred[,-1], 1, function(x) names(sort(x)[12:(12-k+1)])))

# table(predictions_top[c(5*c(0:427)+1)])

# twist<-c(5*c(0:427)+1)
# for (i in twist)
# {
#   if (predictions_top[i]=="US")
#   {
#     predictions_top[i]="NDF"
#     for(temp in (i+1):(i+4))
#     {
#       predictions_top[temp]="US"
#     }
#   }
# }


submission_nohist <- NULL
submission_nohist$id <- ids
submission_nohist$country <- predictions_top

# generate submission file
submission_nohist <- as.data.frame(submission_nohist)

submission<-rbind(submission_hist,submission_nohist)
write.csv(submission, "outputs/xgboost_k4.csv", quote=FALSE, row.names = FALSE)
rm(sessions)