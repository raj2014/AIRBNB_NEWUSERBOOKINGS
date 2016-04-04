#hist #Nohist_Probability frame
#Single submissions History +NO History

#History version of 

agg_pred<-read_csv(file="hist/xgboost_w3.csv")
#number of entries to be choosen

#featureEngineering on History Log
library(readr)
sessions = read_csv("sessions.csv")
uniqueUsers<-unique(sessions$user_id)
rm(sessions)
# load data
df_test = read_csv("test_users.csv")

#code to get the necessary test ids(hist+No history)
hist_testids<-df_test$id[which(df_test$id %in% uniqueUsers)]
unhist_testids<-df_test$id[-which(df_test$id %in% uniqueUsers)]

df_test<-df_test[-which(df_test$id %in% uniqueUsers),]



#Taking care of nohistory users

noHist_pred<-read_csv(file="NoiseClassifiers/xgboost_e1.csv")
filterrows<-which(noHist_pred$id %in% unhist_testids)
noHist_pred<-noHist_pred[filterrows,]
agg_pred<-rbind(agg_pred,noHist_pred)
agg_pred$id<-as.character(agg_pred$id)
agg_pred<-agg_pred[order(agg_pred$id),]
write.csv(agg_pred, "ensemble/w_3.csv", quote=FALSE, row.names = FALSE)