# #feature engineering for history log of users
# 
# actions - 
# total count
# distinct count
# 
# actions type  
# total count
# distinct count
# 
# 
# 
# totaltimeelapsed - scale accordingly
# 
# 
# Device types
# number of distinct device types

length(which(X_train$id %in% sessions_features$user_id))

df_train = read_csv("train_users_2.csv")
k<-which(df_train$id %in% sessions_features$user_id)
table(df_train$country_destination[k])