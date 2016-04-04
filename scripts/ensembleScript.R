#combiner of predictions
rm(list=ls())
files<-list.files(path="ensemble",full.names = TRUE)
print (files)
agg_pred<-NULL
weights<-c(0.5,0.5)
i=1
for (f in files)
{
  pred<-read.csv(file=f,header=TRUE)
  print (nrow(pred))
  print (ncol(pred))
  print (f)
  if (i==1)
  {
    agg_pred<-weights[i]*pred[,-1]
  }
  else
  {
    #agg_pred<-agg_pred+(1/length(files))*pred[,-1]
    agg_pred<-agg_pred+weights[i]*pred[,-1]
  }
  i=i+1
}
agg_pred<-cbind(pred[,1],agg_pred)

#preparing dataframe for submission
#number of entries to be choosen
k<-5
ids <- NULL
for (i in 1:nrow(agg_pred)) {
  idx <- as.character(agg_pred[i,1])
  ids <- append(ids, rep(idx,k))
}
predictions_top <- as.vector(apply(agg_pred[,-1], 1, function(x) names(sort(x)[12:(12-k+1)])))

submission <- NULL
submission$id <- ids
submission$country <- predictions_top

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "outputs/submission_w2.csv", quote=FALSE, row.names = FALSE)