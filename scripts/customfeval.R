#cross validation function
#custom evaluation metric
source("scripts/NCDGCalculation.R")
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  labels<-c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')[labels+1]
  predictions <- as.data.frame(t(matrix(preds, nrow=12)))
  colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
  predictions_top5 <- matrix(as.vector(apply(predictions, 1, function(x) names(sort(x)[12:8]))),nrow=length(labels), byrow= TRUE)
  score <- score_predictions( predictions_top5, labels)
  err <- mean(score)
  return(list(metric = "NCDG error", value = err))
}