goodness.of.classification = function(TP,TN,FP,FN)
{
  N = TP+TN+FP+FN
  sensitivity = TP/(TP+FN)
  specificity = TN/(TN+FP)
  J.Index = sensitivity+specificity-1
  Precision = TP/(TP+FP)
  NPV = TN/(TN+FN)
  Accuracy = (TP+TN)/(N)
  F1.Score = (2*TP)/(2*TP+FP+FN)
  Expected.Accuracy = (((TP+FN)*(TP+FP)/N)+((FP+TN)*(TN+FN)/N))/(N)
  Kappa = (Accuracy-Expected.Accuracy)/(1-Expected.Accuracy)
  Measure = c("Sensitivity","Specificity","J.Index","Precision","NPV", "Accuracy","F1.Score","Kappa")
  Value = c(sensitivity,specificity,J.Index,Precision,NPV, Accuracy,F1.Score,Kappa)
  Value = round(Value,3)
  out = data.frame(Measure,Value)
  return(out)
}