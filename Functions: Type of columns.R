library(ggplot2)
library(corrplot)

data = read.csv('PoliceKillingsUS.csv')

#To get the Numeric columns from a Dataset
getintcols = function(x,p = "integer"){

  vec = character()
  for(i in 1:ncol(x)){
    vec[i] = class(x[,i])
  }
  if(p=="integer"){a = which(vec == "integer" | vec == "numeric")}
  if(p=="character"){a = which(vec == "character")}
  if(p=="factor"){a = which(vec == "factor")}
  if(p=="logical"){a = which(vec == "logical")}
  
  print(a)
  print(colnames(data[,a]))
}

getintcols(data,"integer")
