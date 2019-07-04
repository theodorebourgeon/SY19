
classifieur <- function(dataset) {
  # Chargement de l’environnement
  library(e1071)
  load("env.Rdata")
  return(predict(naiveBaye, newdata = dataset))
}


regresseur <- function(dataset) {
  # Chargement de l’environnement
  load("env.Rdata")
  return(predict(linearModel, newdata = dataset))
}

