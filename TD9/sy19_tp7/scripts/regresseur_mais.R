regresseur_mais <- function(dataset) {
  # Chargement de l environnement
  load("envMais.Rdata")
  
  library(kernlab)
  library(MASS)
  library(e1071)
  predictions <- predict(svmfit, newdata = dataset)
  return(predictions)
}