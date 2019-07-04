classifieur_astronomie <- function(dataset) {
# Chargement de l???environnement
load("envAstro.Rdata")

library(kernlab)
library(MASS)
library(e1071)
  
predictions <- predict(svm_astro, newdata = dataset)
return(predictions)
}


classifieur_ble <- function(dataset) {
  # Chargement de l environnement
  load("envMais.Rdata")
  
  library(kernlab)
  library(MASS)
  library(e1071)
  
  predictions <- predict(svmfit, newdata = dataset)
  return(predictions)
}


classifieur_images <- function(dataset) {
  # Chargement de l???environnement
  load("env.Rdata")
  
  return(predictions)
}