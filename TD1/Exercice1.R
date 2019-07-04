data <- read.table("Desktop/Mes documents/SY19/TD/TD1/prostate.data")
#install.packages("FNN", dependencies=TRUE)
library("FNN")
#plot(data)
#cor(data)

#Selection de certaines colonne de data
data_reduced <- data[,c("lcavol", "lweight", "age", "lbph", "train")]

#Masque en fonction de la valeur train table$colonne_msque 
x_train = data[data$train, 1:4]
y_train = data[data$train, "lpsa"]
x_test = data[!data$train, 1:4]
y_test = data[!data$train, "lpsa"]

#Application de knn.reg
knn.reg(x_train, test=x_test, y=y_train)

#Erreur quadratique en fonction
err_quadra <- c()
for (i in (1 : 10)) {
  reg <- knn.reg(x_train, test=x_test, y=y_train, k=i)
  err_quadra <- rbind(err_quadra, sum((reg$pred-y_test)^2))
}

#Graphe de l'erreur quadratique 
plot(err_quadra, type="l")

#Erreur min pour k=6