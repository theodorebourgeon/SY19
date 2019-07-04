data <-read.table("Desktop/Mes documents/UTC/GI04/SY19/TD/TD3/Spam-20181004/spambase.dat")
head(data)

#Sample data
#On recupere 2/3 des index et on filtre les donnees sur ces indexs
#data.train et data.test
data.index.train <- sample(nrow(data), 2*nrow(data)/3)
data.train <- data[data.index.train,]
data.train
nrow(data.train)
data.test <- data[-data.index.train,]
nrow(data.test)

#Librairy
library("MASS")

#----------------------Analyse discriminante lineaire 
#On constate que la colonne 58 contient le booleen spam ou non, c'est cette variable qu'on cherhce a predire
lda.data <- lda(formula=V58~.,data=data.train)
lda.data

#prediction sur les donnes de test en fonction du modele cree par les donnes d'entrainements
pred.data <- predict(lda.data, newdata = data.test)

#predict renvoit 3 grandeurs
#Classe predite
pred.data$class
#probabilité
pred.data$posterior
#score
pred.data$x

#matrice de confusion
perf <- table(data.test$V58,pred.data$class)

#taux d'erreur
1-sum(diag(perf))/nrow(data.test)
#err_rate = 0.1108214 

#ROC curve
library("pROC")
roc_curve<- roc(data.test$V58, as.vector(pred.data$x))
plot(roc_curve)


#----------------------Regression logistique
#Binomial logistic regression car on a que 2 classes (0 ou 1)
#Sans family, glm fait une regression lieanire classique
glm.data <- glm(V58~., data=data.train, family = binomial)

#type=link --> prediction brute
#type=response --> tasse les valeurs de predictions entre 0 et 1
glm.pred <- predict(glm.data, newdata = data.test, type='response')
glm.perf <- table(data.test$V58, glm.pred>0.5)
glm.perf
1-sum(diag(glm.perf))/nrow(data.test)
roc_curve_glm <- roc(data.test$V58, glm.pred)
plot(roc_curve_glm, add=TRUE, col='red')

#Significativite des predicteurs 
summary(glm.data)
