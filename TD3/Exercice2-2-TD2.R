#-----Creation jeu de donnee
#Comme ils sont gaussiens, ils sont independant les uns des autres
K=2
p=3
p1=0.5
p2=0.5
u1=c(0,0,0)
u2=c(0,0,0)

n_train=30
n_test=10000

n1 <-rbinom(1,n_test,p1)
n2 <- n_test-n1
x1 <- mvrnorm(n1, mu = c(0,0,0), Sigma = diag(c(1,1,1)))
x2 <- mvrnorm(n2, mu = c(1,1,1), Sigma = diag(c(0.8,0.8,0.8)))
#cbind concatenation
x1<-cbind(x1,y= 0)
x2<-cbind(x2,y= 1)
x1
#rbind met l'un a la suite de l'autre
x<-rbind(x1,x2)
head(x)
summary(x)

#BOUCLE
x_test = x
Ns = c(30,100,1000,10000)
err.lda<- c()
err.qda <- c()
for (n in Ns){
  n1 <- rbinom(1,n,p1)
  n2 <- n-n1
  x1 <- mvrnorm(n1, mu = c(0,0,0), Sigma = diag(c(1,1,1)))
  x2 <- mvrnorm(n2, mu = c(1,1,1), Sigma = diag(c(0.8,0.8,0.8)))
  x1 <- cbind(x1,y= 0)
  x2 <- cbind(x2,y= 1)
  x3 <- data.frame(rbind(x1,x2))
  #LDA
  lda.m <- lda(y~.,data=x3)
  pred <- predict(lda.m, newdata = x_test)
  err.lda=c(err.lda, mean(pred$class!=x_test$y))
  #QDA
  qda.m <- qda(y~.,data=x3)
  pred2 <- predict(qda.m, newdata = x_test)
  err.qda=c(err.qda, mean(pred2$class!=x_test$y))       
}

#lda semble bien fonctionner avec peu de donner 
#lda peu de parametre a estimer

plot(Ns, err.lda, type = "l")

#QDA
#beaucoup de parametre