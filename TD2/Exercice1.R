#Donnée
prostate <- read.table("Desktop/Mes documents/UTC/GI04/SY19/TD/TD1/prostate.data", header=TRUE)

#Résumé
summary(prostate)
plot(prostate)

#Q1
#Regression lineaire (variable a expliquer (=lpsa), avec quoi l'expliquer (separe par des + ou des -))
# . Tous les predicteurs
#On enleve train qui n'apporte pas de valeurs
reg <- lm(formula =lpsa ~ .-train, data=prostate)
reg
#Pas parce que les coeff sont petit que c'est pas significatifs, ca donne une idee
summary(reg)
#Estimation des coefficiant Bhat (suit une loi normale) // Std. Error = Variance des Bhat // t-value = test de student sous l'hypoyhese Ho (nullite) // p-value
plot (reg)
#Les coefficiants significativement non nuls sont : lcavol, lweight, svi, age
#p-value tres petite --> Hypothese Ho(=le coefficiant est nul) est rejete --> tres significatif, on arrive a expliquer des choses

#Q2
#Intervalle de confiance sur les coefficiants = confint
IC <- confint(reg, level = 0.99)
IC
#Si l'intervalle contient 0 (95% des valeurs), Ho peut potentiellement etre valide
#Pourquoi 0 ??? Car les variables sont une correlation entre lpsa et d'autres facteurs, si =0 alors il n'y a pas lieu de comparer ces deux données, aucune correspondance

#Q3
#Valeur predite 
#prediction <- predict(reg, prostate=[prostate$train==FALSE, ], interval = "predict")
#prediction
#prediction deja contenue dans la regression !!!! On ne va pas repredire sur ce qu'on a deja predit

reg$fitted.values 

#Q4
#residual plot 
plot(prostate$lpsa, reg$fitted.values, asp=1)
#asp = ration x/y
abline(0,1)
#droite y=x

#On essaye souvent de valider le modele suite a cette question
#On aimerait bien que les residus verifie la meme chose, qu'ils soient decorrele, le bruit ne depend pas du modele (c'est le modele qui veut ca)
#D'ou l'etude des residu en focntions des varibales (on espere qu'ils sont aleatoires)
plot(prostate$lpsa, reg$residuals)
#Aucun probleme apparant, probablement decorele
plot(prostate$lweight, reg$residuals)
#lorsqu'il y a un profil en entonnoire ou courbé c'est pas bon 

#Q5
#Verifier la normalite des residus 
#Moyenne empririque=0 ??
sum(reg$residuals/length(reg$residuals))
#Variance constante ??

#Suit une loie normale ? shapiro-wilk 
plot(prostate$lpsa, rstandard(reg))
shapiro.test(rstandard(reg))
#Pas de presomption contre l'hypothese nulle

#Q6
plot(reg)
#dernier graphique donnée aberantes et leur poid + distance de cook (ici on ne la voit pas car aucune donnee n'est vraiment aberante)
cooks.distance(reg)

#Q7
reg <- lm(formula =lpsa ~ lcavol+lweight+svi+age-train, data=prostate)
summary(reg)
plot(reg)
#la p-value est nettement diminué
#c'est du au fait que des variables sont correlés
#correlation
cor(prostate)
#si decorellé --> matrice diagonale

#Q8 
#Quand modele lineaire est mauvais, on peut esayer de changer les prediteurs (si bruit entonnoire --> transformation log, si parabole --> transformation puissance) pour reequilibrer la distribution 

#Q9
train=prostate[prostate$train==TRUE, ]
test=prostate[prostate$train==FALSE, ]

reg <- lm(formula =lpsa ~ .-train, data=train)
summary(reg)

prediction <- predict(reg, newdata=test, interval = "prediction")
prediction

plot(prediction[, "fit"])
lines(prediction[, "fit"])
lines(prediction[, "lwr"])
