dati <- read_csv("C:/Users/dilet/Desktop/GLM/progetti/progetto 2 Speed Dating/Speed Dating Data.csv")

#DATA CLEANSING
elimina <- c(5, 7, 8, 9, 12, 24, 33, 35, 37, 38, 39, 43, 44, 45, 49, 50, 51, 52, 53, 54, 
             55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 76, 77, 78, 79, 
             80, 81, 82, 83, 84, 85, 86, 87, 93, 94, 95, 96, 97, 107, 108, 109, 110, 111, 
             112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 
             127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142,
             143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 
             159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 
             175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 
             191, 192, 193, 194, 195)

dataset <- dati[, -elimina]
elimina2 <- c(1, 2, 4, 6, 7, 8, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 29,
              28, 33, 34, 35, 36, 37, 38, 39, 40)
dataset <- dataset[, -elimina2]
dataset_nona <- na.omit(dataset)

self <- c()
for (i in 1:nrow(dataset_nona)) {
  self[i] <- sum(dataset_nona[i, 10], dataset_nona[i, 11], dataset_nona[i, 12], 
                  dataset_nona[i, 13], dataset_nona[i, 14])/5
}
self

newdata <- dataset_nona[, -c(10:14)]
newdata <- cbind(newdata, self)

#FINDING THE BEST MODEL

mod1 <- glm(newdata$dec~newdata$gender+newdata$int_corr+newdata$samerace+newdata$age_o+
              newdata$age+newdata$imprelig+newdata$attr + newdata$sinc+ newdata$intel+
              newdata$fun + newdata$amb + newdata$shar + newdata$prob + newdata$self, 
            family = binomial)

summary(mod1)

diff_age <- newdata$age-newdata$age_o
mod2 <- glm(newdata$dec~newdata$gender+newdata$int_corr+newdata$samerace+ diff_age
              +newdata$imprelig+newdata$attr + newdata$sinc+ newdata$intel+
              newdata$fun + newdata$amb + newdata$shar + newdata$prob + newdata$self,
            family = binomial)
# mod2 with diff age
summary(mod2)

mod3 <- glm(newdata$dec~newdata$gender+newdata$int_corr+newdata$samerace+ diff_age
            +newdata$imprelig+newdata$like + newdata$prob + newdata$self, family = binomial)
summary(mod3)
# mod3 con like e non i sei attributi


goalf<-as.factor(newdata$goal)
goalf

newdata<-as.data.frame(cbind(newdata, goalf))

mod4 <- glm(newdata$dec~newdata$gender+newdata$int_corr+newdata$samerace+ diff_age
            +newdata$imprelig+newdata$like + newdata$prob + newdata$self +newdata$goalf, family = binomial)

summary(mod4)

mod5 <-glm(newdata$dec~newdata$gender+newdata$int_corr+newdata$imprace+ diff_age
           +newdata$imprelig+newdata$like + newdata$prob + newdata$self +newdata$goalf, family = binomial)

summary(mod5)

#goal2 MEET NEW PEOPLE
#goal3 HAVING A DATE
#goal4 LONG TERM RELATIONSHIP
#goal5 JUST TO DO A SPEED DATE
#goal6 OTHER

goal1<- ifelse(newdata$goal==1, 1, 0)
goal2<- ifelse(newdata$goal==2, 1, 0)
goal3<- ifelse(newdata$goal==3, 1, 0)
goal4<- ifelse(newdata$goal==4, 1, 0)
goal5<- ifelse(newdata$goal==5, 1, 0)
goal6<- ifelse(newdata$goal==6, 1, 0)
goal6

mod6 <-glm(newdata$dec~newdata$gender+newdata$int_corr+newdata$imprace+ diff_age
           +newdata$imprelig+newdata$like + newdata$prob + newdata$self +goal1
           + goal2 + goal4 + goal6, family = binomial)

summary(mod6)



mod7 <-glm(newdata$dec~newdata$gender++newdata$imprace+
           newdata$imprelig+newdata$like + newdata$prob + newdata$self +
           goal2 + goal4 + goal6, family = binomial)

summary(mod7)

mod8 <-glm(newdata$dec~newdata$gender++newdata$imprace+ newdata$age_o+
             newdata$imprelig+newdata$like + newdata$prob + newdata$self +
             goal2 + goal4 + goal6, family = binomial)
summary(mod8)

modRace <-glm(newdata$dec~newdata$gender+newdata$imprace+ newdata$age_o+
          newdata$like + newdata$prob + newdata$self +
             goal2 + goal4 + goal6, family = binomial)
summary(modRace) #modello bellino CON IMPRACE
modRelig <-glm(newdata$dec~newdata$gender+ newdata$age_o+
             newdata$imprelig+newdata$like + newdata$prob + newdata$self +
             goal2 + goal4 + goal6, family = binomial)
modSameRace <-glm(newdata$dec~newdata$gender+newdata$samerace+ newdata$age_o+
             +newdata$like + newdata$prob + newdata$self +
             goal2 + goal4 + goal6, family = binomial)
BIC(modRace)
BIC(modRelig)
BIC(modSameRace)

diff_age_abs<-abs(diff_age)

mod9 <-glm(newdata$dec~newdata$gender+newdata$imprace+ diff_age_abs+
                newdata$like + newdata$prob + newdata$self +
                goal2 + goal4 + goal6, family = binomial)
summary(mod9)

mod10a <-glm(newdata$dec~newdata$gender+newdata$imprace+ diff_age_abs+ diff_age_abs*newdata$gender+
             newdata$like + newdata$prob + newdata$self +
             goal2 + goal4 + goal6, family = binomial)

mod10b <-glm(newdata$dec~newdata$gender+newdata$imprace+ diff_age_abs:newdata$gender+
             newdata$like + newdata$prob + newdata$self +
               goal2 + goal4 + goal6, family = binomial)



summary(mod10a)
summary(mod10b) 

#COMPARING WITH BIC INDEX "SAME RACE MODEL" WITH VARIABLE INTERACTION (SEX- 0 FOR WOMEN AND 1 FOR MEN) AND MODEL 10b 


BIC(mod10b)
BIC(modRace)

#INTERACTION DIFFAGE (no abs) AND SEX
mod10c <-glm(newdata$dec~newdata$gender+newdata$imprace+ diff_age:newdata$gender+
               newdata$like + newdata$prob + newdata$self +
               goal2 + goal4 + goal6, family = binomial)
summary(mod10c)

mod10c <-glm(newdata$dec~newdata$gender+newdata$imprace+ diff_age:newdata$gender+
               newdata$like + newdata$prob + newdata$self +
               goal2 + goal4 + goal6, family = binomial)

mod11 <-glm(newdata$dec~newdata$gender+newdata$imprace+ newdata$gender:goal4
               + newdata$gender:goal2+ newdata$gender:goal6+
               newdata$like + newdata$prob + newdata$self +
               goal2 + goal4 + goal6, family = binomial)
summary(mod11)

mod12 <-glm(newdata$dec~newdata$gender+newdata$imprace+ newdata$gender:goal4
            + newdata$gender:goal2+ newdata$gender:goal6+
              newdata$like + newdata$prob + newdata$self , family = binomial)
summary(mod12)

install.packages("bestglm")
require(bestglm)


library(leaps)
library(bestglm)
dec<-newdata$dec
newdata<-cbind(newdata[,-10],dec)

diff_age1<-newdata$gender*diff_age
View(diff_age1)
datinuovi<-newdata[,-c(2,3,4,5,6,8,9,10,11,12,13,14,15,19,20)]
datinew<-cbind(datinuovi, goal2, goal4,goal6, diff_age1, dec)



bestglm(datinew,family=binomial,IC="AIC")#can also use IC="BIC"
bestglm(datinew,family=binomial,IC="BIC")

modquasidef<-glm(datinew$dec~ datinew$gender+datinew$imprace+datinew$like+ datinew$goal2+ datinew$goal6
                 + datinew$prob + datinew$self , family = binomial)
summary(modquasidef)
exp(coef(modquasidef))



require(DescTools)
PseudoR2(modquasidef, which = "all")
cor(datinew$dec, fitted(modquasidef)) #0.5664055



library(car)
install.packages("blorr")
library(blorr)



likeg <- seq(0, 10, 0.5)
probg <- seq(0, 10, 0.5)
selfg <- seq(0, 10, 0.5)

Anova(modquasidef)
anova(modquasidef)
1-pchisq(modquasidef$deviance,modquasidef$df.residual)
with(modquasidef, pchisq(null.deviance - deviance, df.null - df.residual, 
                     lower.tail = F))

rocplot<-roc(y~fitted(modquasidef),data=)
plot.roc(rocplot,legacy.axes=TRUE)# Specficity on x axis if legacy.axes=F
auc(rocplot)

predict(modquasidef)
