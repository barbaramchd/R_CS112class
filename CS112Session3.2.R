#install.packages("arm")
library (ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family = binomial, data = Smarket)
summary(glm.fits)

coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]

glm.probs = predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)


#-----------------------///-------------------
#PRE-CLASS WORK

#Estimating the propensity scores
library(Matching)
data(lalonde)
?lalonde

str(lalonde)

#lalonde$treat <-as.numeric(lalonde$treat)-1

lm <- glm(treat ~ age+educ+black+hisp+married+nodegr+re74+re75+u74+u75, data=lalonde, family="binomial")
summary(lm)

lm$fitted.values

predict(lm) #return the logits, the linear propensity scores
plot(predict(lm))

# Getting the actual propensities, or the logit propensity scores
predict(lm, type="response")
lm$fitted.values
exp(predict(lm)) / (1 + exp(predict(lm)))  
plot(predict(lm, type = "response"))


#-----//-----
library(MASS)
?Pima.tr
Pima.tr
