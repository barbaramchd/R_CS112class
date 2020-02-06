library(ISLR)
library(Matching)

#VALIDATION SET APPROACH

set.seed(1) #set a seed for R's random number generator so we can have the same results every time
train = sample(392,196) #sample() split the set into two halves, by selecting a random subset of 196 observations out of the original 392 observations
lm.fit = lm (mpg~horsepower, data=Auto, subset=train) #fit a linear regression using only observations from the training set
attach(Auto)
#predict() estimate the responses for all 392 observations
#mean() calculates the average of the 196 observations in the validation set
#-train selects only the obervations that are not in the training set (or the observations from the validation set)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
#the estimated test MSE for the linear regression fit is 26.14

#estimating the test error for the quadratic regression
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#estimating the test error for the cubic regression
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train=sample(392,196)
lm.fiit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[train]^2)

#estimating the test error for the quadratic regression
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto, subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#estimating the test error for the cubic regression
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto, subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#the validation set error rates for the models with linear, quadratic,
#and cubic terms are 23037, 20.43, and 20.38 respectively

#LEAVE-ONE-OUT CROSS-VALIDATION

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
#cv.glm() produces a list with several components
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta #the two cross-validation results, which are the same: 24.23

#repeating the process for increasingly complex polynomial fits using a for loop
# the for loop iteratively fits polynomial regressions for polynomials of order i=1 to i=5,
#computes the associated cross-validation error, and stores it in the ith element of the vector cv.error
cv.error=rep(0,5)
for(i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

#K-FOLD CROSS-VALIDATION

set.seed(17)
cv.error.10=rep(0,10) #k=10
for(i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10

#BOOTSTRAP
#alpha.fn() takes as input (X,Y) and a vector  indicating which observations should be used to estimate the alpha
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)

#sample() will randomly select 100 observations from the range 1 to 100, with replacement
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio, alpha.fn, R=1000)
?Portfolio

#------------------------------------

#PRE-CLASS WORK
library(Matching)
data(lalonde)

set.seed(1)

treatment_group = lalonde$re78[lalonde$treat==1]
control_group = lalonde$re78[lalonde$treat==0]
means <- c()
for(i in 1:10000){
  treatment_sample=sample(treatment_group,length(treatment_group),replace=T)
  control_sample=sample(control_group,length(control_group), replace=T)
  means[i]<-mean(treatment_sample)-mean(control_sample)
}
quantile(means,c(0.025,0.975)) #95% confidence interval
quantile(means,c(0.125, 0.875)) #75% confidence interval

#-------------------- VINI'S CODE---------------

#Train your model on ALL the data
glm.lalonde <- glm(re78~.,data=lalonde)

#Performs LOOCV
cv.err <- cv.glm(lalonde,glm.lalonde)

#The MSE
cv.err$delta[1]

#Boostrapping

#auxiliary function
boot.fn <- function(data, index) return(coef(lm(re78~., data=data, subset=index)))

boot.fn(lalonde, 1:nrow(lalonde)) #check if the funciton is working
boot.lalonde <- boot(lalonde, boot.fn, 10000)

#Retrieve standard errors
boot.se <- apply(boot.lalonde$t, 2, sd)
lm.se <- summary(lm(re78~.,data=lalonde))$coef[,2]
format(data.frame(boot.se, lm.se),digits=2, scientific=F)