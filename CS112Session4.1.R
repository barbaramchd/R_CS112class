#install.packages("foreign")
library(arm)
library(foreign)

foo <- read.dta("turnout.dta")
foo


#Regression
glm2 <- glm (turnout ~ white+age+educate+income+agesqrd, data=foo, family="binomial")
glm2
glm2$coef

linear_logit <- sum(coef(glm2)*c(1,0.8607059,38,16,3.985987,14.44))
est_prob <- exp(linear_logit)/(1+exp(linear_logit))
linear_logit
est_prob

summary(glm2)

mean(foo$income)
mean(foo$white)

simulated.coefs <- sim(glm2, 1000)@coef


storage <- c()


for (i in 1:1000){
  
  XB <- sum(simulated.coefs[i,]*c(1,0.860705941781903, 38, 16,  3.98598663729691, 14.44))
  
  storage[i] <- exp(XB)/(1 + exp(XB))
}
storage

q <- quantile(storage, probs= c(0.01, 0.99))
q
         
