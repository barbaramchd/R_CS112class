#install.packages("ISLR")
library(MASS)
library(ISLR)

'----'
SAT_score <- runif(n=200, min=0, max=1600)
lose_weight <- runif(n=200, min=0, max=5)
medical_exams <- runif(n=200, min=0, max=1)
irreducibleError <- 30*rnorm(200)
PS4 <- 0.2*SAT_score+0.35*lose_weight+0.45*medical_exams + irreducibleError
PS4
df <- data.frame(PS4,SAT_score, lose_weight, medical_exams, irreducibleError)

#Split data
data <- sample(1:200, 100, replace=FALSE)
my_data <- df[data,]
shared_data <- df[-data,]
shared_data

write.csv(my_data, file="my_data.csv", row.names = FALSE)
write.csv(shared_data, file = "shared_data.csv", row.names = FALSE)

setwd("C:/Users/barba/Desktop/R_CS112class")
'----'

#Understanding the data
shared_data
names(shared_data)


lm.fit=lm(PS4~SAT_score+lose_weight+medical_exams, data=shared_data) #creating a simple regression model
attach(shared_data) #recognizing the variables
lm.fit=lm(PS4~SAT_score+lose_weight+medical_exams) #defining the varaibles we will work with

#Printing info about the linear model
lm.fit
summary(lm.fit)
names(lm.fit)
#Multiple R-squared: 0.8998
#Adjusted R-squared: 0.8993
coef(lm.fit)
confint(lm.fit) #coefficient estimates

#Plotting
plot(SAT_score+lose_weight+medical_exams,PS4) 
abline(lm.fit, col = "red", lwd = 3)

#Creating prediction points
#those prediciton pits are necessary to find the RSME
prediction <- predict(lm.fit, shared_data)
prediction #prdiction points
difference <- prediction - shared_data$PS4
difference #difference between the real points (shared_data$PS4) and the prediction points (prediction) 
rsme <- sqrt(mean(difference^2))
rsme
