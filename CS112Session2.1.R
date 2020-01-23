SAT_score <- runif(n=200, min=0, max=1600)
lose_weight <- runif(n=200, min=0, max=5)
medical_exams <- runif(n=200, min=0, max=1)
irreducibleError <- 30*rnorm(200)
PS4 <- 0.2*SAT_score+0.35*lose_weight+0.45*medical_exams + irreducibleError
PS4
dataFrame <- data.frame(PS4,SAT_score, lose_weight, medical_exams, irreducibleError)


#Split data
data <- sample(1:200, 100, replace=FALSE)
my_data <- dataFrame[data,]
shared_data <- dataFrame[-data,]

#Are there any changes that David gets the PS4 if he gets a SAT score less than 1500, loses 2 kilos and improves its medical exams by 0.5?
plot(dataFrame$lose_weight, dataFrame$PS4,xlab="Weight loss", ylab="PS4 chances", main="kgXspro")
plot(dataFrame$medical_exams, dataFrame$PS4,xlab="Medical results", ylab="PS4 chances", main="resultsXspro")
plot(dataFrame$SAT_score, dataFrame$PS4,xlab="Score", ylab="PS4 chances", main="scoreXprob")


write.csv(my_data, file="my_data.csv", row.names = FALSE)
write.csv(shared_data, file = "shared_data.csv", row.names = FALSE)

setwd("C:/Users/barba/Desktop/R_CS112class")

foo <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTumGoKGzNFWI7JPQInrUHaDHG1dEf15VBXDPmyxBU7BO7p9A4Qtbi-LNOZuAX59Xi1AjiK7uB_hBok/pub?gid=996388243&single=true&output=csv")
