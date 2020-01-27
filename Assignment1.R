# Assignment 1: R Competency and The Drivetrain Approach to Decision Making

# 1. PREPROCESSING THE DATA

## Code 1: Loading the data

### Multilateral Development Institution Data
foo <- read.csv("https://tinyurl.com/yb4phxx8") #read the data

# column names
names(foo)

# dimensions of the data set
dim(foo)

# quick look at the data strcuture
head(foo)

# take note of the columns representing calendar dates
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25) #8 columns with dates


## Code 2: Data Preprocessing
# Addressing the missing values by calling the empty elements "NA"
# Also telling that these are "Date" objects
for(i in date.columns) # loops through the "date.columns"
{
  # Find missing values
  which_values_are_missing <- which(as.character(foo[,i]) == "")
  # Replace them by NAs
  foo[which_values_are_missing, i] <- NA
  # Convert values into Date class
  foo[, i] <- as.Date(as.character(foo[, i]))
}

# Testing that R knows that the columns are comprised of dates
foo[3, 12]
foo[4,12]
foo[3,12]-foo[4,12]

# Eliminating rows with NAs
# First, we find missing values by checking for NA
which.have.NAs <- which(is.na(foo$Rating == TRUE))

# Then we remove the rows with missing values, while retaining only the rows with ratings
# We store the new data set in a variable named new_foo instead of rewriting over the original data set
new_foo <- foo[-which.have.NAs,] 

################################################################################


### ASSIGNMENT 1

# As the assignment's instructions request, we need to consider only projects with
# non-missing "Circulation.Date" >= 2009-01-01
noNA_foo <- new_foo[!is.na(new_foo$CirculationDate),]
df <- noNA_foo[which(noNA_foo$CirculationDate >= as.Date("2009-01-01")), ]
summary(df$CirculationDate)

## Question 1
# (a)

# Some projects has no completion dates and we need to disregard them
sum(is.na(df$OriginalCompletionDate)) # 15 projects without completion date
nrow(df) # number of projects from 2009 to 2018 (1661 projects)
originalCompletionDate.with.NAs <- which(is.na(df$OriginalCompletionDate))
df_withDates <- df[-originalCompletionDate.with.NAs,]
expected_project_duration <- mean(df_withDates$OriginalCompletionDate) - mean(df_withDates$ApprovalDate)
expected_project_duration

#(b)
# Create a circulation year column
df_withDates$CirculationYear <- format(df_withDates$CirculationDate, "%Y")

# Check if there are rows with NAs in the Revised Completion Date column
sum(is.na(df_withDates$RevisedCompletionDate))  # No


# Create an delay column
df_withDates$Delay <- df_withDates$RevisedCompletionDate - df_withDates$OriginalCompletionDate 


# Checking the delays of year by year so we can have each year's mean, median, and IQR
library(dplyr)  
delayByYear <- df_withDates %>%  # %>% means "pass what is to the left to the function that follows"
  group_by(CirculationYear) %>%    
  summarise(mean.delay = mean(Delay), #mean delay of each year
            median.delay = median(Delay), # median delay of each year
            IQR.delay = quantile(Delay, 0.75) - quantile(Delay, 0.25)) # IQR of each year. Note that we are considering the middle quantile
delayByYear

# Plot
plot(delayByYear$CirculationYear, delayByYear$mean.delay, # chossing our X nad Y axis varaibles, respectively
     pch=15, col="red", ylim=c(100, 800), 
     xlab="Circulation Year", ylab="Delay (days)", main="Project Delay")
points(delayByYear$CirculationYear, delayByYear$median.delay, pch=16, col="blue")
points(delayByYear$CirculationYear, delayByYear$IQR.delay, pch=17, col="green")
legend("bottomleft", pch=c(15,16,17), 
       col=c("red","blue","green"), 
       legend=c("Mean Delay", "Median Delay", "IQR of Delays"))
grid(nx=NA, ny=NULL)

# (c)
# Create a actual durantion column
df_withDates$ActualDuration <- df_withDates$RevisedCompletionDate - df_withDates$ApprovalDate

# Expected duration
df_withDates$ExpectedDuration <- df_withDates$OriginalCompletionDate - df_withDates$ApprovalDate

# Actual Duration and Expected Duration statistics
cat("Actual Duration mean: ", mean(df_withDates$ActualDuration))
cat("Expected Duration mean: ", mean(df_withDates$ExpectedDuration))
cat("Actual Duration median: ", median(df_withDates$ActualDuration))
cat("Expected Duration median: ", median(df_withDates$ExpectedDuration))
cat("Actual Duration quantiles: ", quantile(df_withDates$ActualDuration))
cat("Expected Duration quantiles: ", quantile(df_withDates$ExpectedDuration))
cat("Actual Duration IQR: ", IQR(df_withDates$ActualDuration))
cat("Expected Duration IQR: ", IQR(df_withDates$ExpectedDuration))
cat("Actual Duration range: ", range(df_withDates$ActualDuration))
cat("Expected Duration range: ", range(df_withDates$ExpectedDuration))


## Question 2
df_2010toNow <- noNA_foo[which(noNA_foo$RevisedCompletionDate >= as.Date("2010-01-01")), ]
print("Distribution of Project Ratings")
prop.table(table(df_2010toNow$Rating)) * 100

## Question 3
df_PATA <- df_2010toNow[which(df_2010toNow$Type == 'PATA'),]
print("Distribution of Project Ratings (PATA only)")
table(df_PATA$Rating) / length(df_PATA$Rating) * 100  

## Question 4
# First we select the projects with the top 10% of budget
# We do so by selecting the quantile that are equal or highe than 0.9
# We create the dataframe df_TopQuant to store these top projects
df_TopQuant <- df[which(df$RevisedAmount >= quantile(df$RevisedAmount, 0.9)),]
# Then we select the projects with the bottom 10% of budget
# We do so by selecting the quantile that are equal or lower than 0.1
# We create the dataframe df_BottomQuant to store these bottom projects
df_BottomQuant <- df[which(df$RevisedAmount <= quantile(df$RevisedAmount, 0.1)),]

# Compare the ratings
# To compare the ratings we first find the fraction of the top projects that got that specfic rating and multiple by 100
# This give us the percentage of the top projcts that got that specific rating
# We do the same for the projects on the bottom quantile
# Then we subtractthe percentage of the bottom quantile projects from the top quantile projects
(table(df_TopQuant$Rating) / length(df_TopQuant$Rating) * 100) - 
  (table(df_BottomQuant$Rating) / length(df_BottomQuant$Rating) * 100)
# This return a small table with the differences of the percentages between top and bottom quantile projects for that specific rating
# We found small differences ( 2% difference max)

# Compare other features
# We repeat the process to other features
# For Departments we look at count instead of percentages because there are many departments
df_TopQuant$Dept # looking at all the categories we have for Department
# As we have many different categories, the percentages might be misleading (e.g., a difference of 100%
# because there is one project for a specific category )
# Dept
table(df_TopQuant$Dept) - table(df_BottomQuant$Dept)
max(table(df_TopQuant$Dept) - table(df_BottomQuant$Dept))
min(table(df_TopQuant$Dept) - table(df_BottomQuant$Dept))
# Note that out of the difference between top and bottom quantiles, 
# the largest number we found was 10 (SDCC department)
# meaning that more projects in the top quantile are within that department
# Similarly, the smallest number we found was -27, from the SARD department.

# Cluster
df_TopQuant$Cluster # looking into the Cluster's categories; lots of missing data 
# For cluster we do the same process and we look at percentages
(table(df_TopQuant$Cluster) / length(df_TopQuant$Cluster) * 100) - 
  (table(df_BottomQuant$Cluster) / length(df_BottomQuant$Cluster) * 100)
# We found some siggnificant differences percentage-wise
# However, there are a lot of missing data, so it is not very conclusive.

# Country
df_TopQuant$Country # looking into the Country's categories
table(df_TopQuant$Country) / length(df_TopQuant$Country) * 100 # looking the fraction of top quantile projects that were from a specific country
max(table(df_TopQuant$Country) / length(df_TopQuant$Country) * 100) # the majority of the top projects are from REF country
table(df_BottomQuant$Country) / length(df_BottomQuant$Country) * 100 # looking the fraction of bottom quantile projects that were from a specific country
max(table(df_BottomQuant$Country) / length(df_BottomQuant$Country) * 100) # the majority of the bottom projects also are from REF country
(table(df_TopQuant$Country) / length(df_TopQuant$Country) * 100) - 
  (table(df_BottomQuant$Country) / length(df_BottomQuant$Country) * 100)
# Observe that we found negative numbers for percentages
# That means that the country had a larger percentage of bottom project than top project
# Out of the top +  quantile quantiles, 11.2% more of top projects were from REG
max((table(df_TopQuant$Country) / length(df_TopQuant$Country) * 100) - 
      (table(df_BottomQuant$Country) / length(df_BottomQuant$Country) * 100))
# Out of the top +  quantile quantiles, 5.22% more of bottom projects were from BAN
min((table(df_TopQuant$Country) / length(df_TopQuant$Country) * 100) - 
      (table(df_BottomQuant$Country) / length(df_BottomQuant$Country) * 100))