# load the dataset
FluTrain <- read.csv("/media/ANIRUDH/MIT Analytics Edge/Assignment 02/FluTrain.csv")

# week corresponding to the highest percentage of ILI-related 
# physician visits
FluTrain$Week[which.max(FluTrain$ILI)]

# week corresponding to the highest percentage of ILI-related 
# query fraction
FluTrain$Week[which.max(FluTrain$Queries)]

# plot the histogram of the dependent variable
hist(FluTrain$ILI)

# Plot the natural logarithm of ILI versus Queries
hist(log(FluTrain$ILI))
plot(FluTrain$Queries,log(FluTrain$ILI))

# Model the above relation
FluTrend1 <- lm(log(FluTrain$ILI) ~ Queries, data = FluTrain)
s1 <- summary(FluTrend1)

# R Squared of this model
s1$r.squared

# Relation between R Squared and Correlation
cor(log(FluTrain$ILI), FluTrain$Queries) ^2

# We can convert from predictions of log(ILI) 
# to predictions of ILI via exponentiation, 
# or the exp() function
FluTest <- read.csv("/media/ANIRUDH/MIT Analytics Edge/Assignment 02/FluTest.csv")
PredTest1 <- exp(predict(FluTrend1, newdata=FluTest))

# Predict percentage ILI related physician visits for week of 
# March 11, 2012
PredTest1[which(FluTest$Week == '2012-03-11 - 2012-03-17')]

# relative error betweeen the estimate (our prediction) and the observed 
# value for the week of March 11, 2012

1 - (PredTest1[which(FluTest$Week == '2012-03-11 - 2012-03-17')])/FluTest$ILI[which(FluTest$Week == '2012-03-11 - 2012-03-17')]

# RMSE on Test Set
SSE <- sum((PredTest1 - FluTest$ILI)^2)
RMSE <- sqrt(SSE/nrow(FluTest))

# Training a Time Series Model
library(zoo)
# In these commands, the value of -2 passed to lag means to return 
# 2 observations before the current one; a positive value would have 
# returned future observations. The parameter na.pad=TRUE means to 
# add missing values for the first two weeks of our dataset, where 
# we can't compute the data from 2 weeks earlier
ILILag2 <- lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 <- coredata(ILILag2)
View(FluTrain)

# plot log(ILILag2) against log(ILI)
plot(log(FluTrain$ILI), log(FluTrain$ILILag2))
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), FluTrain)
s2 <- summary(FluTrend2)

# R squared
s2$r.squared

# Adding lagged variable to the Test set
ILILag2Test <- lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 <- coredata(ILILag2Test)
View(FluTest)
FluTest$ILILag2[1] <- FluTrain$ILI[416]
FluTest$ILILag2[2] <- FluTrain$ILI[417]
View(FluTest)

# Predict outcome ILI for test data with FluTrend2 model
PredTest2 <- exp(predict(FluTrend2, newdata = FluTest))

# Test set RMSE of the FluTrend2 model
SSE2 <- sum((PredTest2 - FluTest$ILI)^2)
RMSE2 <- sqrt(SSE2/nrow(FluTest))
SSE1 <- sum((PredTest1 - FluTest$ILI)^2)
RMSE1 <- sqrt(SSE1/nrow(FluTest))


