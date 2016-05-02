pisaTrain <- read.csv("/media/ANIRUDH/MIT Analytics Edge/Assignment 02/pisa2009train.csv")
pisaTest <- read.csv("/media/ANIRUDH/MIT Analytics Edge/Assignment 02/pisa2009test.csv")

# students in training set:
nrow(pisaTrain)

# average reading score of males
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# to check for NAs in variables
summary(pisaTrain)

# to remove observations with at least one missing value
pisaTrain <- na.omit(pisaTrain)
pisaTest <- na.omit(pisaTest)


# check for factor variables using str command
str(pisaTrain)

# relevel reference level of factor raceeth to 'White'
# default reference level of factor is based on alphabetical order
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# build a model
lmScore <- lm(readingScore ~ ., data = pisaTrain)
s1 <- summary(lmScore)

# R squared for this model
s1$r.squared

# RMSE
RSS <- sum(s1$residuals^2)
RMSE <- sqrt(RSS/nrow(pisaTrain))

# difference in predicted mean reading scores of students from grade 9 and 11
# 2 times coefficient of 'grade' variable
s1$coefficients[2]*2

# predict reading scores in test dataset
predTest <- predict(lmScore, newdata = pisaTest)
max(predTest) - min(predTest)

# sum of squared errors
SSE <- sum((predTest - pisaTest$readingScore)^2)
RMSE <- sqrt(SSE/nrow(pisaTest))

# predicted test score using baseline model
baselinePred <- mean(pisaTrain$readingScore)
SST <- sum((baselinePred - pisaTest$readingScore)^2)

# test set R squared
1 - SSE/SST
