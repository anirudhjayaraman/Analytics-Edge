# load the dataset
climate_change <- read.csv("/media/ANIRUDH/MIT Analytics Edge/Assignment 02/climate_change.csv")

# divide dataset into train and test sets based on year...
train <- subset(climate_change, Year <= 2006)
test <- subset(climate_change, Year > 2006)

# linear model with Temp as independent variable to predict for post 2006
model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
             data = train)
# print summary of model
s1 <- summary(model1)
# print R squared
s1$r.squared

# checking for correlations among variables
# excluded variables Year and Month:
plot(cor(train[3:11]))

# Listing out all correlations in the model
cor(train[3:11])

# Correlations of N2O with other variables
cor(train[3:11])[4,]

# List only those correlations of N2O with other variables s.t.
# absolute correlation be greater than 0.7
cor(train[3:11])[4,abs(cor(train[3:11])[4,]) > 0.7]


# Correlations of CFC.11 with other variables
cor(train[3:11])[5,]

# List only those correlations of CFC.11 with other variables s.t.
# absolute correlation be greater than 0.7
cor(train[3:11])[5,abs(cor(train[3:11])[5,]) > 0.7]

# Given that the correlations are so high, let us focus on the N2O variable 
# and build a model with only MEI, TSI, Aerosols and N2O as independent 
# variables

model2 <- lm(Temp ~ MEI + TSI + Aerosols + N2O, data = train)
s2 <- summary(model2)

# to check the attributes of the model...
attributes(s2)

# print coefficients of model
s2$coefficients
# to print coefficient of N2O
s2$coefficients[5,1]

# print R Squared of the model
s2$r.squared

# Automatically building the model with R's step function
# automate the procedure of trying different combinations of variables 
# to find a good compromise of model simplicity and R2. This trade-off is 
# formalized by the Akaike information criterion (AIC) - it can be 
# informally thought of as the quality of the model with a penalty for 
# the number of variables in the model.

model3 <- step(model1)
s3 <- summary(model3)

# new R squared value:
s3$r.squared

# Predict test set Temp using new model
predicted <- predict(model3, test)

# Compute R Squared of test set
SSE <- sum((predicted - test$Temp)^2)
SST <- sum((mean(train$Temp) - test$Temp)^2)
1 - (SSE/SST)
