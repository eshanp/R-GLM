require(MASS)
library(MASS)
data("Boston")
attach(Boston)
dim(Boston)
summary(Boston)
str(Boston)
# Creating the variable to store the crime rate below or above the median (1: above the median, 0: below the median)

CrimeRate <- rep(0,length(crim))

CrimeRate[crim > median(crim)] <- 1

# Copying Boston data into another dataframe:

BostonData <- data.frame(Boston)

# Adding the response variable column (CrimeRate) created above in this dataframe:

BostonData <- data.frame(Boston, CrimeRate)

# Converting the necessary variables into factor variables:

BostonData$DummyChas <- factor(BostonData$chas)

BostonData$CrimeRate <- factor(BostonData$CrimeRate)

# Splitting the data into training-testing data for better prediction:

set.seed(123)

BostonTrain <- sample(nrow(BostonData),as.integer(nrow(BostonData)*0.50))

train.BostonData = BostonData[BostonTrain,]

test.BostonData = BostonData[-BostonTrain,]

crimebos = test.BostonData$CrimeRate

# Building a multivariate logistic regression model:

Model1 = glm(CrimeRate ~ . - crim, data = train.BostonData, family = "binomial")
summary(Model1)

predbos = predict(Model1,test.BostonData, type = "response")

predbos.glm <- rep(0, length(predbos))

predbos.glm[predbos > 0.5] <- 1

accuracy <- table(predbos.glm, crimebos)

# Error rate of the model:

mean(predbos.glm != crimebos)
sum(diag(accuracy))/sum(accuracy)
confint(Model1, parm = "crim")

Model2 = glm(CrimeRate ~ rm  + rad + medv + ptratio , data = train.BostonData, family = "binomial")
require(caret)
library(caret)
summary(Model2) 

predbos = predict(Model2,test.BostonData, type = "response")

predbos.glm <- rep(0, length(predbos))

predbos.glm[predbos > 0.5] <- 1

accuracy <- table(predbos.glm, crimebos)

accuracy
mean(predbos.glm != crimebos)
sum(diag(accuracy))/sum(accuracy)
anova(Model1, Model2, test = "Chisq")


