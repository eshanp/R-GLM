install.packages("cowplot")
library(ggplot2)
library(cowplot)


url <-"http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"

#Reading the dataset from the URL
data <- read.csv(url, header = FALSE)

#Checking the first few rows of data
head(data)

#Including the column names as listed in the website.
colnames(data) <- c(
  "age",
  "sex",
  "chest_pain",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "heart_diease"
)
head(data)

#checking the distribution
cdplot(heart_diease~sex,data=data)
cdplot(heart_diease~age,data=data)
cdplot(heart_diease~thal,data=data)
cdplot(heart_diease~chol,data=data)

#Data cleaning
str(data)
dim(data)
data[data == "?"] <- NA
#Converting 0 and 1 to Female and Male
data[data$sex == 0, ]$sex <- "F"
data[data$sex == 1, ]$sex <- "M"
#Converitng columns into factors
data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$chest_pain)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$thal <- as.integer(data$thal)
data$ca <- as.factor(data$ca)
data$thal <- as.factor(data$thal)
#Converting 0 and 1 to Healthy and unhealthy.
data$heart_diease <- ifelse(test = data$heart_diease == 0,  yes = "Healthy",no = "Unhealthy")
data$heart_diease <- as.factor(data$heart_diease)
str(data)

nrow(data)
#Deleting the NA values
data <- na.omit(data)
nrow(data)
#Checking if the independent variables causes affect or not through tables.
xtabs( ~ heart_diease + sex, data = data)
xtabs( ~ heart_diease + cp, data = data)
xtabs( ~ heart_diease + fbs, data = data)
xtabs( ~ heart_diease + restecg, data = data)
xtabs( ~ heart_diease + exang, data = data)
xtabs( ~ heart_diease + slope, data = data)
xtabs( ~ heart_diease + ca, data = data)
xtabs( ~ heart_diease + thal, data = data)


#Running model to check significant variables effecting the dependent variable i.e. heart disease.
logisticmain <- glm(heart_diease ~ ., data = data, family = "binomial")
summary(logisticmain)

#GLM predicting heart disease on basis of sex and chest pain using binomial family.
logistic <- glm(heart_diease ~ sex+chest_pain+chol, data = data, family = "binomial")
summary(logistic)

#GLM predicting heart disease on basis of sex, chest pain, cholesterol, max heart rate achieved, and blood pressure using binomial family.
logistic2 <- glm(heart_diease ~ sex+chest_pain+chol+thalach+trestbps, data = data, family = "binomial")
summary(logistic2)

#Checking which model is better
anova(logistic, logistic2, test = "Chisq")



#Checking the interaction between variable
add1(logistic2,~ .^2,test = "Chisq")
Interaction <- glm( heart_diease ~ sex+chest_pain+chol+thalach+trestbps  
                    + thalach*trestbps, data = data,
                    family = binomial())
#nointeraction
summary(Interaction)
anova(logistic2, Interaction, test = "Chisq")

#Plots
predicted.data <- data.frame(
  probability.of.heart_diease = logistic2$fitted.values,
  heart_diease = data$heart_diease
)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
predicted.data <- predicted.data[order(predicted.data$probability.of.heart_diease, decreasing =  FALSE), ]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data = predicted.data, aes(x = rank, y = probability.of.heart_diease)) +
  geom_point(
    aes(color = heart_diease),
    alpha = 1,
    shape = 4,
    stroke = 2
  ) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")


