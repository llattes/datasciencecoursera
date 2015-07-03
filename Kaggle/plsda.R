library(readr)
library(MASS)
library(caret)
library(pls)
library(klaR)

setwd("~/DataScience/datasciencecoursera/Kaggle")

train <- read_csv("input/train.csv")
test <- read_csv("input/test.csv")

x <- train[,2:785]
y <- as.factor(train[,1])

fit <- plsda(x, y, probMethod = "Bayes")
# There were some warnings 
predictions <- predict(fit, test)

predictions <- data.frame(ImageId=1:nrow(test), Label=levels(y)[predictions])
write_csv(predictions, "output/plsda.csv")
