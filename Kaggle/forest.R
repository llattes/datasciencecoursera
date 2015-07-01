library(randomForest)
library(readr)

set.seed(1)

numTrees <- 1501

train <- read_csv("input/train.csv")
test <- read_csv("input/test.csv")

labels <- as.factor(train[ , 1])
train <- train[ , -1]

rf <- randomForest(train, labels, xtest=test, ntree=numTrees)
predictions <- data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
head(predictions)

write_csv(predictions, "output/rf_tuned2.csv")
