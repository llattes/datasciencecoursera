library(class)
library(readr)

setwd("~/Projects/datasciencecoursera/Kaggle")

train <- read_csv("input/train.csv")
test <- read_csv("input/test.csv")

train$label <- as.factor(train$label)

numTrain <- 5000
set.seed(17)
rows <- sample(1:nrow(train), numTrain)
train2 <- train[rows, ]

classif <- knn(train = train2[, -1], test = test, cl = train2[, 1], k = 10)

predictions <- data.frame(ImageId=1:nrow(test), Label=levels(train$label)[labels])
write_csv(predictions, "output/knn.csv")
