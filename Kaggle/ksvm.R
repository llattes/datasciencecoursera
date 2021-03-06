library(kernlab)
library(readr)

setwd("~/Projects/datasciencecoursera/Kaggle")

train <- read_csv("input/train.csv")
test <- read_csv("input/test.csv")

train$label <- as.factor(train$label)

numTrain <- 7500
set.seed(17)
rows <- sample(1:nrow(train), numTrain)
train2 <- train[rows, ]

filter <- ksvm(label ~ ., data = train2, kernel = "besseldot", C = 10, kpar = list(degree = 2), cross = 5)
labels <- predict(filter, test)

predictions <- data.frame(ImageId=1:nrow(test), Label=levels(train$label)[labels])
write_csv(predictions, "output/besseldot_d2_C10_7500.csv")
