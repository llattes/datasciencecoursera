library(kernlab)
library(readr)

setwd("~/DataScience/datasciencecoursera/Kaggle")

train <- read_csv("input/train.csv")
test <- read_csv("input/test.csv")

train$label <- as.factor(train$label)

numTrain <- 6000
set.seed(13)
rows <- sample(1:nrow(train), numTrain)
train2 <- train[rows, ]

filter <- ksvm(label ~ ., data = train2, kernel = "polydot", kpar = list(degree = 2), cross = 5, tol = 0.005)
labels <- predict(filter, test)

predictions <- data.frame(ImageId=1:nrow(test), Label=levels(train$label)[labels])
write_csv(predictions, "output/polydot_d2_c5_6000_2.csv")
