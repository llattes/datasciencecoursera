library(randomForestSRC)
library(readr)

setwd("~/DataScience/datasciencecoursera/Kaggle")

train <- read_csv("input/train.csv")
test <- read_csv("input/test.csv")

# labels <- as.factor(train[rows,1])
# train <- train[rows,-1]
options(rf.cores = -1L, mc.cores=-1L)
rf <- rfsrc(label ~ ., data = train, ntree = 42, nsplit = 10)

# rf <- randomForest(train, labels, xtest=test, ntree=numTrees)
# predictions <- data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
# head(predictions)

# write_csv(predictions, "output/rf_tuned2.csv")
