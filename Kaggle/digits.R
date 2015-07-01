library(flexclust)
setwd("~/DataScience/datasciencecoursera/Kaggle")
train_data <- read.csv(file = "input/train.csv")
train_data$label <- as.factor(train_data$label)
summary(train_data$label)
set.seed(1)
train_data_subset <- sample(1:nrow(train_data), 5000)
train_data_subset <- train_data[train_data_subset, ]
summary(train_data_subset$label)
k_means <- kmeans(x = train_data_subset[, -1], centers = 10, iter.max = 25, nstart = 20)
k_means$size
kcca_kmedians <- kcca(x = train_data_subset[, -1], k = 10, family = kccaFamily("kmedians"))
# 558 512 570 682 439 698 462 823 710 546
kcca_k_means <- as.kcca(k_means, train_data_subset[, -1])
test_data <- read.csv(file = "input/test.csv")
prediction <- predict(object = kcca_k_means, newdata = test_data)
train_data_subset$cluster <- k_means$cluster
train_data_subset$cluster <- as.factor(train_data_subset$cluster)

maxTable <- function(InVec, mult = FALSE) {
  if (!is.factor(InVec)) InVec <- factor(InVec)
  A <- tabulate(InVec)
  if (isTRUE(mult)) {
    levels(InVec)[A == max(A)]
  } 
  else levels(InVec)[which.max(A)]
}

replacements <- sapply(X = levels(train_data_subset$cluster), FUN = function(x) {maxTable(train_data_subset[train_data_subset$cluster == x, ]$label)})
sapply(X = levels(train_data_subset$cluster), FUN = function(x) {summary(train_data_subset[train_data_subset$cluster == x, ]$label)})

prediction <- as.factor(prediction)
summary(prediction)

### CRAPPY strategy for avoiding duplicated replacements, TODO: improve!
dups <- duplicated(replacements)
present <- c(0:9) %in% replacements

for (i in 1:10) {
  if (dups[i]) {
    for (j in 1:10) {
      if (!present[j]) {
        replacements[i] <- j - 1
      } 
    }
  }
}
###

test_data$prediction <- prediction

digits <- sapply(X = c(1:28000), FUN = function(x) {replacements[.subset2(test_data, 785)[x]]})
test_data$ImageId <- c(1:28000)
test_data$Label <- digits

write.table(x = test_data[786:787], file = "output/digits3.csv", sep = ",", quote = c(2), row.names = FALSE)


