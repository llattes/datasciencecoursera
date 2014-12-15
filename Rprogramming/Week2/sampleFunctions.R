readData <- function(x) {
  fulldata <- read.csv(x)
  fulldata
}

readDataAndGetMean <- function(x, nona = T) {
  fulldata <- read.csv(x)
  means <- numeric(ncol(fulldata))
  for (i in 1:ncol(fulldata)) {
    means[i] <- mean(fulldata[, i], na.rm = nona)
  }
  means
}