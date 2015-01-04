calculate <- function(animal1, animal2, ignoreType) {
  score <- 0
  evaluateuntil <- length(animal1)
  if (ignoreType == TRUE) {
    evaluateuntil <- evaluateuntil - 1
  }
  for (i in 2:evaluateuntil) {
    if (animal1[i] == animal2[i]) {
      score <- score + 1
    }
  }
  score
}

zoo <- function(ignoreType = FALSE) {
  zoodata <- read.csv("zoo.data", header = FALSE, stringsAsFactors=FALSE)
  colnames(zoodata) <- c("animal_name", "hair", "feathers", "eggs", "milk",
                         "airborne", "aquatic", "predator", "toothed",
                         "backbone", "breathes", "venomous", "fins", "legs",
                         "tail", "domestic", "catsize", "type")
  zoodata$type <- as.factor(zoodata$type)
  zoodataobservations <- dim(zoodata)[1]
  animal <- character(zoodataobservations)
  best_match <- character(zoodataobservations)
  score <- numeric(zoodataobservations)
  bestscore <- -1
  for (i in 1:zoodataobservations) {
    bestscore <- -1
    animal[i] <- zoodata[i, 1]
    for (j in 1:zoodataobservations) {
      if (zoodata[i, 1] == zoodata[j, 1]) {
        next
      }
      currentscore <- calculate(zoodata[i, ], zoodata[j, ], ignoreType)
      if (currentscore > bestscore) {
        bestscore <- currentscore
        best_match[i] <- zoodata[j, 1]
        score[i] <- currentscore
      }
    }
  }
  results <- data.frame(animal, best_match, score)
  results
}
