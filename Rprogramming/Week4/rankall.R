rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name

  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  possible <- c("heart attack", "heart failure", "pneumonia")
  states <- unique(data$State)
  orderedstates <- states[order(states)]
  theOutcome <- grep(outcome, possible, fixed = TRUE)
  if (length(theOutcome) <= 0) {
    stop("invalid outcome")
  }
  ranked <- character(length(orderedstates))
  for (i in 1:length(orderedstates)) {
    byState <- grep(orderedstates[i], data$State, fixed = TRUE)
    subset <- data[byState, ]
    ordered <- numeric()
    if (theOutcome == 1) {
      l <- subset[, 11]
      alp <- subset[, 3]
      ordered <- order(as.numeric(l), alp, na.last = NA, decreasing = FALSE)
    } else if (theOutcome == 2) {
      l <- subset[, 17]
      alp <- subset[, 3]
      ordered <- order(as.numeric(l), alp, na.last = NA, decreasing = FALSE)
    } else {
      l <- subset[, 23]
      alp <- subset[, 3]
      ordered <- order(as.numeric(l), alp, na.last = NA, decreasing = FALSE)
    }
    ord <- 0;
    if (num == "best") {
      ord <- 1
    } else if (num == "worst") {
      ord <- length(ordered)
    } else {
      ord <- num
    }
    ranked[i] <- subset[ordered[ord], ]$Hospital.Name
  }
  fr <- data.frame(ranked, orderedstates)
  colnames(fr) <- c("hospital", "state")
  fr
}