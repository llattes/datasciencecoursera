rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  possible <- c("heart attack", "heart failure", "pneumonia")
  byState <- grep(state, data$State, fixed = TRUE)
  if (length(byState) <= 0) {
    stop("invalid state")
  }
  theOutcome <- grep(outcome, possible, fixed = TRUE)
  if (length(theOutcome) <= 0) {
    stop("invalid outcome")
  }
  subset <- data[byState, ]
  order(subset)
  ordered <- numeric()
  min <- if (theOutcome == 1) {
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
  order <- 0;
  if (num == "best") {
    order <- 1
  } else if (num == "worst") {
    order <- length(ordered)
  } else {
    order <- num
  }
  subset[ordered[order], ]$Hospital.Name
}
