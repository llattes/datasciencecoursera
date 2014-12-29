best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
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
  min <- if (theOutcome == 1) {
    which.min(subset[, 11])
  } else if (theOutcome == 2) {
    which.min(subset[, 17])
  } else {
    which.min(subset[, 23])
  }
  hospitalName <- subset[min, ]$Hospital.Name
  hospitalName
}
