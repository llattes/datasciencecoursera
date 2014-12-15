complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  monitorsNo <- length(id)
  monitorsData <- list()
  for (i in 1:monitorsNo) {
    idi <- id[0 + i]
    prepend <- if (idi >= 1 && idi <= 9) {
      "00"
    } else if (idi >= 10 && idi <= 99) {
      "0"
    } else {
      ""
    }
    monitorsData[[i]] <- read.csv(paste(directory, "/", prepend, as.character(idi), ".csv", sep = ""))
  }
  frameData <- list()
  # Create two numeric column vectors for storing IDs and nObs.
  c1 <- numeric(length = monitorsNo)
  c2 <- numeric(length = monitorsNo)
  # Loop through the selected monitors and test for complete cases.
  for (i in 1:monitorsNo) {
    idi <- id[0 + i]
    cases <- complete.cases(monitorsData[[i]])
    c1[i] <- idi
    c2[i] <- length(cases[cases == T])
  }
  # Create a data frame as requested in the assignment and return.
  frame <- data.frame(c1, c2)
  colnames(frame) <- c("id", "nobs")
  frame
}
