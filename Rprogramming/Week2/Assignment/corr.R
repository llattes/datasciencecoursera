corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0

  ## Return a numeric vector of correlations

  monitorsData <- list()
  for (i in 1:332) {
    idi <- i
    prepend <- if (idi >= 1 && idi <= 9) {
      "00"
    } else if (idi >= 10 && idi <= 99) {
      "0"
    } else {
      ""
    }
    monitorsData[[i]] <- read.csv(paste(directory, "/", prepend, as.character(idi), ".csv", sep = ""))
  }
  # Initialize variables for storing the correlations. A list is used due to its flexibility.
  corrs <- list()
  cases <- NULL
  sup <- 0
  vec <- NULL
  # Loop through all monitors.
  for (i in 1:332) {
    idi <- i
    cases <- complete.cases(monitorsData[[i]])
    # Check the threshold condition.
    if (length(cases[cases == T]) > threshold) {
      sup <- sup + 1
      df <- monitorsData[[i]]
      sulf <- df[[2]]
      nitr <- df[[3]]
      # Calculate correlation between the cols no. 2 & 3 (sulfate and nitrate)
      # using complete observations only.
      aCorr <- cor(x = sulf, y = nitr, use = "complete.obs")
      rel <- aCorr
      corrs[[sup]] <- rel
    }
  }
  # Unlist the list of correlations to return a vector.
  if (length(corrs) != 0) {
    vec <- unlist(corrs)
  } else {
    vec = numeric(length = 0L)
  }
  vec
}
