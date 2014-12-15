pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used

  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  # Keep the number of monitors.
  monitorsNo <- length(id)
  # Create a list to store the data of each evaluated monitor.
  monitorsData <- list()
  # Loop through the numbers of monitors and generate the string for the name of each monitor corresponding file.
  for (i in 1:monitorsNo) {
    idi <- id[0 + i]
    prepend <- if (idi >= 1 && idi <= 9) {
      "00"
    } else if (idi >= 10 && idi <= 99) {
      "0"
    } else {
      ""
    }
    # Load the monitors data of the selected file.
    monitorsData[[i]] <- read.csv(paste(directory, "/", prepend, as.character(idi), ".csv", sep = ""))
  }
  # Initialize variables.
  meanfull <- 0
  sum <- 0
  len <- 0
  # Loop through the selected monitors data, obtain the corresponding pollutant column and calculate the mean.
  for (i in 1:monitorsNo) {
    tmp <- monitorsData[[i]][pollutant][[1]]
    sum <- sum + sum(tmp, na.rm = T)
    len <- len + length(tmp[!is.na(tmp)])
  }
  meanfull <- sum / len
  # Round the mean digits and return.
  mean <- round(meanfull, digits = 3)
  mean
}
