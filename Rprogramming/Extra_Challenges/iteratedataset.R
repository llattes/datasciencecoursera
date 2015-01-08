iteratedataset <- function(dataset) {
  input <- dataset
  obs <- dim(input)[1]
  vars <- dim(input)[2]
  variableName <- character(vars)
  topObservations <- character(vars)
  topObservationsVals <- numeric(vars)
  worstObservations <- character(vars)
  worstObservationsVals <- numeric(vars)
  for (i in 1:vars) {
    topval <- -Inf
    worstval <- Inf
    topname <- ""
    worstname <- ""
    variableName[i] <- colnames(input)[i]
    for (j in 1:obs) {
      actualvalue <- .subset2(input, i)[j]
      actualname <- row.names.data.frame(input)[j]
      if (!is.na(actualvalue)) {
        if (actualvalue > topval) {
          topval <- actualvalue
          topname <- actualname
        }
        if (actualvalue < worstval) {
          worstval <- actualvalue
          worstname <- actualname
        }
      }
    }
    topObservations[i] <- topname
    worstObservations[i] <- worstname
    topObservationsVals[i] <- topval
    worstObservationsVals[i] <- worstval
  }
  data.frame(variableName, topObservations, topObservationsVals, worstObservations, worstObservationsVals)
}
