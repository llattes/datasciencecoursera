cleanData <- function() {
  library(datasets)
  data(HairEyeColor)
  mtx <- matrix(byrow = TRUE, ncol = 3, nrow = 1000)
  colnames(mtx) <- (c("Sex", "Eye_color", "Hair_color"))
  mtxindex <- 1
  for (i in 1:length(dimnames(HairEyeColor)$Sex)) {
    sex <- dimnames(HairEyeColor)$Sex[i]
    for (j in 1:length(dimnames(HairEyeColor)$Eye)) {
      eye <- dimnames(HairEyeColor)$Eye[j]
      for (k in 1:length(dimnames(HairEyeColor)$Hair)) {
        hair <- dimnames(HairEyeColor)$Hair[k]
        res <- c(sex, eye, hair)
        for (l in 1:HairEyeColor[k, j, i]) {
          mtx[mtxindex, ] <- res
          mtxindex <- mtxindex + 1
        }
      }
    }
  }
  df <- data.frame(mtx)
  subsetted <- subset(df, complete.cases(df))
  subsetted
}
