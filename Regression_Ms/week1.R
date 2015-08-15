setwd("~/Projects/datasciencecoursera/Regression_Ms")
install.packages("UsingR")
library("UsingR", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
data("galton")
galton
View(galton)
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

# Melt separates child and parent heights.
long <- melt(galton)
View(long)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g + geom_histogram(colour = "black", binwidth = 1)
g <- g + facet_grid(. ~ variable)
g
?galton

mean(long[long["variable"] == "child", ]$value)

install.packages("manipulate")
library("manipulate", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
myHist <- function(mu){
  mse <- mean((galton$child - mu)^2)
  g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", colour = "black", binwidth=1)
  g <- g + geom_vline(xintercept = mu, size = 3)
  g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
  g
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.01))

ggplot(galton, aes(x = parent, y = child)) + geom_point()

library(dplyr)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")
g

y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
  g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
  g <- g  + scale_size(range = c(2, 20), guide = "none" )
  g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
  g <- g + geom_point(aes(colour=freq, size = freq))
  g <- g + scale_colour_gradient(low = "lightblue", high="white")
  g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
  mse <- mean( (y - beta * x) ^2 )
  g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
  g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

# -1 for removing intercept.
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)
lm(I(child - mean(child))~ I(parent - mean(parent)), data = galton)
lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)

# Ejemplo normalizacion
rand <- rnorm(10, 50, 10)
mean((rand - mean(rand)) / sd(rand))
sd((rand - mean(rand)) / sd(rand))

?cov
rpois(10, 6)
rpois(10, 4)
rpois(10, 4)
pois6 <- rpois(10, 6)
pois4 <- rpois(10, 4)
cov(pois4, pois6)
cov(pois6, pois4)
cor(pois6, pois4)
poises <- as.data.frame(table(pois4, pois6))
poises
poises <- as.data.frame(pois4, pois6)
poises
data_frame(pois4, pois6)
poises <- data_frame(pois4, pois6)
poises
ggplot(poises, aes(x = pois4, y = pois6)) + geom_point()
g <- ggplot(poises, aes(x = pois4, y = pois6)) + geom_point()
g
g <- g + geom_smooth(method="lm", formula=y~x)
g
