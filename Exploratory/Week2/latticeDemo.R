## Simple lattice examples.
library(datasets)
library(lattice)
xyplot(Ozone ~ Wind, data = airquality)
xyplot(Ozone ~ Wind, data = airquality, main = "Ozone vs. Wind")
# transform function to make month variable a factor.
airquality <- transform(airquality, Month = factor(Month))
View(airquality)
str(airquality)
# 'data.frame':  153 obs. of  6 variables:
#  $ Ozone  : int  41 36 12 18 NA 28 23 19 8 NA ...
#  $ Solar.R: int  190 118 149 313 NA NA 299 99 19 194 ...
#  $ Wind   : num  7.4 8 12.6 11.5 14.3 14.9 8.6 13.8 20.1 8.6 ...
#  $ Temp   : int  67 72 74 62 56 66 65 59 61 69 ...
#  $ Month  : Factor w/ 5 levels "5","6","7","8",..: 1 1 1 1 1 1 1 1 1 1 ...
#  $ Day    : int  1 2 3 4 5 6 7 8 9 10 ...
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(1, 5))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))
# Draw xyplot with 2 panels based on factor.
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2, 1)) ## Plot with 2 panels
## Custom panel function
xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...) ## First call default panel function
  panel.lmline(x, y, col = 2) ## Overlay a simple linear regression line
})
# Demonstrate relation between "Goals for" and "Position" in english PL.
source('~/DataScience/datasciencecoursera/GettingAndCleaning/Week2/olescrapping.R')
england <- scrapPositions()
england <- transform(england, Pos = as.numeric(Pos))
england <- transform(england, Goals.for = as.numeric(Goals.for))
xyplot(Goals.for ~ Pos, data = england, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...) ## First call default panel function
  panel.lmline(x, y, col = 2)
  panel.abline(h = median(y), lty = 2)
})
# And "Goals against"?
england <- transform(england, Goals.against = as.numeric(Goals.against))
xyplot(Goals.against ~ Pos, data = england, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...) ## First call default panel function
  panel.lmline(x, y, col = 2)
  panel.abline(h = median(y), lty = 2)
})
