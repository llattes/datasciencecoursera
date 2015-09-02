fit <- lm(child ~ parent, galton)
# Standard deviation of the error.
sqrt(sum(fit$residuals ^ 2) / (n - 2))
# Same than this...
summary(fit)$sigma
# And this...
sqrt(deviance(fit) / (n - 2))

summary(fit)$r.squared
# It is the percentage of variation explained by the
# regression model.

# The intercept is really the coefficient of a special regressor
# which has the same value, 1, at every sample.
# The default intercept can be excluded by using -1 as follows.
lm(child ~ ones + parent - 1, galton)
lm(child ~ parent, galton)
# The regression line given by lm(child ~ parent, galton) goes
# through the point x=mean(parent), y=mean(child).

# Residuals should be uncorrelated with the fit, and should be
# independent and (almost) identically disributed with mean zero.
plot(fit, which = 1)
head(dfbeta(fit))
# Comparing the first row with those below it, we see that the first
# sample has a much larger effect on the slope (the x column) than other samples.

# Now calculate the influence of our outlier.
head(hatvalues(fit))

# Theoretically, however, residuals of individual samples have different
# variances and these differences can become large in the presence of outliers.
sigma <- sqrt(deviance(fit)/df.residual(fit))
rstd <- resid(fit)/(sigma * sqrt(1-hatvalues(fit)))
head(cbind(rstd, rstandard(fit)))

# Scale-Location plot
plot(fit, which=3)

# Studentized residuals (leave outlier out == fitno)
sigma1 <- sqrt(deviance(fitno)/df.residual(fitno))
resid(fit)[1]/(sigma1*sqrt(1-hatvalues(fit)[1]))
head(rstudent(fit))

# Cook distance
dy <- predict(fitno, out2) - predict(fit, out2)
sum(dy^2)/(2*sigma^2)
cooks.distance(fit)[1]
plot(fit, which=5)

# Knots for fitting complex functions:
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
knots <- c(0)
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
lines(x, yhat, col = "red", lwd = 2)
# Add more knots and re-draw line.
knots <- c(0, 2, 3, 4)
splineTerms2 <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat2 <- cbind(1, x, splineTerms2)
yhat2 <- predict(lm(y ~ xMat2 - 1))
lines(x, yhat2, col = "green", lwd = 2)
