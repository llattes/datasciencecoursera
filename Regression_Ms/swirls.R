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
