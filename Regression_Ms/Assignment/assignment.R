library(ggplot2)
?mtcars
data(mtcars)
mtcars$am <- as.factor(mtcars$am)

View(mtcars)
lm(mpg ~ am, data = mtcars)
fit <- lm(mpg ~ am, data = mtcars)
summary(fit)
lm_eqn <- function(m) {
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  }

  as.character(as.expression(eq));
}

f <- ggplot(mtcars, aes(x = am, y = mpg)) + geom_jitter()
f
f <- ggplot(mtcars, aes(x = am, y = mpg)) + geom_point()
f <- f + geom_text(aes(-Inf, -Inf, hjust = 0, vjust = 0, label = lm_eqn(fit)), parse = TRUE)
f <- f + geom_smooth(method ="lm")
f
f <- f + geom_smooth(method ="lm")
setwd("~/Projects/datasciencecoursera/Regression_Ms/Assignment")
e <- resid(fit)
yhat <- predict(fit)
n <- length(mtcars$mpg)

f + geom_segment(aes(x = am,
                 y = mpg,
                 xend = am,
                 yend = 7.245 * am + 17.147),
             size = 1)

fit2 <- lm(mpg ~ hp, data = mtcars)
f <- ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()
f <- f + geom_smooth(method ="lm")
f <- f + geom_segment(aes(x = hp,
                     y = mpg,
                     xend = hp,
                     yend = fit2$coefficients[2] * hp + fit2$coefficients[1],
                     color = abs(mpg - (fit2$coefficients[2] * hp + fit2$coefficients[1]))),
                 size = 1)
library(dplyr)
summarise(group_by(mtcars, am), mn = mean(mpg))

g2 <- ggplot(data = mtcars, aes(y = mpg, x = factor(am), fill = factor(am)))
g2 <- g2 + geom_violin(colour = "black", size = 2)
g2 <- g2 + xlab("0 = auto, 1 = manual") + ylab("miles per US gallon")
g2

ggpairs(mtcars, lower = list(continuous = "smooth"))
# El corr: que sale en el gráfico, es "from moderate to weak", according to
# Hair et al. (2011) & Hair et al. (2013) rule of thumb.
cor(mtcars$drat, mtcars$mpg)
# CYL, DISP, WT mejores que AM.

library(car)
fit <- lm(cyl ~ ., data = mtcars)
vif(fit)
