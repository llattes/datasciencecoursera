library(BSDA)
# 40 valores generados a propósito para tener media 22.5 y sd ~4.5.
x <- rep(x = c(18,27), times = 20)
mean(x)
sd(x)
# Hago el test de hipótesos para H0: X = 25 a 2 colas.
z.test(x = x, alternative = "two.sided", mu = 25, conf.level = 0.95, sigma.x = sd(x))

# One-sample z-Test
# 
# data:  x
# z = -3.4694, p-value = 0.0005215
# alternative hypothesis: true mean is not equal to 25
# 95 percent confidence interval:
#   21.0877 23.9123
# sample estimates:
#   mean of x 
#        22.5

# Generamos un set de datos similar al del problema de jbstatistics.
data <-rnorm(n = 288, mean = -0.064, sd = 0.183)
summary(data)
t.test(x = data, alternative = "two.sided", mu = 0)

# One Sample t-test
# 
# data:  data
# t = -5.4339, df = 287, p-value = 1.178e-07
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.08098521 -0.03791676
# sample estimates:
#   mean of x 
# -0.05945098 

## A really small p-value gives really strong evidence against the null hypothesis.
## Los grados de libertad (df) son iguales a n - 1, en este caso 287.
## Una distribución 't' con una muestra de 288 tiene un gráfico aproximadamente normal.
qt(.975, df = 287) - qnorm(.975)


