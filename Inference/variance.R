nosim <- 1000
n <- 10
asample <- sample(0 : 1, nosim * n, replace = TRUE)
# 1000 sets del resultado de 10 tiradas de moneda
amatrix <- matrix(asample, nosim)
# Promedio de cada experimento de 10 tiradas de moneda
means <- apply(amatrix, 1, mean)
# Calculo el desvio de los promedios
sdevi <- sd(means)
# Fair coin flips have variance 0.25; means of random samples of n coin flips have sd 1 / (2 * sqrt(n))
1 / (2 * sqrt(n))
# The number of web hits to a site is Poisson with mean 16.5 per day.
# What is the probability of getting 20 or fewer in 2 days expressed as a percentage to one decimal place?
ppois(20, lambda = 16.5 * 2)
