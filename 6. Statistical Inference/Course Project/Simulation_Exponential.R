#1-1. Show the sample mean and compare it to the theoretical mean of the distribution.
lambda <- 0.2

Simul_rexp <- NULL
for (i in 1:1000){Simul_rexp <- c(Simul_rexp, mean(rexp(40, lambda)))}

Simul_mean <- mean(Simul_rexp)
Theo_mean <- 1 / lambda

matrix(data = c(Simul_mean, Theo_mean), nrow = 1, ncol = 2, 
       dimnames = list("Result", c("Mean of Simulations", "Theoretical Mean")))

#1-2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
Simul_var <- var(Simul_rexp)
Theo_var <- 1 / lambda

matrix(data = c(Simul_mean, Theo_mean), nrow = 1, ncol = 2, 
       dimnames = list("Result", c("Variance of Simulations", "Theoretical Variance")))

#1-3. Show that the distribution is approximately normal.
hist(Simul_rexp, main = "Histogram of Exponential Simulations and line of Normal Distribution")
clt <- seq(0, 10, length = 1000)
par(new = T)
plot(clt, dnorm(clt, mean = Theo_mean, sd = sqrt(Theo_var)), 
     xlab = "", ylab = "", type = "l", axes = F, col = "red")

