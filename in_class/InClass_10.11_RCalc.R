dpois(x = 7, lambda = 10.4)
dpois(x = 8, lambda = 10.4)
# Standard normal has mean = 0 and sd = 1
dnorm(0.5, mean = 0, sd = 1)
## [1] 0.3520653
dnorm(1, mean = 0, sd = 1)
## [1] 0.2419707
pnorm(0.5, mean = 0, sd = 1)
## [1] 0.6914625

# How many points?
n = 13

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l")

# How many points?
n = 1000

# Create a vector of x-values from -4 to 4:
x = seq(from = -6, to = 6, length.out = n)

# Create the corresponding y-values:
y = dnorm(x, mean = 0, sd = 1)

# plot!
plot(y ~ x, type = "l", ylab = "Probability Density")

y_2 = dnorm(x, mean = 0, sd = 2)
plot(y ~ x, type = "l", ylab = "Probability Density")
points(y_2 ~ x, type = "l", lty  = 2)

y_3 = dnorm(x, mean = -2, sd = 1)
points(y_3 ~ x, type = "l", lty  = 2)

