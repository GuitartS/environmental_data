## In class assingment 10.11.22 - In-Class Probability 3: Calculations In R
dpois(x = 7, lambda = 10.4)
dpois(x = 8, lambda = 10.4)
dpois(x = 10, lambda = 10.4)
dpois(x = 11, lambda = 10.4)

# Standard normal has mean = 0 and sd = 1
dnorm(0.5, mean = 0, sd = 1)
## [1] 0.3520653
dnorm(1, mean = 0, sd = 1)
## [1] 0.2419707

pnorm(0.5, mean = 0, sd = 1)

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
points(y_3 ~ x, type = "l", lty  = 4)

y_cdf_1 = pnorm(x, mean = 0, sd = 1)
plot(y_cdf_1 ~ x, type = "l", ylab = "cumulative density")

y_cdf_2 = pnorm(x, mean = 0, sd = 2)
plot(y_cdf_1 ~ x, type = "l", ylab = "Cumulative Density")
points(y_cdf_2 ~ x, type = "l", lty = 2)

y_cdf_3 = pnorm(x, mean = -2, sd = 1)
points(y_cdf_3 ~ x, type = "l", lty = 4)

## SUBMISSION
n = 1000
x = seq(from = -6, to = 6, length.out = n)
y = dnorm(x, mean = 0, sd = 1)
y_2 = dnorm(x, mean = 0, sd = 2)
y_3 = dnorm(x, mean = -2, sd = 1)
y_cdf_1 = pnorm(x, mean = 0, sd = 1)
y_cdf_2 = pnorm(x, mean = 0, sd = 2)
y_cdf_3 = pnorm(x, mean = -2, sd = 1)
par(mfrow = c(1, 2))
plot(y ~ x, type = "l", ylab = "Probability Density", main = "Our Plot")
points(y_2 ~ x, type = "l", lty  = 2)
points(y_3 ~ x, type = "l", lty  = 4)
plot(y_cdf_1 ~ x, type = "l", ylab = "Cumulative density", main = "Our Plot")
points(y_cdf_2 ~ x, type = "l", lty = 2)
points(y_cdf_3 ~ x, type = "l", lty = 4)
dev.off()

x_bin = 0:5
y_bin_2 = dbinom(x_bin, size = 5, prob = 0.4)
barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 5, p = 0.4")

x_bin = 0:5
y_bin_2 = dbinom(x_bin, size = 5, prob = 0.5)
barplot(
  height = y_bin_2,
  # the names to print with each bar:
  names.arg = x_bin,
  # Tells R to remove space between bars:
  space = 0,
  ylab = "Pr(x)",
  main = "Binomial: n = 5, p = 0.4")

x_bin = 0:5
y_bin_1 = dbinom(x_bin, size = 6, prob = 2/3)
barplot(
  height = y_bin_1,
  names.arg = x_bin,
  space = 0,
  ylab = "Pr(x)",
  main = "Our Binomial: n = 6, p = 2/3")

dbinom(x = 4, size = 6, prob = 2/3)

dbinom(x = 0, size = 6, prob = 2/3)

pbinom(q = 4, size = 6, prob = 2/3)

1 - pbinom(q = 4, size = 6, prob = 2/3)

pnorm(q = 1, mean = 0, sd = 1)

pnorm(q = 2, mean = 0, sd = 1) - pnorm(q = 1, mean = 0, sd = 1)
