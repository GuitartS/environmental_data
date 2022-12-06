## Lab 4 script and practice 
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)
plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)

rnorm(3)

require(palmerpenguins)
hist(penguins$body_mass_g,
     main = "Histogram of Penguin Body Mass",
     xlab = "Body Mass (g)")
#to get random numbers from the data, need the mean, sd,
# and number of observations
mean(penguins$body_mass_g, na.rm = T)
sd(penguins$body_mass_g, na.rm = T)
nrow(penguins)
n_pts = 344
penguin_mean = 4202
penguin_sd = 802
#use soft code to generate 4 vectors of random penguin masses
dat_1 = rnorm(n = n_pts, mean = penguin_mean, sd = penguin_sd)
dat_2 = rnorm(n = n_pts, mean = penguin_mean, sd = penguin_sd)
dat_3 = rnorm(n = n_pts, mean = penguin_mean, sd = penguin_sd)
dat_4 = rnorm(n = n_pts, mean = penguin_mean, sd = penguin_sd)

par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

#runif() will generate random, uniformaly-distributed numbers 
dat_unif = runif(n = 27, min = 0, max = 4)
hist(dat_unif)
# but it's not flat at all!
# look at the sample size, smaller samples are prone to 
#sampling error
dat_unif1 = runif(n = 270, min = 0, max = 4)
hist(dat_unif1)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

## residuals are the difference between a predicted value and 
## the observed value 

line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

# Creat the data


n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
set.seed(1)
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(dat_random$x, guess_x, guess_y, guess_slope))

# Can calculate the predicated values (the line) based on my estimated model 
# parameters
line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
dat_random$y_predict <- line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)
head(dat_random)
dat_random$resids <- dat_random$y_predict - dat_random$y
head(dat_random)
sum(dat_random$resids)

## next section 
guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

#lab questions
mean1 <- 10.4
sd1 <- 2.4 

norm_17 = rnorm(n = 17, mean = mean1, sd = sd1)
norm_30 = rnorm(n = 30, mean = mean1, sd = sd1)
norm_300 = rnorm(n = 300, mean = mean1, sd = sd1)
norm_3000 = rnorm(n = 3000, mean = mean1, sd = sd1)

par(mfrow = c(2, 2))
hist(norm_17, main = "Histogram of 17 Randomly Generated Points",
     xlab = "Point Values")
hist(norm_30, main = "Histogram of 30 Randomly Generated Points",
     xlab = "Point Values")
hist(norm_300, main = "Histogram of 300 Randomly Generated Points",
     xlab = "Point Values")
hist(norm_3000, main = "Histogram of 3000 Randomly Generated Points",
     xlab = "Point Values")


