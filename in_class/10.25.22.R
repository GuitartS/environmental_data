x_observed = c(2, 6)
print(x_observed)
dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
prod(dpois(x = wiwa_counts, lambda = 4.5))
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))
dat_bird = read.csv(here::here("data", "bird.sta.csv"))
dat_habitat = read.csv(here::here("data", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)
summary(dat_all$WIWA)
hist(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 7)
hist(dat_all$WIWA, breaks = 0:7)
hist(dat_all$WIWA, breaks = 0:7 - .5)
par(mfrow = c(1, 2))
dat = dat_all$WIWA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")
dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")

sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
sum(log(dpois(x = wiwa_counts, lambda = 4.5)))
sum(log(dpois(x = wiwa_counts, lambda = 5.0)))
sum(log(dpois(x = wiwa_counts, lambda = 6.0)))
sum(log(dpois(x = wiwa_counts, lambda = 4.0)))
sum(log(dpois(x = wiwa_counts, lambda = 3.5)))
sum(log(dpois(x = wiwa_counts, lambda = 3.75)))

dev.off()
hist(dat_all$WIWR)
dat1 = dat_all$WIWR
hist(dat1, breaks = 0:(max(dat1) + 1) - 0.5, main = "Histogram of\nWinter Wren counts",
     xlab = "Winter Wren counts")

sum(log(dpois(x = dat_all$WIWR, lambda = 1.5)))

length(dat1)
length(dat_all$WIWR)

dbinom(dat1, 1046, 0.5)
sum(log(dbinom(dat1, 1046, 0.001)))


set.seed(1)
vec_rnorm = rnorm(n = 10, mean = 0, sd = 1)
sum(log(dnorm(vec_rnorm, mean = 0.5, sd =1)))
sum(log(dnorm(vec_rnorm, mean = 0.0, sd =1.5)))

library(palmerpenguins)
mean(penguins$flipper_length_mm, na.rm = TRUE)
sd(penguins$flipper_length_mm, na.rm = TRUE)

mean <- mean(penguins$flipper_length_mm, na.rm = TRUE)
sd <- sd(penguins$flipper_length_mm, na.rm = TRUE)

sum(log(dnorm(penguins$flipper_length_mm[!is.na(penguins$flipper_length_mm)], 
              mean=200, sd=14))) 

penguins$flipper_length_mm[!is.na(penguins$flipper_length_mm)]

sum(log(dnorm(penguins$flipper_length_mm[!is.na(penguins$flipper_length_mm)], 
              mean=200, sd=13))) 
sum(log(dnorm(penguins$flipper_length_mm[!is.na(penguins$flipper_length_mm)], 
              mean=200.9, sd=14.1))) 
