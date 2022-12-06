## lab 8 walk through
library(palmerpenguins)
head(penguins, 4)
dat_pen <- droplevels(subset(penguins, species != "Gentoo"))
head(dat_pen,3)
nrow(dat_pen)
str(dat_pen)
t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "less")
require(simpleboot)
install.packages("simpleboot")
library(simpleboot)
x <- (droplevels(subset(dat_pen, species == "Adelie")))
str(x)
y <- (droplevels(subset(dat_pen, species == "Chinstrap")))
str(y)
pen_boot <- two.boot(x$flipper_length_mm, y$flipper_length_mm, FUN = mean, R = 10000,
                     na.rm = TRUE)
str(pen_boot)
pen_boot[["t"]]

hist(pen_boot$t)
library(here)
veg <- read.csv(here("data", "vegdata.csv"))
head(veg, 3)
str(veg)
sum(is.na(veg))
boxplot(pine ~ treatment, dat = veg)
dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, data = dat_tree)
table(dat_tree$treatment)
wilcox.test(pine ~ treatment, data = dat_tree, na.rm = TRUE)
boot.ci(tree_boot)
plot(ecdf(pen_boot$t))
pen_ecdf <- ecdf(pen_boot$t)
1 - pen_ecdf(-4.5)
pen_ecdf(-8)
t.test(flipper_length_mm ~ species, data = dat_pen, alternative = "two.sided")
hist(tree_boot$t)
#### questions 12-20
dat_bird <- read.csv(here("data", "bird.sub.csv"))
dat_hab <- read.csv(here("data", "hab.sub.csv"))
dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])
# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

s_sidi_mean = mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd   = sd(dat_all$s.sidi, na.rm = TRUE)
s_sidi_mean
s_sidi_sd
dat_all$s.sidi.standardized = (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd
mean(dat_all$s.sidi.standardized)
sd(dat_all$s.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

m = 10000 
result_mc = numeric(m) 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2])
    
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result_mc[i] = coef(fit_resampled_i)[2]
} 
hist(result_mc)
abline(v = slope_observed)
quantile(result_mc, 0.05)

## bootstrap loop
set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
coef(fit_bs1)


result_boot = numeric(m) 
for(i in 1:m)
{
  index_boot_1 = sample(nrow(dat_1), replace = TRUE)
  
  boot_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_boot_1],
      s.sidi = dat_1$s.sidi[index_boot_1])
  
  fit_boot_i = lm(b.sidi ~ s.sidi, data = boot_resampled_i)
  result_boot[i] = coef(fit_boot_i)[2]
} 
hist(result_boot)
