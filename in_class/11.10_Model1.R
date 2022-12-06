## lecture assignment walk through 
library(here)
catrate <- read.csv(here("data", "catrate.csv"))
head(catrate)
summary(catrate)
hist(catrate$cat.rate)
shapiro.test(catrate$cat.rate)
install.packages("nortest")
library(nortest)
t.test(catrate$cat.rate, mu = 2/7)
t.test(catrate$cat.rate, mu = 2/7, alternative = "greater")
t.test(catrate$cat.rate, mu = 2/7, alternative = "less")
wilcox.test(catrate$cat.rate, mu = 2 / 7)

summary(penguin_dat)
boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)")
dat_adelie = droplevels(subset(penguin_dat, species == "Adelie"))
dat_chinstrap = droplevels(subset(penguin_dat, species == "Chinstrap"))
str(dat_adelie)
str(dat_chinstrap)

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)
