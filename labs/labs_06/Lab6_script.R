sse_mean = function(x)
{
  return(sd(x, na.rm = TRUE) / sqrt(length(x)))
}
require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)
rm(list = ls())
sse_mean = function(x)
{
  return(sd(x, na.rm = TRUE) / sqrt(length(x)))
}
sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

dat_pen = droplevels(subset(penguins, species != "Chinstr"))
boxplot(flipper_length_mm ~ species, data = dat_pen,
    ylab = "Flipper length (mm)")
dev.off()

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)")
}

# for reproducibility
set.seed(123)

flipper_shuffled = sample(
  penguins$flipper_length_mm, replace = TRUE)

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_shuffled ~ penguins$species,
    ylab = "Flipper length (mm)",
    main = "MonteCarlo Resampled Data",
    xlab = "species")
}

penguins2 = penguins[sample(1:nrow(penguins), replace = T), ]

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_length_mm ~ species, data = penguins2,
    ylab = "Flipper length (mm)",
    main = "Bootstrap Data")
}

t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

two_group_resample_diff = function(x, n_1, n_2)
{
 dat_1 = sample(na.omit(x), n_1, replace = TRUE)
 dat_2 = sample(na.omit(x), n_2, replace = TRUE)
 diff_sim = mean(dat_1, na.rm = T) - mean(dat_2, na.rm = T)
   return(diff_sim)
}
set.seed(54321)
two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)


n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)
sum(abs(mean_differences) >= diff_observed)

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
sum(abs(mean_differences) >= diff_observed)
