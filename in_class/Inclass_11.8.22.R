data(iris)
fit_species = 
  lm(
    Sepal.Length ~ Species,
    data = iris)
summary(fit_species)
boxplot(Sepal.Length ~ Species, data = iris)
residuals(fit_species)
hist(residuals(fit_species))
shapiro.test(residuals(fit_species)) 
plot(
  Petal.Width ~ Petal.Length,
  data = iris,
  xlab = "Petal Length (cm)",
  ylab = "Petal Width (cm)")
fit_petal = lm(Petal.Width ~ Petal.Length, data = iris)
summary(fit_petal)
shapiro.test(residuals(fit_petal))
