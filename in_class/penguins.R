install.packages("palmerpenguins")
install.packages("here")
require("palmerpenguins")
require("here")
library("palmerpenguins")
class(penguins)
penguins = data.frame(penguins)
mean(penguins$body_mass_g)
head(penguins)
mean(penguins$body_mass_g, na.rm = TRUE)
summary(penguins)
plot(penguins$species, penguins$bill_depth_mm)

boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)

coplot(body_mass_g ~ bill_depth_mm | species, data = penguins, rows = 1,
       xlab = c("Bill Depth (mm)", "Species"), ylab = "Body Mass (g)")
