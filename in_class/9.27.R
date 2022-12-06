getwd()
dat_bird <- read.csv("bird.sta.csv")
install.packages("here")
require("here")
dat_bird <- read.csv(here("data", "bird.sta.csv"))
dat_hab <- read.csv(here("data", "hab.sta.csv"))
                     
require(palmerpenguins)
pairs(penguins)
pairs(penguins[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])

pairs(dat_hab[, c("slope", "lat", "long", "elev")])
hist(dat_bird$GCKI)

hist(dat_bird$HOME, breaks = 0:7 - 0.5, xlab = "Number of birds counted", main = "Histogram of Footed Merganser Abundance") 
