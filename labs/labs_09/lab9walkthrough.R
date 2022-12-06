## lab 9 walk through

library(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)
str(catrate)
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(
  x = n_success,
  n = n_years,
  p = 0.5)
n_success
n_years
late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate
binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate) 

binom.test(
  x = n_success,
  n = n_years,
  p = normal_fill_rate,
  alternative ='less')

t.test(catrate$cat.rate, mu = 2/7)

veg = read.csv(here("data", "vegdata.csv"))
head(veg)
boxplot(pine ~ treatment, data = veg)

veg2 = droplevels(
  subset(
    veg,
    treatment %in% c('control','clipped')
  ))

# verify that treatment is factorized
veg2$treatment = factor(veg2$treatment)
var.test(
  pine ~ treatment,
  data = veg2)

shapiro.test(veg2$pine[veg2$treatment=="control"])
shapiro.test(veg2$pine[veg2$treatment=="clipped"])

fligner.test(
  pine ~ treatment,
  data = veg2)
bartlett.test(pine ~ treatment, data = veg)
fligner.test(pine ~ treatment, data = veg)

t.test(
  pine ~ treatment,
  data = veg2)

wilcox.test(
  pine ~ treatment,
  data = veg2)

install.packages("datarium")
require(datarium)
data("mice2")
head(mice2)
str(mice2)
t.test(mice2$before, mice2$after, paired = TRUE)

shapiro.test(mice2$before)
shapiro.test(mice2$after)

wilcox.test(mice2$before, mice2$after)
t.test(mice2$before, mice2$after, paired = FALSE)
disp = read.csv(here("data", "dispersal.csv"))
disp
plot(
  disp.rate.ftb ~ disp.rate.eb,
  data = disp,
  main = "Marbled Salamander Dispersal Rates",
  xlab = "Dispersal Rate\nFirst Time Breeders",
  ylab = "Dispersal Rate\nExperienced Breeders",
  pch = 21, col = 1, bg = "steelblue"
)
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')
cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE,
  main = "Mike's Plot of Marbled Salamanders\nFirst-Time and Experienced Breeders: ECDF")
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)
legend(
  x = 0.4, y = 0.4,
  lty = c(1, 3),
  legend = c("first-time", "experienced"),
  title = "Breeder Class")
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)
prop.test(
  x = c(4,16),
  n = c(40,250))
owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq_owls = chisq.test(owls)
chisq_owls
round(chisq_owls$expected, 1)
chisq_owls$observed
round(
  chisq_owls$observed - chisq_owls$expected,
  digits = 1)
fisher.test(owls)

birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(
  birds,
  hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(
  birdhab$s.edge,
  birdhab$BRCR > 0)

br_creeper_table = table(
  birdhab$s.edge, 
  birdhab$BRCR > 0)[, 2:1]

br_creeper_table

dat_groups = aggregate(
  body_mass_g ~ island,
  data = penguins, FUN = c)
str(dat_groups)
require(palmerpenguins)
dat_groups2 = aggregate(
  body_mass_g ~ species * sex,
  data = penguins,
  FUN = c)
str(dat_groups2)
bartlett.test(dat_groups2$body_mass_g)


dat_fl$fail = factor(dat_fl$Failure_Standardized != "none")

levels(dat_fl$fail) = c("No Fail", "Fail")
fl_table_2 = table(
  dat_fl$ProbabilityofFailure,
  dat_fl$fail)
fl_table_2
