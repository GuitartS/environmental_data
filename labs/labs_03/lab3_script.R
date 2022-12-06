# Walking through the lab exercises

install.packages("psych")
require(psych)
pairs.panels(iris)
names(iris)
pairs.panels(iris[c("Sepal.Length", "Sepal.Width", "Petal.Length")])

install.packages("here")
require(here)
here()

dat_bird = read.csv(here("data", "bird.sta.csv"))
head(dat_bird, 2)

dat_hab = read.csv(here("data", "hab.sta.csv"))
head(dat_hab, 2)
str(dat_bird)
str(dat_hab)

dat_all <- merge(dat_hab, dat_bird)
dat_all

plot(ba.tot ~ elev, data = dat_all)

sample(dat_all$CEWA, 100)

sum(dat_all$CEWA)

sum(dat_all$CEWA > 0)

my_vec1 <- dat_all$CEWA > 0 
my_vec1
sum(my_vec1)

as.numeric(my_vec1 > 0)
cewa_present_absent <- as.numeric(my_vec1 > 0)
plot(x = dat_all$elev, y = cewa_present_absent)


# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

pairs.panels(dat_all[c("elev", "slope", "aspect", "ba.tot")])

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = 0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)

### Starting work on the lab questions 

## bird species 1
my_vec2 <- dat_all$GCKI > 0 
sum(my_vec2)
gcki_present_absent <- as.numeric(my_vec2 > 0)
plot(x = dat_all$ba.tot, y = gcki_present_absent)
plot(x = dat_all$ba.con, y = gcki_present_absent)
plot(x = dat_all$ba.hard, y = gcki_present_absent)
plot(x = dat_all$ba.snag, y = gcki_present_absent)
plot(x = dat_all$elev, y = gcki_present_absent)
plot(x = dat_all$slope, y = gcki_present_absent)
plot(x = dat_all$aspect, y = gcki_present_absent)
plot(x = dat_all$p.edge.1, y = gcki_present_absent)

## bird species 2
my_vec3 <- dat_all$AMRO > 0 
sum(my_vec3)
amro_present_absent <- as.numeric(my_vec3 > 0)
plot(x = dat_all$ba.tot, y = amro_present_absent)
plot(x = dat_all$ba.con, y = amro_present_absent)
plot(x = dat_all$ba.hard, y = amro_present_absent)
plot(x = dat_all$ba.snag, y = amro_present_absent)
plot(x = dat_all$elev, y = amro_present_absent)
plot(x = dat_all$slope, y = amro_present_absent)
plot(x = dat_all$aspect, y = amro_present_absent)
plot(x = dat_all$p.edge.1, y = amro_present_absent)

## pair plots of terrain and basal area 
pairs.panels(dat_all[c("slope", "aspect", "elev", "ba.tot")])
