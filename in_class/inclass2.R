data(iris)
head(iris)
iris$Sepal.Width
mean(iris$Sepal.Length)
sd(iris$Sepal.Width)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")


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
line_point_slope(2, 4, 4, -2)

plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -0.1), 
  add = TRUE)


plot(x = iris$Sepal.Width, y = iris$Sepal.Length, main = "Sarah's Plot")
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    1), 
  add = TRUE)

library(MASS)
data(Animals)
head(Animals)

Animals$pig

x3 = mean(Animals$body)
y3 = mean(Animals$brain)
plot(x = Animals$body, y = Animals$brain, main = "Sarah's Animal Farm", 
     xlab = "Animal Body Mass", ylab = "Animal Brain Mass")
points(x = x3, y = y3, col = "red")
curve(
  line_point_slope(
    x, 
   x3, 
   y3,
    1), 
  add = TRUE)
