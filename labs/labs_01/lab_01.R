c(1,2,3)
"c(1,2,3)"

#A1: The outputs of the line are different as c(1,2,3) creates a vector
#containing the numbers 1,2,3 while placing commas around "c(1,2,3)" 
#makes the entire contents it's own string. 

c_1=c(1,2,3)
c_1
c_2 <- "c(1,2,3)"
class(c_2)
class(c_1)
 
my_vec<-c(1,2,3,4,5,6)
my_vec
mat_1 <- matrix(my_vec, nrow = 3)
mat_1
mat_1[3,1]
mat_2 <- matrix(my_vec, nrow = 2, ncol = 3)
mat_2
mat_3 <- matrix(my_vec, nrow = 3, ncol = 2)
mat_3

mat_4 <- matrix(my_vec, nrow = 4)

ma

my_list_1 <- list("two"=5.2, "one"="five point two", "three"= 0:5)
my_list_1

my_list_1[[1]]
my_list_1[[as.numeric("1")]]
my_list_1[["1"]]
my_list_1[["one"]]
my_list_1$one
my_list_1$"one"
my_list_1$1
my_list_1$"1"

