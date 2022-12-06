a = c(T, F, F)
b = c(T, T, F)
c = c(T, T, F, T)

a & b
a & c
a && b

class(4)
class(4.0)
1.0 == TRUE
3.0000000000000001 == 3.0

(0 + 1) == TRUE
(0 - 1) == TRUE
3.0 * TRUE
4 / FALSE
FALSE / FALSE
3.0 * (TRUE -FALSE)
(TRUE - FALSE)
(FALSE - FALSE)
TRUE / 5

## high level programming language

?paste()
int_rnd = sample(100, 1)
int_rnd_sentence = paste0("The value of the randomly-generated number is:", 
                          int_rnd)
int_rnd_sentence2 = paste("The value of the randomly-generated number is:", 
                          int_rnd)
int_rnd_sentence
int_rnd_sentence2

print(paste0("The value of the randomly-generated number is: ", 
             sample(100, 1)))

## loops

for(i in 1:10)
{print(i)}


## custom functions

print_number = function(n)
{
  print(paste0("The value of the number is ", n))
}

print_number(145)

## arguments 

rnorm(10)
rnorm(n = 10, sd = 1)
rnorm(sd = 1, mean = 0, n = 10)
