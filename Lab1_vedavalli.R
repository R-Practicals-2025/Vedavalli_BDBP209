#Lab1 Jan 3 2025
#Q1.1
x <- 2.7/2
print(x)
#Q1.2
x <- 2.7%/%2
print(x)
#Q1.3
x <- 10+5i/2
print(x)
#Q1.4
x <- round(2.5)
print(x)
#Q1.5
x <- round(-2.5)
print(x)
#Q1.6
x <- 2%/%4-1
print(x)
#Q1.7
x <- 3*2**2
print(x)
#Q1.8
x <- 3**2*2
print(x)
#Q1.9
x <- 7%/%4
print(x)
#Q1.10
x <- 7%%4
print(x)
#Q1.11
x <- -7%%4
print(x)
y <- -9%%4
print(y)
#Q1.12
x <- trunc(5.7)
print(x)
#Q1.13
x <- trunc(-5.7)
print(x)

#Q2
#ceiling(5.7) == floor(5.7+0.5)
ceil <- function(x) floor(x+0.5)
ceil(5.7)
#Q3
a<-1
b<-2 
c<-4
#Q3.1
x <- a & b
print(x)
#Q3.2
y= !(a < b) || (c > b)
print(y)

#Q4
#Q4.1
x <- c(5,3,7,8)
print(x)
#Q4.2
is.integer(x)
#Q4.3
is.numeric(x)
#Q4.4
x <- c(5, 3, 7, 8)
x <- integer(x)
print(x)
#this gives an error- invalid length error, because integer() function expects the "length" as argument,such as a 10 length integer vector-integer(10) provides an integer vector of length 10

#Q4.5
x <- as.integer(x)
is.integer(x)
#as.integer() converts numeric vector to integer vector, so is.integer(x) returns TRUE

#Q5
#Q5.1
x <- sqrt(2)
print(x)
#Q5.2
x*x ==2
#gives FALSE, the issue is because of the floating-point precision. Because real and irrational numbers can't be represented exactly in a computer, they are approximated.
#Q5.3
x <- sqrt(2)
x*x - 2 
#this is also because of the floating-point precision.The value it returns is very close to zero, but not zero.
#Q5.4
all.equal(x*x,2)


