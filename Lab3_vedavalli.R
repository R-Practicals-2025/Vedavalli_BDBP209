#Lab3 - 13th January 2025

#Ex4
#Ex 4.1- Vectors
vec <- c(4,7,6,5,6,7)
#class, min and max of vectors
print(class(vec))
print(length(vec))
print(min(vec))
print(max(vec))

#Ex4.2
vec <- scan() #creation of vector using keyboard input
print(vec) 

#Ex4.3
#NOTE: indexing starts from 1 in R
vec[4] #Accessing the 4th element

#Ex 4.4
#Extracting multiple elements using two ways:
ind <- c(2,3,6)
vec[ind] #or
vec[(c(2,3,6))]

#Ex 4.5
#dropping elements using - symbol
vec[-1]
print(vec[-1]) #drops the 1st element 
print(vec[-2]) #drops the 2nd element and so on..

#Ex 4.6
vec[-length(vec)]
#this removes the last element in the vector

#Ex 4.7
#trim function to remove largest two and smallest two values
trim <- function(x) {
  sorted_x <- sort(x)
  trimmed_x <- sorted_x[3:(length(sorted_x)-2)] #leaving the 1st two indices and the last two.
}
print(trim(vec))
#another way
a_trim <- function(x){
  sorted_x <- sort(x)
  trimmed_x <-[c(-1,-2,-(length(sorted_x)), -(length(sorted_x)-1))]
}
trim(vec)

#Ex 4.8
#using sequences to extract elements
vec[1:3] #extract 1st 3 elements
#other way to achieve this
head(vec,3)

vec[seq(2,length(vec),2)] #syntax- seq(starting index,stopping index, skips by)
#other ways to achieve this
vec[-seq(1,length(vec),2)] #using -ve indexing
vec[seq(vec)%% 2 == 0] #using modulo


#Ex 4.9
x <- 0:10 #creates a vector x with elements 0 to 10
#using logical subsetting- using logical vectors to filter specific elements based on the condn
sum(x[x < 5]) #sum of elements in x which is less than 5

#using which function to find the indices
sum(x[which(x<5)])

#using logical condition
sum(x*(x<5))

#Ex 4.10
#sum of 3 largest values in y
y <- c(5,3,4,56,73,200,38)
sorted_y= sort(y,decreasing=TRUE)
sum(sorted_y[1:3])

#Ex 4.11
#finding index of vector (min,max)
y <- c(5,3,4,56,73,200,38)
which.min(y)
which.max(y)

#Ex 4.12
#combining vectors as columns or rows
cbind(1:10,10:1) #combine vectors as columns
rbind(1:10,10:1) #combine vectors as rows

#Ex 4.13
#Vector operations
X <- c(1:10)
X
Y <- c(1:10*5)
Y
#multiplication
X*Y
#addition
X+Y
#division
X/Y
#exponential
X^Y
#logarithmic
log(X)
#exponential
exp(Y)

#Ex 5- Matrices, Frames, Arrays
#building a matrix
y <- 1:24
dim(y) <- c(2,4,3) #two rows,four cols, 3 matrices(2x4x3)
y
#changing the dimensions of matrix y to (3x2x4)
dim(y) <- c(3,2,4) #three rows, two cols, 4 matrices
y
#Ex 5.1
#blank subscripts- all of a certain dimension
X <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3)
print(X)

#Ex 5.2
#another way to convert a vector to matrix- using dim function
vector <- c(1,2,3,4,4,3,2,1)
#method 1
V <- matrix(vector,byrow=T,nrow=2) 
print(V)
#method 2
dim(vector) <- c(4,2)
is.matrix(vector)

#Ex6 - Vector functions
#Ex 6.1
vec <- c(15,9,32,3)
min(vec)
max(vec)
sum(vec)
range(vec)
sort(vec)

#Ex 6.2
M <- matrix(1:6, nrow=2, ncol=3)
M
colMeans(M) #this calculate the column means of the matrix

#Ex 6.3
#Matrix ops
X <- c(7,8,9,10)
Y <- c(2,5,4,3)
Z <- X[1:4] %o% Y[1:3]
YoX <- X[1:3] %o% Y[1:4]
print(Z) #outer product, each element is multiplied by the corresponding element
YoX

#transpose
t(Z)
t(YoX)

#dot product
X %*% Y #1st method
sum(X*Y) #another way to carry out dot pdt

#cross product between X and Z
crossprod(X[1:4],Z) 

#Identity matrix of size 4 (4x4)
diag(4)

#Checking the Class of X
class(X) #

