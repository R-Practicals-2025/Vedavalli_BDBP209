#Feb 14 2025
#Lab exercises
#(1)i) Solving matrix equations and review of matrix operations.
amat1<- matrix(seq(10,120, by=10), nrow=4, ncol=3, byrow=T)
print(amat1)
#
amat2<- matrix(seq(10,120, by=10),nrow=3, ncol=4, byrow=F)
print(amat2)
#checking the relation btw two matrices
print(t(amat1) == amat2) #by changing the dimensions
#
#(ii) Assign the row names to amat as R1, R2 and R3 and column names as C1, C2, C3, C4.Print 
#the matrix again to see that the assignments have been made.
# Create the matrix
amat <- matrix(seq(10, 120, by=10), nrow=3, ncol=4, byrow=TRUE)

# Assign row and column names
rownames(amat) <- c("R1", "R2", "R3")
colnames(amat) <- c("C1", "C2", "C3", "C4")
print(amat)

#(iii) Form matrix A and B with the following elements.Perform element wise multiplication, matrix-matrix multiplication

A <- matrix(c(2,5,7,3, 1,8,9,10, 1,12,5,10, 4,17,15,11), nrow=4, ncol=4, byrow=TRUE)

B <- matrix(c(12,5,3,17, 1,18,9,10, 1,12,5,10, 4,15,15,4), nrow=4, ncol=4, byrow=TRUE)
print("Matrix A:")
print(A)
print("Matrix B:")
print(B)
# Element-wise multiplication
elementwise_result <- A * B
print("Element-wise multiplication (A * B):")
print(elementwise_result)
# Matrix multiplication
matrix_mult_result <- A %*% B
print("Matrix-Matrix Multiplication (A %*% B):")
print(matrix_mult_result)


#(iv) Define a vector X with elements 5,6,8,9 and vector Y with elements 8,10,12,5.
#Obtain the outer product of the two vectors and print the result. Also obtain the
#inner product of the two vectors and print the result.

X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)
outer_product <- X %o% Y #outer pdt
inner_product <- X %*% Y #inner pdt,dot product
print("Outer Product (X %o% Y):")
print(outer_product)

print("Inner Product (X %*% Y):")
print(inner_product) 

#(v) Form a diagonal matrix using the entries of the vector X above.
X <- c(5, 6, 8, 9)
# Create a diagonal matrix
diag_matrix <- diag(X)
print("Diagonal Matrix:")
print(diag_matrix)

#vi)Print the diagonal elements of A.
A <- matrix(c(2,5,7,3, 1,8,9,10, 1,12,5,10, 4,17,15,11), nrow=4, ncol=4, byrow=TRUE)
diagonal_elements <- diag(A)
print("Diagonal Elements of A:")
print(diagonal_elements)

#(vii) Create an identity matrix of dimensions 6x6 in one line.
Id_matrix <- diag(6)
print(Id_matrix)

#(viii) Create a 3x3 matrix A using the elements 3,4,-2,4,-5,1,10,-6,5 with default options.Print A.
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
print(A)

#(ix) Create a 3x1 matrix B with elements 5,-3,13. Print B.
B <- matrix(c(5, -3, 13), nrow=3, ncol=1)
print(B)

#(x) Find the unknown vector X of the equation AX=B using the command X=solve(A,B).Print X to see the results. What type of object is X?
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
B <- matrix(c(5, -3, 13), nrow=3, ncol=1)
X <- solve(A, B)
print(X)
print(typeof(X)) #type of X

#(xi) Find the inverse of matrix A by using the command Ainv = solve(A). Print the inverse. Check that this is indeed the inverse by
# calculating the matrix product of Ainv and A matrix and print the result. Are you getting an identity matrix?

A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)
Ainv <- solve(A) #inverse
print(Ainv)

# Verify by multiplying Ainv with A
Identity_matrix <- Ainv %*% A
print("Ainv * A (Should be Identity Matrix):")
print(Identity_matrix)

#

# Define matrix A
A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow=3, ncol=3)

results <- eigen(A)
print(results$values) # eigenvalues
print(results$vectors) #eigenvectors

print(class(results)) #type of result
#the second eigenvector
second_eigenvector <- results$vectors[,2]

#matrix-vector multiplication A * (second eigenvector)
multiplication_result <- A %*% second_eigenvector
print(multiplication_result)

#Compare it with lambda * eigenvector
lambda2 <- results$values[2]
expected_result <- lambda2 * second_eigenvector
print(expected_result)

#when we multiply the matrix A  with its second eigenvector, the result is 
#exactly the same as multiplying that eigenvector by its corresponding eigenvalue.This proves that
#the computed eigenvalues and eigenvectors are correct.

#(2) Removing a column from a data frame. Read in the file “BrainCancer.csv” and store the data as a dataframe.
#(i) Create a new column of data that calculates the following: In a given row, the
#entry from ’GTV’ column should be squared and added to the ’time’ column entry
#in that row. Add this column to the existing data frame after giving it a column
#name.

df <- read.csv("/home/ibab/Downloads/BrainCancer.csv")
df$new_column <- df$gtv^2 + df$time
head(df)
#(ii) Print Row and Column Names
print(rownames(df))
print(colnames(df))
#(iii) Change the row names of the data such that each row has ’Row-’ followed by the row number. Use the paste function for this.
rownames(df) <- paste("Row", 1:nrow(df), sep="-")
print(rownames(df))
#(iv) Remove the 'ki' Column
df$ki <- NULL 
print(colnames(df))
head(df)
#
#(3) Reading excel files.
#i)have installed the readxl package
#ii) Load the readxl package
library(readxl)
#(iii) Read the Excel file into a dataframe
data=read_excel("/home/ibab/Downloads/pone.0148733.s001.xlsx",1)
#(iv) Print column names and dimensions of data
print(colnames(data))
print(dim(data))
#
#(4) Sets and operations on sets.
#(i) Define vectors A and B
setA<-c("a","b","c","d","e")
setB<-c("d","e","f","g")
#ii) Perform a union operation 
union(setA,setB)
#(iii) Perform an intersection operation
intersect(setA,setB)
#iv) Perform a difference operation 
setdiff(setA,setB)
setdiff(setB,setA)
#(v) Check set equality using setequal()
c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA)) #combined set
setequal(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA)),union(setA,setB))
#
#(5) Practice with subsets.
vec <- c(8, 10, 12, 7, 14, 16, 2, 4, 9, 19, 20, 3, 6)
#i) (a) Values greater than 12
gt_than_12 <- vec[vec > 12]
print(gt_than_12)

# i) (b) Values greater than 10 and less than 20
btw_10_and_20 <- vec[vec > 10 & vec < 20]
print(btw_10_and_20)

#(ii) Form an array A and remove NA values
A <- c(2, 7, 29, 32, 41, 11, 15, NA, NA, 55, 32, NA, 42, 109)
A_modified <- A[!is.na(A) & A < 100]
print(A_modified)

#(iii) Assign 0 to NA values in A
A_0 <- A
A_0[is.na(A_0)] <- 0
print(A_0)

#(iv) Create a vector with gene names “gene-1”,“gene-2” . . . “gene-6”. Create a vector
#for gender with entries M,M,F,M,F,F,M.
gene_names <- paste("gene", 1:6, sep="-")
print(gene_names)
gender <- c("M", "M", "F", "M", "F", "F", "M")
print(gender)

#(v) Enter the following data as 7 vectors:
result1 = c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 = c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 = c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 = c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 = c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 = c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 = c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)

#vi)Create a dataframe with the following columns: genes, gender, result1, result2,result3, 
#result4, result5, result6, result7. Call this data frame as datframe.
genes <- paste("gene", 1:7, sep = "-")
gender <- c("M", "M", "F", "M", "F", "F", "M")
datframe <- data.frame(genes, gender, result1, result2, result3, 
                       result4, result5, result6, result7)
#vii)Add column names to this dataframe: “GeneName”, “Gender”, “expt1”, “expt2”,
#“expt3”, “expt4”, “expt5”, “expt6”, “expt7”.

colnames(datframe) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", 
                        "expt4", "expt5", "expt6", "expt7")
#(viii) Create a subset of data with “expt2” values greater than 20
expt2 <- datframe[datframe$expt2 > 20, ]
print(expt2)

#(ix) Create a subset of data with only Female gender
female <- datframe[datframe$Gender == "F", ]
print(female)

#(x) Create a subset of data with Male gender for which “expt2” is less than 30.
male_expt2 <- datframe[datframe$Gender == "M" & datframe$expt2 < 30, ]
print(male_expt2)

#(6) If-else-if structure.
#(i) Write an if-else-if structure that explores and prints the quadrant in which an angle belongs. For example, if you input 45o
#it should print ‘First quadrant’.
quad_finder <- function(angle){
  angle <- angle %% 360
  if (angle>0 & angle <= 90){
    print("First quadrant")
  }
  else if (angle>90 & angle <=180){
    print("Second quadrant")
  }
  else if (angle>180 & angle <=270){
    print("Third quadrant")
  }
  else if (angle>270 & angle<=360){
  print("Fourth quadrant")
  }else print("angle on axis")
}
quad_finder(188)
quad_finder(95)
quad_finder(30)
quad_finder(400)

#(ii) Write an if-else-if structure that takes three numeric inputs and uses this structure alone to put the 3 numbers 
#in decreasing order. Do not use any built in function for sorting purposes.
sort_3_numbers <- function(a,b,c){
  if (a>=b & a>=c){
    if(b>=c){
      print(c(a,b,c))
    } else{
      print(c(a,c,b))
    }
  }else if (b>=a & b>=c){
    if (a>=c){
      print(c(b,a,c))
    }else {
      print(c(b,c,a))
    }
  }else{
    if (a>=b){
      print(c(c,a,b))
    } else{
      print(c(c,b,a))
    }
  }
}
sort_3_numbers(12,5,8)

#iii)Let’s say the cost of a journey ticket depends not only on the distance travelled but also on the details of the traveller.
#Distance-wise, the cost is a minimum of Rs. 100 for the first 100km, Rs. 1.50 for every extra km until 1000km and Rs.2 per
#km thereafter. On top of that, senior citizens (> 60 years ) get a 25% concession and children under 6 years of age get 50% concession.
#Write a code that takes the journey distance and the traveller’s age as inputs, and prints out the ticket cost.

calc_ticket_cost <- function(distance,age){
  #distance based cost
  if (distance <= 100){
    cost <- 100
  }else if (distance <= 1000){
    cost <- 100 + (distance -100)*1.5
  }else{
    cost <- 100+(900*1.5) + (distance-1000)*2
  }
  #discount calculations
  if (age>60){
    cost <- cost * 0.75 #25% discount for senior citizens
  } else if (age<6){
    cost <- cost * 0.50
  }
  #final cost
  print(paste("Ticket cost:Rs.",round(cost,2)))
}
calc_ticket_cost(1200,65)
calc_ticket_cost(80,30)

#(7) Writing functions.
#(i) Write a function to replace all the negative values in a vector by zeros.
replace_negatives <- function(vec) {
  vec[vec < 0] <- 0  # Replace negative values with 0
  return(vec)
}
v <- c(3, -5, 7, -2, 8, -10)
print(replace_negatives(v))




