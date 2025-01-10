#Lab2 Jan-10-2025
#Ex1.1
x=round(12.1343,digits=3)
print(x)

#Ex1.2
x=round(123.12344,digits=3)
print(x)

#Ex1.3
x=round(1234.12344,digits=3)
print(x)

#Ex1.4
x=round(12345.12344,digits=3)
print(x)

#Ex 1.5
#using options function to change the no. of digits displayed
options(digits=15)
x=round(12345.12344,digits=3)
print(x)

#Ex 1.6
formatC(round(12345.12344,digits=3),format="f,digits=3")
#we can use format to modify the decimal places
print(formatC(round(12345.12344,digits=3),format="f",digits=3))

#Ex 1.7
print(1234.12344)

#Ex 1.8
print(1234.723,digits=3)
#The digits option here refers to the number of significant digits. If the number of digits is less than or equal to the number of digits before the decimal point, then print() retains the nearest rounded
#integer (here it will be rounded to 1235).
print(1234.723, digits=5)
print(1234.723, digits=6)
#If the digits value is greater than the number of digits before the decimal point, then the appropriate number of digits
#after the decimal point are retained.

#Ex 1.9
#The same rule above applies to round function.
x=round(123456788.123,digits=3)
print(x)

#Ex 1.10
print(round(123456788.123,digits=2),digits=20)
print(round(123546788.123,digits=2),digits=20)

#Ex 1.11
print(round(123456789.1234,digits=4),digits=20)

#Ex 1.12
x=paste("Hello World")
print(x)


x= paste("Hello", "World")
print(x)

#Ex 1.13
paste(1:10) #generates vector of char strings
paste(1:10)[4]
paste(1:10)[2]

#Ex 1.14
as.numeric(paste(1:10))

#Ex 1.15
paste(1:10,collapse=".") #generates single string with sep "." between each number instead of a vector.

#Ex 1.16
paste(c("Hello","World"), 1:10, sep="-")
print(paste("Hello", 1:10, sep="-"))

#Ex2-Generating sequences
#Ex 2.1
print(0:10)

#Ex 2.2
print(15:5)

#Ex 2.3
print(seq(0,1.5,0.1))

#Ex 2.4
print(seq(6,4,-0.2))

#Ex 2.5
N = c(55,76,92,103,84,88,121,91,65,77,99)
print(N)

#Ex 2.6
seq(from=0.04,by=0.01,length=11)
seq(0.04,by=0.01,along=N) #as N was assigned to 11 integers, along takes the length of N and prints

#Ex 2.7
seq(from=0.04,to=0.14,along=N) 
#yes it does match with the above values, since length will be 11- the elements starts from 0.04 and ends at 0.11

#Ex 2.8
sequence(c(4,3,4,4,4,5))
sequence(4,3,2) #(no.of elements,starts from,skips number)

#Ex 2.9
rep(9,5)
rep(1:4,2)
rep(1:4,each=2)
rep(1:4,each=2,times=3)
rep(1:4,1:4)

#Ex 2.10
rep(1:4,c(4,1,4,2))
rep(c("cat","dog","goldfish","rat"),c(2,3,2,1)) #similar to zip the 1st element 
#of 1 list is repeated 1st element of list 2 times

#Ex 2.11
seq(-1,1,by=0.1)
seq(-1,1,0.1)

#Ex 2.12
seq(-1,1,length=7)

#Ex 2.13
nums<- -1 + (0:20) * 0.1
print(nums)

#Ex3 - Missing values, infinity and NaN, NA
#Ex3.1
print(3/0)

#Ex3.2
print(exp(-Inf))

#Ex3.3
print((0:3)**Inf)

#Ex3.4
print(0/0)

#Ex3.5
print(Inf-Inf)

#Ex3.6
print(Inf/Inf)

#Ex3.7
print(is.finite(10))

#Ex3.8
print(is.infinite((10))
      
#Ex3.9
print(is.infinite((Inf)))

#Ex3.10
y<- c(4,NA,7)
y=='NA'
print(is.na(y))


#Ex3.11
x<- y[!is.na(y)]
print(x)

#Ex3.12
c1<- c(1,2,3,NA)
c2<- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)
print(full.frame)
reduced.frame <- full.frame[! is.na(full.frame$c1),]
print(reduced.frame)
#mean(x) function
x<-mean(c1,na.rm=TRUE)
print(x)

#Ex3.13
v <- c(1:6,NA,NA,9:12)
print(seq(along=v)[is.na(v)]) #prints the indices where the NA values are present
print(which(is.na(v))) #which returns the indices where the condition is.na (NA) is true



