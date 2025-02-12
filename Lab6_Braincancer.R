#Lab 7-Feb 2025
data=read.csv('/home/ibab/Downloads/BrainCancer.csv',header=T)
#print(data)
#5)Manipulating the ‘factors’ in existing data
#i)Change the current class of the headers in the Brain Cancer data to the class
#‘factor’ and check that it is indeed the factor class.
print(head(data,20))
print(class(data$sex))
data$sex <- factor(data$sex,levels=c("Male","Female"))
print(class(data$sex))
print(is.factor(data$sex))
print(class(data$diagnosis))
data$diagnosis <- as.factor(data$diagnosis)
print(is.factor(data$diagnosis))
data$loc <- as.factor(data$loc)
print(is.factor(data$loc))
print(class(data$ki))
data$ki <- as.factor(data$ki)
print(is.factor(data$ki))
print(class(data$gtv))
data$gtv <- as.factor(data$gtv)
print(is.factor(data$gtv))
print(class(data$stereo))
data$stereo <- as.factor(data$stereo)
print(is.factor(data$stereo))
print(class(data$status))
data$time <- as.factor(data$time)
print(is.factor(data$time))
print(class(data$status))
data$status <- as.factor(data$status)
print(is.factor(data$status))

#ii)no of levels using nlevels
print(nlevels(data$sex))

#iii)levels in the category 'Diagnosis' using levels function
print(levels(data$sex))
print(levels(data$diagnosis))
#(6) Generating factor levels using gl() function

#(i) Generate a new category called ’Temperature’ containing the same number of
#elements are number of rows in the Brain Cancer data with labels ’Hot’, ’Cold’
#and ’lukewarm’.

Temp <- gl(3,29,88,labels=c("Hot","Cold","Lukewarm"))
print(Temp)
#ii)Add this category of data to the above Brain Cancer dataframe and give it a new
#dataframe name. Print this new dataframe.
data_temp_df=data.frame(data,Temp)
print(data_temp_df)
#
#(7) Using the tapply() function. Try the following and see what you get
#(i) tapply(data$gtv, data$ki, mean)

data$gtv <- as.numeric(as.character(data$gtv))
tapply(data$gtv, data$ki, mean)
#ii)tapply(data$gtv, data$ki, mean, trim=0.1) What does trim option do here?
tapply(data$gtv, data$ki, mean, trim=0.1)
#without trimming the outliers the mean is increased significantly, the effects of outliers are reduced
#
subdata1<-data$gtv
print(subdata1)
sorted_subdata1=sort(subdata1)
print(sorted_subdata1)
mean_subdata_trimmed = mean(sorted_subdata1,trim=0.1)
print(mean_subdata_trimmed)
#(8) Finding parallel-max and min. Use the pmin() and pmax() functions to find the mini-
#mum for each triplet set of 3 vectors data$gtv, data$ki and data$time
print(pmin(data$gtv,data$time,data$ki))
print(pmax(data$gtv,data$time,data$ki))

#(9) Difference between rank(), sort() and order().
ranks <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)
new_vector <- data.frame(data$gtv,ranks,sorted,ordered)
print(new_vector)
#
#
ordered_diag <- data$diagnosis[ordered]
new_df=data.frame(data$gtv[ordered],ordered_diag)
#writing to csv file
write.csv(new_df,"/home/ibab/R/lab4_ordered_data.csv",row.names = F)
print(new_df)

#(10) Converting data frames to matrices. This is important to do efficient numerical calcula-
#tions with the data.
#(i) Extract the following rows and columns from the Brain cancer data: rows 1-6,
#columns 3-8.
extract1=data[1:6,3:8]
print(extract1)
#(ii) Convert the class of this object into a matrix using as.matrix() function and
#check the attributes to make sure it is a matrix
print(mode(extract1_matrix))
extract1_matrix=as.matrix(extract1)
print(extract1_matrix)
print(class(extract1_matrix))
print(attributes(extract1_matrix))
#(iii)Create a newcol vector/column by adding 3 vectors - data$ki, data$gtv, data$time.
#data=read.csv('/home/ibab/Downloads/BrainCancer.csv',header=T)
data$ki = as.numeric(data$ki)
data$gtv= as.numeric(data$gtv)
data$time=as.numeric(data$time)
new_column <- data$ki + data$gtv + data$time
print(new_column)

#(iv) Create a new dataframe by adding this column to the last, call this new dataframe
#as newcoladded. Print the column names and make sure that newcol has been added.
new_col_added=data.frame(data,new_column)
print(new_col_added)
print(colnames(new_col_added))

#(v) One can add columns in another way, using cbind(). Use cbind() to add newcol
#to the original data dataframe. Call this new dataframe newcoladded2. Print
#the column names to make sure the new column has been added.

new_col_added2=cbind(data,new_column)
print(new_col_added2)

#(vi) Pick rows 26 and 35 from the original data and add these are new rows to the
#original data using the data.frame() function. Print the new dataframe and
#make sure that the rows have been added. Also print the dimensions of the newly created dataframe.

extract2=data[c(26:35),]
new_row_added=rbind(data,extract2)
print(new_row_added)
#row binding cannot be done by data.frame(data,column) method like for cols.
#dimensions
print(dim(new_row_added))
#
#11)Adding row and column names. Create a 4x5 matrix with the following data.
X= matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0))
print(X)
#print(rownames(X))
#print(colnames(X))
#Then add rownames to this matrix using the rownames command where the row numbers are prefixed by ’Trial-’. Sim-ilarly,
#add column names to this matrix by (a) supplying 5 different names as “aspirin”,“paracetamol”,“nurofen”,“hedex”,“placebo”,
rownames(X)<-rownames(X,do.NULL=FALSE,prefix="Trial")
drugs.names<-c("aspirin","paracetamol","nurofen","hedex","placebo")
colnames(X)<-drugs.names
print(X)
#(b) using the dimnames() function where each column number is prefixed by “drug.”.

dimnames(X)<-list(NULL,paste("drug",1:5,sep=" "))
print(dimnames(X))

#(12) Calculations on rows or columns of the matrix. Use the matrix defined in the previous
#problem to do the following. Predict your answer first before you print the answer.
#(i) mean(X[,5])
X= matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0),nrow=4,byrow=T)
print(mean(X[,5]))
#(ii) var(X[4,])
print(var(X[4,]))
#(iii) rowSums(X) calculates the sum of elements along each row. Faster method: apply(X,1,sum)
print(rowSums(X))
print(apply(X,1,sum))
#(iv) colSums(X) Faster method: apply(X,2,sum)
print(colSums(X))
print(apply(X,2,sum))
#(v) rowMeans(X) alternative: apply(X,1,mean)
print(rowMeans(X))
print(apply(X,1,mean))
#(vi) colMeans(X) alternative: apply(X,2,mean)
print(colMeans(X))
print(apply(X,2,mean))
#(vii) Sum groups of rows within a column. rowsum(X,c("A","B","B","A")). Another method is to use the tapply() or aggregate() function:
#(a) group=c("A","B","B","A")
group=c("A","B","B","A")
print(group)
print(rowsum(X,group))

#(b) what do row(X) and col(X) do? As an alternative to rowsum do the following.
print(row(X))  # Row indices
print(col(X))  # Column indices

#(c) tapply(X,list(group[row(X)], col(X))),sum)
tapply(X, list(group[row(X)], col(X)), sum)

#(d) aggregate(X,list(group),sum)
aggregate(X, list(group), sum)
