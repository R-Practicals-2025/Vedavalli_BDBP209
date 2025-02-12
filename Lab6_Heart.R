#Lab6 7-Feb 2025
#heart.csv data
data=read.csv('/home/ibab/Downloads/Heart.csv',header=T)
#print(head(data))
#5)i)
col_name_factor<-as.factor(colnames(data))
print(str(col_name_factor))
#ii)
str(data$Sex) #CHECKING IF IT IS A FACTOR
data$Sex <- as.factor(data$Sex) #converting them as factor and printing the nlevels
print(nlevels(data$Sex))
#iii)
data$ChestPain <- as.factor(data$ChestPain)
print(levels(data$ChestPain))
#
#(6) Generating factor levels using gl() function
#i)
Temp <- gl(3,101,303,labels=c("Hot","Cold","Lukewarm"))
print(Temp)
#ii)
data_temp_df=data.frame(data,Temp)
print(data_temp_df)
#
#(7) Using the tapply() function. Try the following and see what you get
#i)
tapply(data$RestBP,data$Chol,mean)
#ii)
tapply(data$RestBP,data$Chol,mean,trim=0.1)
#
subdata<-data$RestBP
print(subdata)
sorted_subdata=sort(subdata)
print(sorted_subdata)
mean_subdata_trimmed<-mean(sorted_subdata,trim=0.1)
print(mean_subdata_trimmed)

#(8) Finding parallel-max and min. Use the pmin() and pmax() functions to find the minimum
#for each triplet set of 3 vectors
print(pmin(data$MaxHR,data$RestBP,data$Chol))
print(pmax(data$MaxHR,data$RestBP,data$Chol))

#(9) Difference between rank(), sort() and order().
#i)
ranks<-rank(data$RestBP)
sorted<-sort(data$RestBP)
ordered<-order(data$RestBP)
new_vector=data.frame(data$RestBP,ranks,sorted,ordered)
print(new_vector)
#ii)
ordered_RestBP <- data$RestBP[ordered]
new_df_heart=data.frame(data$Chol[ordered],ordered_RestBP)
#writing to csv
write.csv(new_df_heart,"/home/ibab/R/lab4_ordered_data_heart.csv",row.names = F)
print(new_df_heart)

#(10) Converting data frames to matrices. This is important to do efficient numerical calcula-
#tions with the data.
#(i) Extract the following rows and columns
extract1=data[1:6,3:8]
print(extract1)
#(ii) Convert the class of this object into a matrix using as.matrix() function and
#check the attributes to make sure it is a matrix
print(mode(extract1_matrix))
extract1_matrix=as.matrix(extract1)
print(extract1_matrix)
print(class(extract1_matrix))
print(attributes(extract1_matrix))
#(iii)Create a newcol vector/column by adding 3 vectors
data=read.csv('/home/ibab/Downloads/Heart.csv',header=T)

new_col=data$RestBP + data$RestECG + data$Chol
print(new_col)
new_col_added=data.frame(data,new_col)
print(new_col_added)
print(colnames(new_col_added))

#(v) One can add columns in another way, using cbind(). Use cbind() to add newcol
#to the original data dataframe. Call this new dataframe newcoladded2. Print
#the column names to make sure the new column has been added.

new_col_added2=cbind(data,new_col)
print(new_col_added2)

#(vi) Pick rows 26 and 35 from the original data and add these are new rows to the
#original data using the data.frame() function. Print the new dataframe and
#make sure that the rows have been added. Also print the dimensions of the newly created dataframe

extract2=data[c(26:35),]
new_row_added=rbind(data,extract2)
print(new_row_added)

#dimensions
print(dim(new_row_added))






