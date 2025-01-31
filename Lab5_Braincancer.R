#Exercise1
#1)Read in the file ”BrainCancer.csv” and store the data as data frame
data=read.csv('/home/ibab/Downloads/BrainCancer.csv',header=T)
#2)Print the basic features of the data
#i)dimensions
print(data)
print(dim(data)) #just like matrix dimension
print(length(data)) #only column length
#ii) column names
print(colnames(data))
#iii) rownames
print(rownames(data))
#iv) head 30 rows
print(head(data)) #prints first six entries
print(head(data,n=30L)) #to print 30 entries
print(head(data,30))
#v)Data representation as frq table
print(table(data$diagnosis)) #frequency
print(table(data$gtv)) 
#vi) and vii) how many categorical variables are present?what are the categorical variables present?
#creating a factor
print(data)
data$sex<-factor(data$sex,levels=c("Male","Female"))
data$diagnosis<-factor(data$diagnosis,levels=c("Meningioma","HG glioma","LG glioma","Other"))
data$loc<-factor(data$loc,levels=c("Infratentorial","Supratentorial"))
data$stereo<-factor(data$stereo,levels=c("SRS","SRT"))
# 10 categorical variables are present
#(viii) how many levels does each categorical variable have and what are the levels?
print(levels(data$sex))
print(levels(data$diagnosis))
print(levels(data$loc))
print(levels(data$stereo))


#(3) Print the following statistical parameters of the data of
#i)mean of column describing the gross tumor volume (GTV)
print(mean(data$gtv))
#(ii) mean of column describing the time
print(mean(data$time))
#(iii) median of GTV column (how does it compare with the mean?)
print(median(data$gtv)) #median is lesser than mean, which could mean that the data is skewed right
#(iv) mode of GTV column (how does it compare with median and mean? Is the
#distribution symmetric?

frq<-print(table(data$gtv))
print(names(which.max(frq)))
frq2<-print(table(data$ki))
print(names(which.max(frq)))
#mean value-27.45,median-6.5,mode-2.5 this tells us that the distribution is skewed right
#(v) standard deviation of GTV column
sd(data$gtv)
#(vi) statistical summary of the GTV column data
print(summary(data$gtv))
#(vii) histogram plot of GTV data (does the appearance agree with the mean,median,mode
#order?
hist(data$gtv) #yes, the appearance appearance agree with the mean,median,mode order.
#(viii)skewness value of GTV data
library(moments)
print(skewness(data$gtv))
#(ix) kurtosis value of the GTV data
print(kurtosis(data$gtv))

#(x) Make a boxplot of the GTV data with default options and compare this with 
#another boxplot of the same data with the following options: range=0.1, 
#hori-zontal=FALSE, border=c(”blue”),col=c(”red”). Repeat the plot with range=0.2and range=0.05. Comment on the differences.
boxplot(data$gtv,main="Boxplot of GTV",xlab="Spread of GTV",ylab="GTV",horizontal=F,border=c("navy"),col=c("lightblue"))
boxplot(data$gtv,main="Boxplot of GTV",xlab="Spread of GTV",ylab="GTV",horizontal=F,range=0.1,border="navy",col="lightblue")
boxplot(data$gtv,main="Boxplot of GTV",xlab="Spread of GTV",ylab="GTV",horizontal=F,range=0.2,border="navy",col="lightblue")
boxplot(data$gtv,main="Boxplot of GTV",xlab="Spread of GTV",ylab="GTV",horizontal=F,range=0.5,border="navy",col="lightblue")
#AS the ranges increase, more values get included in the whiskers, which means lesser outliers.

#(xi) Make a boxplot of 3 data sets – GTV, KI, and time. Which of the data has the broadest distribution?
boxplot(data$gtv,data$ki,data$time,
        names=c("GTV","KI","Time"),
        main="Boxplot of GTV,KI and Time",
        col=c("lightblue","pink","brown"),
        border="black",
        xlab="Variables",
        ylab="Values")
#The variable with the widest box and longest whiskers is Time

#4)Build subsets of the data below.
#(i) GTV data with values > 20. Also print the dimensions of the subset.
filter1=subset(data,data$gtv>20)
print(filter1)
print(dim(filter1))

#(ii) Row numbers 1,3,8,9,13,14, 18 and 21
filter3=data[c(1,3,8,9,13,14,18,21),]
print(filter3)

#(iii) Obtain the indices of the rows that have only Female sex entries, and using this list of indices build a subset of the data
filter4_ind=which(data$sex=='Female')
print(filter4_ind)
filter4=data[filter4_ind,]
print(filter4)

#(iv) Extract the columns GTV and KI and build a new column that uses this formula:
#data$gtv*data$ki/234. Build a new dataframe containing the three columns
#GTV, KI and the newly added column.

new_data <- data[, c("GTV", "KI")]
new_data$New_Column <- (new_data$GTV * new_data$KI) / 234
print(new_data)

#(v) Write the subset data corresponding to ”Female” entries (above) as a dataframe,
#and write the dataframe to a csv file – call this file as lab4 female BrainCancer.csv
female_entries<-which(data$sex=='Female')
write.csv(female_entries,"/home/ibab/Downloads/lab4_female_BrainCancer.csv",row.names = F)









