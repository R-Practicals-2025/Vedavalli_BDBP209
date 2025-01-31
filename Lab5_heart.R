#2)Print the basic features of the data
#i)dimensions
df=read.csv('/home/ibab/Downloads/Heart.csv',header=T)
print(df)
print(dim(df)) #just like matrix dimension
print(length(df)) #only column length
#ii) column names
print(colnames(df))
#iii) rownames
print(rownames(df))
#iv) head 30 rows
print(head(df)) #prints first six entries
print(head(df,n=30L)) #to print 30 entries
print(head(df,30))
#v)Data representation as frq table
print(table(df$Sex))
table(df$ChestPain)
table(df$Fbs)
table(df$RestECG)
table(df$ExAng)
table(df$Slope)
table(df$Ca)
table(df$Thal)
table(df$AHD)

#vi) and vii) how many categorical variables are present?what are the categorical variables present?
#creating a factor

df$Sex<-factor(df$Sex,levels=c("1","0"))
df$ChestPain<-factor(df$ChestPain,levels=c("typical","asymptomatic","nonanginal","nontypical"))
df$Thal<-factor(df$Thal,levels=c("fixed","normal","reversable"))
df$AHD<-factor(df$AHD,levels=c("Yes","No"))
#11 categorical variables are present
#(viii) how many levels does each categorical variable have and what are the levels?
print(levels(df$Sex))
print(levels(df$ChestPain))
print(levels(df$Thal))
print(levels(df$AHD))
#(3) Print the following statistical parameters of the data of
#i)mean of column describing the RestBP
print(mean(df$RestBP))
#(ii) mean of column describing the Cholesterol
print(mean(df$Chol))
#(iii) median of RestBP column (how does it compare with the mean?)
print(median(df$RestBP)) #median is lesser but almost close to the mean, the data may be skewed to the right
#(iv) mode of RestBP column (how does it compare with median and mean? Is the
#distribution symmetric?

frq<-print(table(df$RestBP))
print(names(which.max(frq)))
#No, The distribution isn't symmetric as the values of mean, median and mode follows the trend-> mean>median>mode
#hence, the distribution is skewed to the right.
#(v) standard deviation of RestBP column
sd(df$RestBP)
#(vi) statistical summary of the RestBP column data
print(summary(df$RestBP))
#(vii) histogram plot of RestBP data (does the appearance agree with the mean,median,mode
#order?
hist(df$RestBP) #yes, the appearance appearance agree with the mean,median,mode order.
#(viii)skewness value of GTV data
library(moments)
print(skewness(df$RestBP))
#(ix) kurtosis value of the GTV data
print(kurtosis(df$RestBP))

#(x) Make a boxplot of the GTV data with default options and compare this with 
#another boxplot of the same data with the following options: range=0.1, 
#hori-zontal=FALSE, border=c(”blue”),col=c(”red”). Repeat the plot with range=0.2and range=0.05. Comment on the differences.
boxplot(df$RestBP,main="Boxplot of RestBP",xlab="RestBP",ylab="Frequency",horizontal=F,border=c("navy"),col=c("lightblue"))
boxplot(df$RestBP,main="Boxplot of RestBP",xlab="RestBP",ylab="Frequency",horizontal=F,range=0.1,border="navy",col="lightblue")
boxplot(df$RestBP,main="Boxplot of RestBP",xlab="RestBP",ylab="Frequency",horizontal=F,range=0.2,border="navy",col="lightblue")
boxplot(df$RestBP,main="Boxplot of RestBP",xlab="RestBP",ylab="Frequency",horizontal=F,range=0.5,border="navy",col="lightblue")
#AS the ranges increase, more values get included in the whiskers, which means lesser outliers.

#(xi) Make a boxplot of 3 data sets – GTV, KI, and time. Which of the data has the broadest distribution?
boxplot(df$RestBP,df$Chol,df$MaxHR,
        names=c("Rest BP","Cholesterol","Max HeartRate"),
        main="Boxplot of RestBP,Cholesterol,Max HeartRate",
        col=c("lightblue","pink","brown"),
        border="black",
        xlab="Variables",
        ylab="Values")
#The variable with the widest box and longest whiskers is Cholesterol

#4)Build subsets of the data below.
#(i) GTV data with values > 20. Also print the dimensions of the subset.
filter1=subset(df,df$Age>50)
print(filter1)
print(dim(filter1))

#(ii) Row numbers 1,3,8,9,13,14, 18 and 21
filter3=df[c(1,3,8,9,13,14,18,21),]
print(filter3)

#(iii) Obtain the indices of the rows that have only Female sex entries, and using this list of indices build a subset of the data
filter4_ind=which(df$Sex=='0')
print(filter4_ind)
filter4=df[filter4_ind,]
print(filter4)

#(iv) Extract the columns GTV and KI and build a new column that uses this formula:
#data$gtv*data$ki/234. Build a new dataframe containing the three columns
#GTV, KI and the newly added column.

new_data <- df[, c("RestBP", "Chol")]
new_data$New_Column <- (new_data$RestBP * new_data$Chol) / 234
print(new_data)

#(v) Write the subset data corresponding to ”Female” entries (above) as a dataframe,
#and write the dataframe to a csv file – call this file as lab4 female BrainCancer.csv
female_entries<-which(df$Sex=='0')
write.csv(female_entries,"/home/ibab/Downloads/lab4_female_Heart.csv",row.names = F)












