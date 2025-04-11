#III. Central Limit Theorem
#(1) CLT case with sampling from non-normal distribution: Here we will demonstrate the central limit theorem by 
#taking samples from a non-normal distribution such as the uniform distribution. Follow the steps below.

#(i) Generate a sample set of 5 random deviates from a uniform distribution in the
#interval [0,10]. Repeat this 10,000 times. We now have 10,000 samples of 5
#numbers each.
samples=replicate(10000,runif(5,min=0,max=10))
head(samples)
###
#(ii) Calculate the mean for each of the 10,000 samples and store it in a separate array.
#Plot this distribution of means as a histogram. Print the mean and standard
#deviation of the distribution of means.
mean_samples=colMeans(samples)
#print(mean_samples)
hist(mean_samples,breaks="Sturges",col="pink",main="Histogram of distribution of means",xlab="Frequency",
     ylab="Density")
#mean of the mean distribution
mean_means=mean(mean_samples)
std_means=sd(mean_samples)
print(paste("Mean of sample means:",mean_means))
print(paste("Standard deviation of sample means:",std_means))
###
#(iii) Generate a sequence of numbers between 0 and 10 with 0.1 spacing. Obtain the
#normal probability densities using dnorm function with the calculated mean and
#standard deviation in the last question and store it as a separate vector.
mean_means=mean(mean_samples)
std_means=sd(mean_samples)
x=seq(0,10,by=0.1)
mean_val=mean_means
std_val=std_means

normal_pdf=dnorm(x,mean=mean_val,sd=std_val)
# print(normal_pdf)
#plot(x,prob_densities,xlab="x values",ylab="Density",main="Normal Density Curve",type='l')
###
#(iv) Since we have 10,000 samples, we have to scale the normal probability density
#function to our particular case (since the area is 1.0 otherwise). The height and
#bin width of a histogram are related to each other – if we doubled the bin width,
#there would be roughly twice as many numbers in the bin and the bar would be
#twice as high on the y-axis. So to get the height of the bars on our frequency scale,
#we multiply the total frequency, i.e., 10,000 by the bin width 0.5 to get 5000. This
#is the scaling factor for the PDF. Perform this and save the scaled probabilities
#as a separate array.

#we need to scale the normal pdf because it integrates to 1.
#total samples is 10000 so scaling factor=total samples x Bin width=10000x0.5=5000
scaled_pdf=normal_pdf*5000
###

#(v) Plot the normal curve on top of the histogram to see the level of agreement
#between the normal behaviour of the sample means and the normal curve.
scaled_pdf=normal_pdf*5000
brk=seq(0,10,by=0.5)
hist(mean_samples,breaks=brk,freq=T,col="rosybrown3",border="black",main="CLT: Normal curve over histogram of sample means ",
     xlab="Sample means",ylab="Frequency")
lines(x,scaled_pdf,col="navyblue",lwd=2)
###

#(2) CLT demo with sampling from non-normal, non-functional distribution: Here
#we demonstrate CLT by taking samples not from an intrinsic definition of the uniform
#distribution. Here we will create samples of dice throwing exercise.

#(i) Create 10,000 samples of single dice throw using the sample() function:
#a<- sample(1:6,replace=T,10000)
#Make a plot of this distribution. You should see a uniform distribution.
set.seed(42)
a=sample(1:6,replace=TRUE,size=10000) #beware of the order (error)
#plotting histogram - shd appear uniform as the outcomes 1-6 are equally likely
x=table(a)
#max_frq=max(x)- also can be used for ylim
barplot(b,col="chocolate",main="Die roll- outcomes",xlab="Outcome",ylab="Frequency",ylim=c(0,2000))
###
#(ii) Throw two dice and add the scores together (this is the ancient game of craps).
#Generate a new object b similar to the above. Plot a histogram of a+b. You should see a triangular shape developing for the histogram.
b=sample(1:6,replace=T,10000)
sum_of_two_dice=a+b

#plotting histogram
hist(sum_of_two_dice,breaks=seq(1.5,13.5,by=1),col="palegreen3",main="Sum of two dice",xlab="Sum",ylab="Frequency",ylim=c(0,2000))
#we can't use sturges as it's a discrete distribution
###
#(iii) Repeat the exercise with three dice. The histogram should start showing the distinct bell shape.
c=sample(1:6,replace=T,10000)
sum_of_three_dice=a+b+c

hist(sum_of_three_dice,breaks = seq(2.5, 18.5, by = 1),col="coral3",main="Sum of three dice",xlab="Sum",ylab="Frequency",ylim=c(0,1500))
###
#(iv) Repeat this with five dice. The histogram is now very close to a normal curve.
#Use the mean and standard deviation of the 5 dice to generate a normal PDF. As
#in the last problem, one has to scale the PDF to match the height of the normal
#curve with the height of the histogram.
d=sample(1:6,replace=T,10000)
e=sample(1:6,replace=T,10000)
sum_of_five_dice=a+b+c+d+e

hist(sum_of_five_dice,breaks = seq(4.5, 30.5, by = 1),col="deepskyblue3",main="Sum of five dice",xlab="Sum",ylab="Frequency",ylim=c(0,1500))

mu=mean(sum_of_five_dice)
sigma=sd(sum_of_five_dice)
x_vals=seq(5, 30, by = 0.1)
pdf_vals=dnorm(x_vals,mean=mu,sd=sigma)
scaled_pdf=pdf_vals*10000*1 #as the bin-width is 1, we shd multiply by 1 to scale 

lines(x_vals, scaled_pdf, col = "black", lwd = 2)
#we can see that it's approaching normal
###
#
library(pROC)
#IV. ROC curve
#The goal is to model wine quality based on physicochemical tests.There are 11 input variables (see column names)
#and the output variable is the quality score in scale of 0 to 10 from wine tasting. We want to test various 
#thresholds of the quality score to achieve the best possible binary classifier of good and bad quality wine.

#(1) Read in the white wine data into a dataframe, and create additional columns that classifies the data
#as good or bad wine based on threshold quality scores of 6, 7, 8, 9 and 10.

df=read.csv("/home/ibab/Downloads/winequality-white.csv", sep = ";")
print(head(df))
print(colnames(df))
#adding new columns
good6=ifelse(df$quality >= 6,1,0)
good7=ifelse(df$quality >= 7,1,0)
good8=ifelse(df$quality >= 8,1,0)
good9=ifelse(df$quality >= 9,1,0)
good10=ifelse(df$quality >= 10,1,0)
print(head(df))
table(good10) #gives how many 0s and 1s are present (table data)
#assigning them to the df
df$good6 <- good6
df$good7 <- good7
df$good8 <- good8
df$good9 <- good9
df$good10 <- good10

# SYNTAX-ifelse(condition, value_if_TRUE, value_if_FALSE)

#or we can do this as well
# good6 <- c()
# 
# for (i in 1:nrow(wine_data)) {
#   quality_value <- wine_data$quality[i]
#   
#   if (quality_value >= 6) {
#     good6[i] <- 1
#   } else {
#     good6[i] <- 0
#   }
# }
# df$good6=good6


#(2) Use plot.roc() function as follows to plot the ROC curves for each threshold value.
#Which threshold value brings the ROC curve to the perfect classifier?
# plot.roc(data$winequality,data$alcohol, #data vectors
#          main="title", #main plot title
#          legacy.axes=TRUE, # plots sensitivity against (1-specificity)
#          ci=TRUE, # plot confidence interval of AUC
#          print.auc=TRUE, # print values of AUC
#          identity.lwd=2, # linewidth of the 45 deg line (worst classifier)
#          print.thres=TRUE, # print best threshold on the graph
#          
# )
par(mfrow=c(2,2))
plot.roc(df$good6, df$alcohol, main = "ROC Curve-Threshold >= 6", 
         legacy.axes = TRUE, ci = TRUE, print.auc = TRUE, 
         identity.lwd = 2, print.thres = TRUE)

plot.roc(df$good7, df$alcohol, main = "ROC Curve-Threshold >= 7", 
         legacy.axes = TRUE, ci = TRUE, print.auc = TRUE, 
         identity.lwd = 2, print.thres = TRUE)

plot.roc(df$good8, df$alcohol, main = "ROC Curve-Threshold >= 8", 
         legacy.axes = TRUE, ci = TRUE, print.auc = TRUE, 
         identity.lwd = 2, print.thres = TRUE)

plot.roc(df$good9, df$alcohol, main = "ROC Curve-Threshold >= 9", 
         legacy.axes = TRUE, ci = TRUE, print.auc = TRUE, 
         identity.lwd = 2, print.thres = TRUE)

plot.roc(df$good10, df$alcohol, main="ROC Curve-Threshold >= 10",
         legacy.axes = TRUE, ci = TRUE, print.auc =TRUE,
         identity.lwd = 2, print.thres =TRUE)

