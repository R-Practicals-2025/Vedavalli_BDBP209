#LAB 14 
#Apr 24 2025
#I. Error bars, covariance and correlation:

#error bar shows uncertainity in a value - it can represent 1)Std dev 2)Std error 3)Confidence Interval
#We can use arrows() function to draw error bars - syntax is arrows(x0,y0,x1,y1,length,angle,code,col,lty,lwd

#(1) Error bars on bar plots: Enter the following data into R:
#means = c(20.34,19.49,25.68)
#stderr = c(0.83,1.51,1.39)
#Make a barplot with the following features: the three means should be labeled as ‘A’,
#‘B’ and ‘C’, grey filled bars, plot title as ’Errors on bar plot’. Use the arrows() function
#to plot the error bars as follows:
  #arrows(<barplotobject>, means+stderr, <barplotobject>,means-stderr,
         #angle=90,code=3,length=0.06,col=’red’)
means=c(20.34,19.49,25.68)
stderr=c(0.83,1.51,1.39)
#calculate midpoints of a bar
midpoints=barplot(means,names.arg=c("A","B","C"),col="grey",ylim=c(0,max(means+stderr)+2),main="Errors on barplot")
#add error bars using arrows()
arrows(midpoints,means+stderr,midpoints,means-stderr,angle=90,code=3,length=0.06,col='red')
###
#(2)Error bars on (x,y) plots: Enter the following data into R. The errors provided are standard errors on the mean
#x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
#y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
#errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)
#Plot (x,y) with points and xlabel ‘concentration’ and ylabel ‘optical activity’ and plot title as ‘Error bars on data points”. Again we can use the arrows() function to plot the error
#bars, only difference is in the first 4 arguments: arrows(x,y+errors,x,y-errors,.....)


x = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
y = c(5, 9, 13, 17, 20, 24, 26, 27, 28, 27)
errors = c(0.5, 0.9, 1.4, 1.5, 2.0, 2.2, 2.3, 2.5, 2.9, 3.0)
#scatter plot with error bars
plot(x,y,pch=19,xlab="Concentration",ylab="optical activity",main="Error bars on data points",
     ylim=c(min(y-errors),max(y+errors)))
#adding the vertical error bars
arrows(x,y+errors,x,y-errors,angle=90,code=3,length=0.05,col="coral")
###

#(3) Covariance and Pearson’s correlation coefficient: For a univariate sample, the functions
#cov() and cor() return a number, for multivariate samples, these functions returns a matrix. Try the following:

x =c(10,20,30,40,50,60,70,80,90,100)
y =c(95, 220, 279, 424, 499, 540, 720, 880, 950, 1200)
cov(x, y)   # Covariance: measure of joint variability
cor(x, y)   # Pearson Correlation: strength + direction of linear relationship
#For multivariate datasets
cor(longley) # built-in multivariate dataset

#II. One sample tests

#(1) One sample Z test:
#(a) Write a function to perform one sample Z test, where given a data set x that is
#randomly drawn from a Gaussian distribution of population mean μ and standard
#deviation σ the function returns the conclusions of the test along with the computed statistic values. 
#The function should be defined as one_sample_Ztest(x,sigma,muzero, alpha,null) where x is the data vector,
#sigma is the population standard deviation, muzero is the population mean for
#comparison, alpha is the significance level and null is a string indicating type of
#null hypothesis. The possible values of null should be equal, less_than_or_equal
#or more_than_or_equal. The function should return a vector with two numbers
#and the conclusion: p-value and the Z-value and the statistical conclusion.

#function for 1 sample test
one_sample_Ztest <- function(x,sigma,muzero,alpha,null){
  #defining sample mean as xbar
  xbar=mean(x)
  n=length(x) #size
  #Z-statistic
  Z=(xbar-muzero)/(sigma/sqrt(n))
  
  #possible values of null
  #p-value computation using null hypothesis
  #1.equal-two tailed test 
  #wkt pnorm(Z) gives the area to the left of Z , 1-pnorm(Z) gives the area to the right of Z,so
  #for two tails we multiply by 2
  if (null=="equal"){
    p_val=2*(1-pnorm(abs(Z)))
  } 
  #2.less_than_or_equal - one tailed test (right tail) - calculates the area to the right of Z
  else if (null=="less_than_or_equal"){
    p_val=1-pnorm(Z)
  }
  #3.greater_than_or_equal - one tailed test (left tail) - calculates the area to the left of Z
  else if (null=="greater_than_or_equal"){
    p_val=pnorm(Z)
  }
  else{
    stop("Invalid null hypothesis type.")
  }
  #statistical conclusion
  conclusion = ifelse(p_val<alpha,"Reject null hypothesis","Fail to reject null hypothesis")
  return(list(Z_value=Z,p_value=p_val,conclusion=conclusion))
}

#(b) Use the data set to test the null hypothesis that μ = μ0:
#x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
#      137.4, 145.6, 135.6, 135.4, 121.5)
#and μ0 of 124.6 and σ = 14.5 with 0.05 significance level.

#we see from thw qn mu=muzero so the two tailed test would be done

x = c(141.5, 152.3, 121.2, 123.0, 151.6, 124.8, 138.9,
            137.4, 145.6, 135.6, 135.4, 121.5)
result=one_sample_Ztest(x,sigma=14.5,muzero=124.6,alpha=0.05,null="equal") #this pval is for alpha 0.05- 0.00781887
#since it is two tailed test, the p value (alpha/2) of left and right tail would be 0.00781/2=0.003909435
print(result)
##
#(2) One sample t- test:
#(a) Write a function to perform a one sample t-test given a data set x that is randomly
#drawn from a Gaussian distribution of population mean μ and standard deviation
#σ. The function should return the conclusions of the test along with the statistic
#values. Function should be defined as one_sample_t_test(x,muzero,alpha,null) where the arguments have the same
#meaning as above.
one_sample_ttest=function(x,muzero,alpha,null){
  #defining sample statistics
  xbar=mean(x)
  s=sd(x)
  n=length(x)
  #Computing t statistic
  t = (xbar-muzero)/(s/sqrt(n))
  df= n-1 #degrees of freedom
  
  #Computing p-val based on the type of null hypothesis
  #1.two tailed test
  #pt() function t-distribution's CDF
  #pt(abs(t),df)- gives the area to the left of t, then 1-pt(abs(t),df) gives the area right to t,
  #we multiply by 2 for obtaining both the tails
  if(null=="equal"){
    p_val = 2*(1-pt(abs(t),df))
  }else if (null=="less_than_or_equal")#one tailed,right tailed t test
    {
    p_val = 1-pt(t,df)
  }else if (null=="more_than_or_equal")#one tailed,left tailed t test
    {
    p_val = pt(t,df)
  }else {
    stop("Invalid null hypothesis type.")
  }
  #statistical conclusion
  conclusion=ifelse(p_val<alpha,"Reject null hypothesis","Fail to reject null hypothesis")
  return(list(tval=t,pval=p_val,conclusion=conclusion))
}

#(b) Use the data set below to test the null hypothesis that μ = μ0, where μ0 = 100 for 0.05 significance level.
#x = c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
 #     96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)

x = c(96.0, 104.0, 99.1, 97.6, 99.4, 92.8, 105.6, 97.2,
      96.8, 92.1, 100.6, 101.5, 100.7, 97.3, 99.6, 105.9)
result=one_sample_ttest(x,muzero=100,alpha=0.05,null="equal")
print(result)
##

#25 Apr 2025
#(3) One sample proportion test:
#In R, the functions binom.test() and prop.test() performs the one sample proportion test. The former computes the exact binomial probability,
#and is used when the sample sizes are small. The latter uses a normal approximation
#to the binomial distribution and can be used when n > 30. The functions are
# binom.test(x,n,p,alternative)
# prop.test(x,n,p,alternative,correct)
# where x is the number of successes, n is the total number of trials, p proportion to test
# against (i.e., hypothesized value), alternative is the string value indicating type of null
# hypothesis as “two-sided”, “less”, “greater”, and correct is a logical variable indicating
# whether a correction should be applied for small sample sizes (the default is TRUE).
# Print the results of both the tests with x = 710, n = 2600, p = 0.25 and alternative=greater.

#testing with binom.test()
binom.test(x=710,n=2600,p=0.25,alternative="greater") #testing whether the true prop is >0.25

#testing with prop.test()
prop.test(x=710,n=2600,p=0.25,alternative = "greater",correct=T) #the correction is not required because when we are 
#converting the discrete binomial distb to continuous normal distbn, the sample size is already large enough to approximate the normal distbn
prop.test(x=710,n=2600,p=0.25,alternative="greater",correct=F)
##

#(4) One sample variance test:
#(a) Write a function with the following structure to output the statistical conclusion
#and properties (p-value and σ2 value):
#one_sample_variance_test(x,test_sigma,alpha), where the function should compute the χ2
#test statistic and get the appropriate limits by using the qchisq() function and deciding the conclusion.

one_sample_variance_test=function(x,test_sigma,alpha){
  #sample stats
  n=length(x)
  s2=var(x) #sample variance
  sigma2=test_sigma^2 #hyposthesized population variance
  
  #chi-sq test statistic
  chi2=(n-1)*s2/sigma2
  
  #critical values for 2 tailed test
  chi2_lower=qchisq(alpha/2,df=n-1)
  chi2_upper=qchisq(1-alpha/2,df=n-1)
  # Two-tailed p-value
  p_val = 2*min(pchisq(chi2,df=n-1), 1-pchisq(chi2,df=n-1))
  
  #decision rule
  if (chi2<chi2_lower || chi2>chi2_upper){
    conclusion="Reject null hypothesis"
  }else{
    conclusion="Fail to reject null hypothesis"
  }
  return(list(chi_squared_value=chi2,pop_var=s2,p_value=p_val,conclusion=conclusion))
}

#(b) Perform the above test for the data set given below for hypothesized σ = 29 and for
# 0.05 significance level. The points are from a normal distribution with mean=140
# and standard deviation of 20.

x = c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
      156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)
result=one_sample_variance_test(x,test_sigma = 29,alpha=0.05)
print(result) #result wd be rejecting null hypothesis
##
#using the one-tailed test variation for this qn
one_sample_variance_test_one_tailed = function(x, test_sigma, alpha, alternative="two_sided") {
  # sample stats
  n = length(x)
  s2 = var(x)
  sigma2 = test_sigma^2
  chi2 = (n - 1) * s2 / sigma2
  
  # initialize critical and conclusion
  chi2_critical = NA
  conclusion = NA
  p_val = NA
  if (alternative == "greater") {
    chi2_critical = qchisq(1 - alpha, df = n-1)
    p_val = 1 - pchisq(chi2,df=n - 1)
    conclusion = if (chi2 > chi2_critical) "Reject H0" else "Fail to reject H0"
  } else if (alternative == "less") {
    chi2_critical = qchisq(alpha,df= n-1)
    p_val = pchisq(chi2,df=n - 1)
    conclusion = if (chi2 < chi2_critical) "Reject H0" else "Fail to reject H0"
  }
  
  return(list(
    chi_squared_value = chi2,
    chi_squared_critical = chi2_critical,
    pop_variance = s2,
    p_value = p_val,
    conclusion = conclusion
  ))
}
x=c(142.8, 135.0, 157.5, 148.4, 135.9, 153.4, 149.0, 130.2,
    156.0, 189.7, 151.6, 156.5, 123.8, 152.9, 118.4, 145.8)
# Run the tests
result_more = one_sample_variance_test_one_tailed(x, test_sigma = 29, alpha = 0.05, alternative = "greater")
result_less = one_sample_variance_test_one_tailed(x, test_sigma = 29, alpha = 0.05, alternative = "less")
# Print the results properly
cat("Result for one-sided test (H1:variance>841)right tailed:\n")
print(result_more) #conclusion is fail to reject H0,since 4.91 < 24.99
cat("\nResult for one-sided test (H1:variance<841) left tailed:\n")
print(result_less) #conclusion is reject H0,since 4.91 < 7.26

##
#(5) One sample Wilcoxon signed rank test: In R, the function wilcox.test() carries out
#one- and two-sample non-parametric tests. The arguments are
#wilcox.test(x,y=NULL,alternative,mu=0,paired=FALSE,
            #exact=NULL,correct=TRUE,conf.int=FALSE,conf.level=0.95)
# where for two samples, y will not be NULL. Perform this test for the data set with μ = 160
# and confidence level of 95% and H0 as μ >= μ0.

#we need to test if the median of a sample is equal to a specific value,also wkt it 
#doesn't assume that the data is normally distributed
x = c(176.9, 158.3, 152.1, 158.8, 172.4, 169.8, 159.7, 162.7,
      156.6, 174.5, 184.4, 165.2, 147.8, 177.8, 160.1, 161.5)
#mu-hypothesized median value
#our H0 is median>= 160, H1 is median<160 -left tailed test
result=wilcox.test(x,mu=160,alternative="greater",conf.level=0.95)
print(result)
##
##III. Two sample tests
#(1) Two sample Z test:
#(a) Write a function to perform a two sample Z test given data sets x1, x2, their respective 
#standard deviations, signficance level and null hypothesis. As above, the function should 
#return the conclusions of the test along with the statistic values. The function should be
#defined as two_sample_Z_test(x1,x2,sigma_x1,sigma_x2,alpha,null)

#for two-sample Z test
two_sample_Z_test = function(x1, x2, sigma_x1, sigma_x2, alpha, null) {
  # Remove missing values
  x1=na.omit(x1)
  x2=na.omit(x2)
  # Calculate sample means and sizes
  mean_x1=mean(x1)
  mean_x2=mean(x2)
  n1=length(x1)
  n2=length(x2)
  #test statistic
  se=sqrt((sigma_x1^2/n1) + (sigma_x2^2/n2))
  z_stat=(mean_x1-mean_x2)/se
  # Determine critical value based on the null hypothesis
  if (null == "mu1 >= mu2") {
    critical_value <- qnorm(1 - alpha)  # Right-tailed test (critical value for upper tail)
    reject=z_stat < critical_value  # Decision rule
  } else {
    stop("Invalid null hypothesis format. Expected 'mu1 >= mu2'")
  }
  # Conclusion based on z-statistic
  conclusion <- ifelse(reject,"Reject the H0","Fail to reject the H0")
  # Return results
  return(list(Z_statistic=z_stat, Critical_Value=critical_value,Conclusion=conclusion,Mean_x1 = mean_x1,
    Mean_x2 = mean_x2,Sample_Size_x1=n1,Sample_Size_x2=n2))
}

#(b) Use the data given in two-sample.dat for this problem with σx1 = 24.6 and
#σx2 = 27.8 with α = 0.05 to test the null hypothesis that the μ1 ≥ μ2.

x1= c( 258.0, 271.5, 189.1, 216.5, 237.2, 222.0, 231.3, 181.7, 220.0, 179.3, 238.1, 217.7,
        246.2, 241.5, 233.8, 222.3, 199.2, 167.9, 216.2, 240.4, 235.3, 187.0, 233.7, 214.7,
        174.6, 246.3, 185.7, 207.0, 244.3, 237.7, 245.2, 228.3, 201.8, 218.3, 242.7, 213.8,
        231.9, 257.3, 208.4, 250.7, 198.3, 206.7, 259.7, 253.3, 200.3, 196.6, 210.6, 257.6,
        173.5, 267.5, 167.2, 227.1, 172.1, 197.6, 256.9, 203.7, 195.1, 237.4, 210.2, 208.8,
        218.0, 205.1, 241.1, 216.8, 223.6, 191.0, 225.9, 215.1, 233.1, 243.0)

x2= c( 221.0, 213.0, 199.3, 211.2, 225.2, 229.1, 253.9, 194.6, 243.0, 221.9, 230.9, 221.1,
        206.7, 217.2, 215.8, 203.0, 234.0, 196.3, 235.8, 234.3, 244.7, 248.8, 200.5, 232.0,
        233.3, 220.6, 289.2, 244.9, 230.8, 182.9, 199.3, 263.2, 220.6, 266.7, 258.0, 243.9,
        178.1, 200.7, 270.2, 224.4, 222.4, 234.6, 296.7, 202.3, 277.9, 204.3, 221.1, 257.0,
        243.4, 239.4, 230.0, 263.5, 241.3, 216.6, 227.9, 230.1, 230.5, 188.6, 289.3, 234.4,
        267.5, 256.0, 246.5, 210.5, 270.6, 295.5, 195.8, 235.3, 245.4, 245.4)
sigma_x1 = 24.6
sigma_x2 = 27.8
alpha=0.05
null="mu1 >= mu2"
result=two_sample_Z_test(x1,x2,sigma_x1,sigma_x2,alpha,null)
print(result)
###

#(2) Two sample t-test: In R, both one-sample and two-sample t-tests can be performed with
#the library function t.test() with the following arguments:
  #t.test(x,y,alternative,mu,paired,var.equal,conf.level) where x,y are the data vectors, 
#alternative is a character string specifying the alternate hypothesis of 3 possible values: two.sided, less and greater. 
#One can also input a vector with all three,in which case all the 3 hypotheses are tested, 
#the default value is two.sided. For one sample, mu is the hypothesized mean of the population, and for two samples, it is the
#hypothesized difference of the means. A conf.level=0.95 sets the confidence level at 95%.

#(a) Welsch’s test: use the data sets to carry out the t-test for equality of means as H0.
#Print the results summary.
Xvar=c(4.95, 5.37, 4.70, 4.96, 4.72, 5.17, 5.28, 5.12, 5.26, 5.48)
Yvar=c(4.65, 4.86, 4.57, 4.56, 4.96, 4.63, 5.04, 4.92, 5.37, 4.58, 4.26, 4.40)

# Welch's t-test is used when 2 samples have unequal variances
result_welch=t.test(Xvar,Yvar,alternative="two.sided",var.equal=FALSE,conf.level=0.95)
print(result_welch) #reject null hypothesis

#(b) Dependent variables: Use the data sets below to carry out a paired t-test for a
#significance level of 0.05. In this case, do we need to input μ?
data_before = c(95,106,79,71,90,79,71,77,103,103,92,63,82,76)
data_after = c(97,116,82,81,82,86,107,86,94,91,85,98,91,87)
result_paired=t.test(data_before,data_after,alternative="two.sided",paired=TRUE,conf.level=0.95)
print(result_paired)
###

#(3) Two-sample proportion test: In R, the function prop.test() can perform proportion tests for one, two or more proportions. 
#Here we will also learn the Fisher’s test applicable for small samples. The input is changed as follows:
#prop.test(x,n,p=NULL,alternative="two.sided",correct=TRUE)
#fisher.test(x,alternative="two.sided",conf.int=TRUE,conf.level=0.95)
#where x is a 2x2 matrix containing the values of the contingency table under different categories for the Fisher’s test and a data vector
#of counts of successes for the prop.test(),n is a data vector containing number of trials in which x successes were observed, p is a
#vector of probabilites of successes. The alternative can be two.sided, less or more.


#(a)Perform a prop.test() test for the following problem with H0 that the proportions
#are equal. In a health survey, 520 out of 600 men and 550 out of 600 women
#questioned said they use antibiotics whenever fever continues for more than 2 days.
#We want to test whether there is a significant difference in the fraction of men and
#women who start taking antibiotics after 2 days of fever.
men_success=520
men_total=600
women_success=550
women_total=600
# we use this when you have large sample sizes and you can assume that the data follows a binomial distribution. This test uses the 
#Chi-squared approximation to test the difference between proportions.
prop_test_result=prop.test(c(men_success, women_success),c(men_total, women_total),alternative="two.sided")
print(prop_test_result) #reject H0

#(b) Perform a Fisher’s exact test for the following data to find whether the patients from
#higher income group indulge in tobacco abuse in a significantly different proportion
#than the patients from the lower income group.
#Higher-income Lower-income
#Tobacco abuse 11 17
#No abuse 42 39

# Create a 2x2 contingency table
tobacco_abuse_data=matrix(c(11,42,17,39),nrow=2)
print(tobacco_abuse_data)
#Fisher's Exact Test- we Use this when you have small sample sizes, especially when any of the expected cell frequencies in a contingency 
#table are less than 5. Fisher's test provides an exact p-value for small sample sizes, avoiding the approximations used in Chi-squared tests.
fisher_test_result = fisher.test(tobacco_abuse_data,alternative="two.sided",conf.int=TRUE,conf.level=0.95)
print(fisher_test_result) #keep H0

###

# (4) Two-sample variance test: In R, we will use the F-distribution functions (qf()) to carry
# out the two-sample variance test.

# (a) Write a function of the form two_sample_variance_test(x,y,alpha) that outputs the statistical 
#conclusion along with the statistical values of F and p-values.

two_sample_variance_test=function(x, y, alpha){
  #sample stats
  var_x =var(x)
  var_y =var(y)
  n1 =length(x)
  n2 =length(y)
  
  # Test statistic
  F_stat=var_x / var_y
  # Degrees of freedom
  df1 =n1-1
  df2 =n2-1
  # Calculate the p-value using the F-distribution
  p_value=pf(F_stat, df1, df2, lower.tail = FALSE)
  
  # Critical value at significance level alpha (two-tailed test)
  critical_value_upper=qf(1 - alpha, df1, df2)
  critical_value_lower=qf(alpha, df1, df2)
  
  # Decision rule
  reject_null=(F_stat > critical_value_upper) || (F_stat < critical_value_lower)
  conclusion=ifelse(reject_null, "Reject H0", "Fail to reject H0")
  
  return(list(F_statistic = F_stat, p_value = p_value, 
              Critical_value_lower = critical_value_lower,
              Critical_value_upper = critical_value_upper,
              Conclusion = conclusion))
}
# (b) Use the data sets below to carry out this test with α = 0.05.
x = c(1067.7, 984.3, 998.8, 1025.9, 1060.9, 959.1, 1013.8,
      1047.0, 987.8, 1051.0, 885.2, 1049.5, 1098.2, 1001.5,
      1011.1, 991.6)

y = c(957.6, 981.8, 1096.5, 984.4, 1074.3, 929.4, 1056.0,
      1012.3, 1040.7, 1099.5, 1006.1, 1064.3, 865.6, 944.4,
      1091.8, 952.1)
#two-sample variance test
result=two_sample_variance_test(x, y, alpha = 0.05)
print(result)
###

#(5) Wilcoxon signed rank test for two dependent samples: This is carried out using wilcox.test()
# function in R again, and the parameters are already described above in the one sample
# tests. Carry out this test for the following data with conf.level=0.95 for the null hypothesis 
#that the mean for the paired sample is greater than 0, i.e. the two samples have different means.
Pre_therapy=c(74, 72, 62, 58, 59, 65, 54, 63, 80, 66, 65, 64, 79, 60)
Post_therapy=c(79, 55, 53, 53, 74, 55, 64, 55, 39, 44, 37, 68, 54, 54)
#The Wilcoxon Signed Rank Test is used to compare the differences between paired samples (dependent samples).
#This test is typically used when you have two related groups (i.e., the same subjects measured before and after an intervention). 
#It tests whether the median of the differences between paired observations is significantly different from zero.
result=wilcox.test(Pre_therapy, Post_therapy,paired = TRUE, alternative="greater",conf.level = 0.95)
print(result) #reject H0
###

#(6) Wilcoxon rank sum test for unpaired samples and Mann-Whitney test: Use the wilcox.test()
# function to carry out the Wilcoxon rank sum test for two independent samples given below with the 
#alternate hypothesis that the placebo population has a smaller mean than that exposed to the drug. Use a confidence level of 95%.

#used when the assumptions of two-sample t-test (like normality) are not met, and the data are ordinal or continuous but not normally distributed.
drug= c(31.7, 75.0, 101.1, 60.5, 62.8, 59.3, 58.9, 91.3, 99.1, 52.0, 39.1)
placebo=c(59.3, 72.7, 100.5, 64.7, 69.0, 72.7, 69.6, 97.4, 100.6, 65.1, 65.7)

#Wilcoxon rank sum test (Mann-Whitney test)
result=wilcox.test(drug,placebo, alternative="less",conf.level = 0.95)
print(result)
###

#(7) Kruskal Wallis test: In R, this test is performed by kruska.test() function.
#Reform the data above into a (x,y) form where x stands for the value and y is the
#category of the group (use rep() function to label each data point according to the group), then use the above R function 
#with arguments x and y. Print the results output by the function.
Group1 <- c(220, 214, 203, 184, 186, 200, 165)
Group2 <- c(262, 193, 225, 200, 164, 266, 179)
Group3 <- c(272, 192, 190, 208, 231, 235, 141)
Group4 <- c(190, 255, 247, 278, 230, 269, 289)
#This test is performed when there's a need to prove that there are statistically significant differences between the 
#distributions of three or more independent groups-it is a non-parametric counterpart to ANOVA and used when the ANOVA assumptions are not met.
# Combining the data into a single vector (x)
x=c(Group1, Group2, Group3, Group4)
# Create the group labels (y)
y =rep(c("Group-1", "Group-2", "Group-3", "Group-4"),times=c(length(Group1), length(Group2), length(Group3), length(Group4)))
result=kruskal.test(x ~ y)
print(result) #keep H0 / fail to reject H0
###

#(8) Chi-square GoF test: Based on what we learnt in class, write a function to perform the
# GoF test based on input data of expected and observed values. We will use qchisq()
# function to get the critical value and pchisq() to get the p-value. Use the function to
# carry out the test for the following data:

#The Chi-square Goodness of Fit (GoF) Test is used to determine if there is a significant difference between the observed and expected
#frequencies in categorical data. The null hypothesis for this test is that the observed frequencies are consistent with the expected frequencies.

# Define the Chi-square GoF Test Function
chi_square_gof_test =function(observed, expected, alpha = 0.05) {
  #Chi-square statistic
  chi_square_stat=sum((observed-expected)^2 / expected)
  
  df=length(observed) - 1 #k-1
  
  # Critical value for Chi-square distribution at the given alpha (significance level)
  critical_value=qchisq(1 - alpha, df)
  
  # p-value corresponding to the calculated chi-square statistic
  p_value=pchisq(chi_square_stat, df, lower.tail = FALSE)
  
  # Conclusion based on p-value
  conclusion=ifelse(p_value < alpha, "Reject the null hypothesis", "Fail to reject the null hypothesis")
  
  return(list(Chi_Square_Statistic = chi_square_stat,Critical_Value = critical_value,p_value = p_value,Conclusion = conclusion))
}
observed=c(32, 82, 77, 49)
expected=c(40, 80, 80, 40)
result=chi_square_gof_test(observed, expected)
print(result)












