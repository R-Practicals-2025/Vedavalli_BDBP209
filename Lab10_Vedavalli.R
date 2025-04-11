#28 March 2025 Lab10-11
#I. Sampling, permutations and combinations
#(1) Sampling from a vector:
x<- seq(1,100)
s<- sample(x,10)
print(s)
#this is happening w/o replacement, by default the elements are not replaced. 
u<- sample(x,10,replace=T)
print(u)
#this will allow us to repeat/ duplicate elements in a sample.
#
#The package gtools has permutations and combinations functions that should be used
#as follows.
install.packages("gtools")
library(gtools)
x<-c("A","B","C","D")
per<-permutations(n=length(x),r=3,v=x,repeats.allowed=T) #nPr #v-vector from which the permutations are generated.
#repeats allowed-each letter can appear multiple times in the same permutation
#r- the resultant vectors here it is 3 (3 elements at a time-similar to how many 3 letter word can be formed)
print(per)

comb<-combinations(n=length(x),r=3,v=x)
print(comb) #wkt order doesn't matter in combinations (abc=bac)
#
#II. Distributions
#(1) Binomial distribution: The shape of this PDF is decided by the parameter p. Use
#n=10, p=0.4 and m=3 to do the following:
#(a) Print the probability value for the above combination of numbers. The syntax is dbinom(m,n,p).
n=10 #trials
p=0.4 #prob of success in each trial
m=3 #we want exactly 3 successes in 10 trials
prob_value<-dbinom(m,size=n,prob=p)
#dbinom-pmf of binomial P(X=m)- prob of getting exactly 3 successes in 10 trials
print(prob_value)
#
#(b) Print the cumulative probability value for the above. Syntax: pbinom(m,n,p).
cumulative_prob<-pbinom(m,size=n,prob=p)
print(cumulative_prob) #P(X<=3)
#
#(c) Find the m value corresponding to cumulative probability of 0.8. Syntax: qbinom(cum_prob,n,p)
#Find P(X<=m)=0.8
# qbinom finds the smallest m for which the cumulative probability is 0.8
m_val<-qbinom(0.8,size=10,prob=0.4)
print(m_val)
#
#(d) Print 5 points randomly sampled from the Binomial distribution using rbinom(npts,n,p).
set.seed(42)
random_samples<-rbinom(5,size=10,prob=0.4) #rbinom-generates rand nos from a binomial distbn
print(random_samples)
#the output represents how many times success occured in 10 trials for each sample

#(e) Plot the probability density function (PDF) for the above parameters. On the same
#plot, plot the PDF for p=0.7 with a different colour.
n=10
p1=0.4
p2=0.7
x_values=0:n #possible values of successes(0 to n)
#probabilities for 0.4 and 0.7
prob1<-dbinom(x_values,size=n,prob=p1)
prob2<-dbinom(x_values,size=n,prob=p2)
#plotting
plot(x_values,prob1,type="b",col="green",pch=15,lty=1,ylim=c(0,max(prob1,prob2)),
     xlab="No of successes(k) ",ylab="Probability",main="Binomial Distribution PDF")
lines(x_values,prob2,type="b",col="black",pch=16,lty=1)
legend("topright",legend=c("p=0.4","p=0.7"),col=c("green","black"),pch=c(15,16),lty=c(1,1))
#the curve peaks around the mode
#
#(f) Generate 100 and 10000 points randomly from this distribution and make a fre-
#quency table of the sampled points. Plot these as bar plots in a 2x1 grid.
set.seed(42)
par(mfrow=c(2,1))
n=10
p=0.4
#generating samples
samples_100 <- rbinom(100, size=n, prob=p)
samples_10000 <- rbinom(10000, size=n, prob=p)
#create frq tables
frq_100<-table(samples_100)
frq_10000<-table(samples_10000)
#plotting
barplot(frq_100,main="Binomial distbn of 100 samples",col="mediumvioletred",xlab="No of successes",ylab="Frequency")
barplot(frq_10000,main="Binomial distbn of 10000 samples",col="plum",xlab="No of successes",ylab="Frequency")

#(2) Hypergeometric distribution: The functions pertaining to this distribution <distn name>
#hyper. The parameters are N, K, n and k (see class notes). Carry out the following:

#(a) Plot a histogram type plot of the hypergeometric probability density function with
#N=100, K=70, p=0.3, n=12 and add text within the plot window of the parameter
#names and their values.
N=100 #popn size
K=70 #no of successes in popn
n=12 #sample size
#possible values of k
k_vals=0:n
#computing probabilities
pdf_vals<-dhyper(k_vals,K,N-K,n) #prob of drawing exactly k successes
print(pdf_vals)
#plotting
barplot(pdf_vals,names.arg=k_vals,col="pink", ylim=c(0,max(pdf_vals)),
        main="Hypergeometric distribution",xlab="No of successes(k)",ylab="Probability",border="mediumvioletred")
#adding text with parameter values inside the plot
text(x=6,y=max(pdf_vals)*0.8,labels=paste("N=",N,"\K=",K,"\nn=",n),col="black",cex=0.8)
#
#(b) Compute the cumulative probability up to x=10 and print the result after rounding off to 3 decimal places.
hyper_cpdf<-round(phyper(10,K,N-K,n),3)
print(hyper_cpdf)
#
#(c) Obtain the x value corresponding to a cumulative probability value of 0.9.
hyper_x_val<-qhyper(0.9,K,N-K,n)
print(hyper_x_val)
#
#(d) Sample 5 points randomly from this distribution and print these with two significant
#digits.
hyper_samples<-rhyper(5,K,N-K,n)
print(signif(hyper_samples,2)) #rounding off to 2 signif figures
#
#(3) Geometric distribution: The functions pertaining to this distribution <distn name>
#use geom. The parameters are p and m(trial number at which the first success is observed).
#(a) Plot 2 probability density functions for this distribution in a 1x2 grid with (i)p=0.3
#and (ii) p=0.8. What differences do you see?
p1=0.3
p2=0.8
m_vals=1:10#first success occurs at trial m
#pmf values
pdf_p1<-dgeom(m_vals-1,prob=p1) #m-1 counts the no of failures before the 1st success
pdf_p2<-dgeom(m_vals-1,prob=p2)
#plotting
par(mfrow=c(1,2))
#for 0.3
barplot(pdf_p1,names.arg=m_vals,col="darkred",main="Geometric Distribution (p=0.3)",
        xlab="Trial(m)",ylab="Probability",border="black")
#for 0.8
barplot(pdf_p2, names.arg=m_vals, col="seagreen", main="Geometric Distribution (p=0.8)", 
        xlab="Trial (m)", ylab="Probability", border="black")
#
#(b) Compute the cumulative probability up to x=4.
cpdf_geom<-pgeom(4-1,prob=0.3)
print(cpdf_geom)
#
#(c) Compute the value of m at which the cumulative probabilty is 0.2.
m_val<-qgeom(0.2,prob=0.3)+1
print(m_val)
#
#(d) Generate 6 random deviates or sample points from this distribution with p=0.4.
geom_samples=rgeom(6,prob=0.3)+1
print(geom_samples)
#(4) Negative binomial distribution: The functions pertaining to this distribution uses
#nbinom. The parameters are p, r (number of successes desired) and y (number of failures
                                                                     #before r successes).

#(a) Compute and print the negative binomial probability density for y=5, r=3 and
#p=0.3
y <- 5 #x
r <- 3 #size-target for no of successful trials,
p <- 0.3

pdf_nbinom <- dnbinom(y, size=r, prob=p) #P(Y=5) prob of getting 5 failures before 3 successes
print(pdf_nbinom)

#(b) Compute and print the cumulative negative binomial probability density up to y=5.
cpdf_nbinom<-pnbinom(5,size=3,prob=0.3) #P(Y<=5)
print(round(cpdf_nbinom,3))
#
#(c) What is the y value corresponding to a cumulative probabilty value of 0.5? (ie the
#median)
y_val<-qnbinom(0.5,size=3,prob=0.3)#P(Y<=Y)=0.5
print(y_val)
#
#(d) Print 4 random points sampled from this distribution with r=3 and p=0.3.
random_nbinom<-rnbinom(4,size=3,prob=0.3)
print(signif(random_nbinom,2))
#
#(e) Plot the negative binomial distribution function using r=10, p=0.3.
par(mfrow=c(1,1))

p=0.3
r=10
y_vals=0:70
pdf_val<-dnbinom(y_vals,size=r,prob=p)
barplot(pdf_val,names.arg=y_vals,col="indianred",main="Negative Binomial Distribution (r=10,p=0.3)",
        xlab="Failures before r successes(y)",ylab="Probability",border="black"
          )
#
#(f) Generate a frequency histogram of 10,000 random deviates from this distribution
#with r=10 and p=0.3.
r=10
p=0.3
samples <- rnbinom(10000, size=r, prob=p)

hist(samples, breaks=30, col="seagreen4",
     main="Histogram of Negative Binomial Distribution (r=10, p=0.3)",
     xlab="Number of Failures (y)", freq=T)

#
#(5) Poisson distribution: The functions pertaining to this distribution uses pois. The
#key parameter is λ and the discrete variable being m.
#(a) Compute and print the Poisson probability given λ = 10 and m = 7.
lambda<-10
m<-7
prob<-dpois(m,lambda)
print(prob)
#(b) Calculate and print the cumulative probability for the same values above.
cumulative_prob <- ppois(m, lambda)
print(round(cumulative_prob, 3))
#
#(c) Make two barplots showing a binomial probability distribution with n = 1000, p =
#0.3 and a Poisson PDF with λ = np. Do the two distributions agree? Why? Why
#not?
n=1000
p=0.3
lambda=n*p
m_values=250:350
binom_values<-dbinom(m_values,size=n,prob=p)
pois_values<-dpois(m_values,lambda)
#plotting
par(mfrow=c(1,2))
barplot(binom_values,names.arg=m_values,col="hotpink2",main="Binomial distribution(n=1000,p=0.3)",
        xlab="m values",ylab="Probability",border="black")
barplot(pois_values,names.arg=m_values,col="steelblue2",main="Poisson(lambda=300)",xlab="m values",ylab="Probabilty",
        border="black") #generally for large n and small p the binomial distribution closely resembles a Poisson
#distribution with lambda=np
legend("topright", legend = c("Binomial", "Poisson"), col = c("hotpink2", "steelblue2"), lwd = 2)

#######
###keeping n constant and changing p values###
# n = 1000
# p = 0.030
# lambda = n * p
# m_values = 0:100
# binom_values <- dbinom(m_values, size = n, prob = p)
# pois_values <- dpois(m_values, lambda)
# 
# # Plot Binomial Distribution
# plot(m_values, binom_values,xlim=c(10,100),ylim=c(0,0.1), type = "h", col = "hotpink2", lwd = 2,
#      main = "Binomial vs Poisson Distribution", xlab = "m values", ylab = "Probability")
# 
# # Overlay Poisson Distribution
# lines(m_values, pois_values,xlim=c(10,100),ylim=c(0,0.1), type = "l", col = "steelblue2", pch=16,lwd = 2)
# 
# # Add legend
# legend("topright", legend = c("Binomial", "Poisson"), col = c("hotpink2", "steelblue2"), lwd = 2)
##
###changing n values keeping p as 0.01
n = 1000
p = 0.01
lambda = n * p
m_values = 0:150
binom_values <- dbinom(m_values, size = n, prob = p)
pois_values <- dpois(m_values, lambda)

# Plot Binomial Distribution
plot(m_values, binom_values,xlim=c(0,150),ylim=c(0,0.5), type = "h", col = "hotpink2", lwd = 2,
     main = "Binomial vs Poisson Distribution", xlab = "m values", ylab = "Probability")

# Overlay Poisson Distribution
lines(m_values, pois_values,xlim=c(0,150),ylim=c(0,0.5), type = "l", col = "steelblue2", pch=16,lwd = 2)

# Add legend
legend("topright", legend = c("Binomial", "Poisson"), col = c("hotpink2", "steelblue2"), lwd = 2)

#####
####keepin p as constant 0.3 and varying n ####
n = 10000
p = 0.3
lambda = n * p
m_values = 0:10000
binom_values <- dbinom(m_values, size = n, prob = p)
pois_values <- dpois(m_values, lambda)

# Plot Binomial Distribution
plot(m_values, binom_values,xlim=c(2000,6000),ylim=c(0,0.02), type = "h", col = "hotpink2", lwd = 2,
     main = "Binomial vs Poisson Distribution", xlab = "m values", ylab = "Probability")

# Overlay Poisson Distribution
lines(m_values, pois_values,xlim=c(2000,6000),ylim=c(0,0.02), type = "l", col = "steelblue2", pch=16,lwd = 2)

# Add legend
legend("topright", legend = c("Binomial", "Poisson"), col = c("hotpink2", "steelblue2"), lwd = 2)
################################
#(d) Find the quantile value corresponding to cumulative probability of 0.22 and λ = 10.
lambda=10
m_value<-qpois(0.22,lambda)
print(m_value)
#
#(e) Obtain 10000 random sample points from a Poisson distribution with λ = 9 and
#make a histogram plot.

# Generate Poisson-distributed random values
random_sample_points <- rpois(10000, lambda = 9)

hist(random_sample_points, breaks = seq(min(random_sample_points), max(random_sample_points), by = 1),
     border = "black", 
     main = "Histogram - Poisson Distribution (λ=9)", 
     xlab = "No of events (m)", 
     probability = TRUE)

# to ensure discrete bins- breaks should be given accordingly
#
#(6) Gaussian distribution: The functions in R for the Gaussian/Normal distribution are
#for the unit normal distribution with the suffix norm. As we know the relevant parameters
#are μ and σ.

#(a) Compute and print the unit normal PDF value for μ = 12 and σ = 2.
mu=12
sigma=2
x=mu #evaluating mean at 12, we can have any x values , here(12-12) goes to 0
pdf_val<- dnorm(x,mean=mu,sd=sigma)
print(pdf_val)

#(b) Calculate and print the cumulative probability for Z = 2.0. Is this same as 1-
#CPDF(Z=-2)?
Z<-2.0
cdf_val<-pnorm(Z,mean=0,sd=1) #std normal cumulative probability
print(cdf_val)
#checking if P(Z<=2)==1-P(Z<=2)
is_equal<-cdf_val==(1-pnorm(-2,mean=0,sd=1)) 
print(is_equal) #output:TRUE
#This property holds because the normal distribution is mirror-symmetric about its mean (0 for standard normal).
#
#(c) Plot a unit normal curve for the above parameters with X range of ±4σ and add a
#text box to the plot showing the parameter symbols and their values.

#100 equally spaced X-values from 𝜇−4𝜎to μ+4σ,also wkt 99.99% of the values lie 
#btw +- 4σ
mu=12
sigma=2
x=mu
x_range<-seq(mu-4*sigma,mu+4*sigma,length=100)
y_values<-dnorm(x_range,mean=mu,sd=sigma) #pdf of X value in the x_range
#plotting
plot(x_range, y_values, type = "l", col = "limegreen", lwd = 2,
     main = "Normal Distribution Curve",
     xlab = "X", ylab = "Density")
#adding the text box
text(mu, max(y_values) * 0.8, labels = expression(paste(mu,"= 12, ",sigma,"= 2")),
     col = "mediumvioletred", cex = 1)
##
#(d) Generate the 75th quantile point for a unit normal distribution with the above
#parameters.
quantile_75 <- qnorm(0.75, mean = mu, sd = sigma)
print(quantile_75)
#
#(e) Generate 10,000 random deviates from the unit normal distribution and plot a
#histogram. On top of this plot the unit normal curve from which you sampled.
set.seed(42)
samples<-rnorm(10000,mean=0,sd=1)
#plot histogram
hist(samples,probability=T,breaks=50,col="paleturquoise3",main="Histogram of Normal Distribution Samples",
     xlab="X")
#overlaying normal density curve
curve(dnorm(x,mean=0,sd=1),col="turquoise4",lwd=2,add=TRUE)
##
#(f) Make a histogram plot of a ‘normalised’ binomial distribution with μ = np = 10
#and p = 0.5. ‘Normalised’ here means computing a variable of type W = √m−np/
#sqrt(np(1−p)) where m is the number of successes in n trials. On top of this, plot a unit normal
#distribution N(np,np(1-p). Do the two distributions agree?
#set.seed(42)
n=100
p=0.5
np=n*p
sd_binom<-sqrt(n*p*(1-p))
# Generate 10,000 binomial samples and normalize
binom_samples <- rbinom(10000, size = n, prob = p)
W <- (binom_samples - np) / sd_binom#W-normalized binomial
print(W)
#plotting histogram of W
hist(W,probability = T, breaks = 30, col = "lightskyblue",
     main = "Normalized Binomial Distribution vs. Normal",
     xlab = "W")
curve(dnorm(x,mean=0,sd=1),col="violetred",lwd=2,add=T)
#
#(g) Plot 4 Poisson probability distribution functions corresponding to λ values 1, 10,
#100 and 1000 in the same graph. On the same graph of each Poisson PDF plot,
#plot a unit normal distribution with Z =m-λ/√λ.For what value of λ(s) do the two plots agree? Use a 2x2 grid for this problem.

par(mfrow=c(2,2))
lambda_values=c(1,10,100,1000)
for (lambda in lambda_values){
  x_vals<- 0:(lambda*2) #x-axis range for Poisson
  pois_pmf<-dpois(x_vals,lambda) #poisson probabilities
  #plotting PMF
  plot(x_vals,pois_pmf,type="h",col="seagreen",lwd=2,main=paste("Poisson vs Normal dtb lambda=",lambda),
       xlab="X",ylab="Probability")
  #Overlaying normal appx
  curve(dnorm(x,mean=lambda,sd=sqrt(lambda)),col="red4",lwd=2,add=T)
}
#As lambda λ increases the Poisson dtb converges to the normal dtb, here the normal appx agrees to λ >=100
#and is almost perfect for λ=1000 (for large λ values the Poisson and Normal dtb closely match)
##
#
#(h) The library MASS is used to generate two or more vectors of normally distributed
#random numbers that are correlated with one another to a specified degree. For example,
#xy <- mvrnorm(1000, mu=c(50,60), matrix(c(4,3.7,3.7,9),2))
#will generate two sets of 1000 numbers each with a mean of 50 for the first set and
#60 for the second set. The matrix option specifies the covariance matrix of thevariables.

#(i) Execute var(xy) to check how close the variances are to our specified values
#– what is the covariance from these sample sets?
install.packages("MASS")
library(MASS)
xy<-mvrnorm(1000,mu=c(50,60),Sigma = matrix(c(4,3.7,3.7,9),2))
#sigma is the covariance matrix,mu=c(50,60) → Means of the two distributions (first: 50, second: 60)
var(xy) #comutes the sample variance and covariance btw these 2 variables returning a 2x2 covariance matric
print(var(xy))
#
#(ii) Extract the separate vectors x and y as x <- xy[,1] and y <- [xy,2] and
#plot them to look at the correlation. Print the individual variances as var(x)
#and var(y).
#To extract x and y vectors
x<-xy[,1]
y<-xy[,2]
#to visualise correlation
print(var(x))
print(var(y))

plot(x, y, main="Scatter plot of x and y", xlab="x", ylab="y", col=rgb(1, 0.3, 0.5, 0.5), pch=16,cex=0.6)
 
# # Scatter plot with multiple colors
# plot(x, y, main="Scatter plot of x and y", xlab="x", ylab="y", pch=16, col=rainbow(length(x)), cex=0.6)
#
#(iii) Are the two samples independent? If so then the sum of their variances
#should be equal to the variance of their sum.
x<-xy[,1]
y<-xy[,2]
var_x<-var(x)
var_y<-var(y)
var_x_plus_y<-var(x+y)
sum_var_xy<-var_x+var_y #this is done when the variance of the variables are not correlated
cat("Var(X) + Var(Y):", sum_var_xy, "\n")
cat("Var(X + Y):", var_x_plus_y, "\n")
#if X anY are independent then their variances shd add up[Var(X+Y)=Var(X)+Var(Y)], if they are correlated
#we need to add covariance in the formula,since we generated a correlated data (w covariance as 3.7),
#we expect that X and Y are not independent


cov_xy <- var(xy)[1,2]
var_x_plus_y <- var_x + var_y + 2 * cov_xy #VARIANCE OF THE SUM using the formula
actual_var_x_plus_y <- var(x + y) #actual variance
cat("Var(X) + Var(Y) + 2*Cov(X, Y):", var_x_plus_y, "\n")
cat("Actual Var(X + Y):", actual_var_x_plus_y, "\n")
#since the o/p matches, this confirms taht the 2 variables are correlated 
#The variables X and Y are correlated, NOT independent, because the covariance is nonzero.

###

#(iv) The reported covariance is var(xy). Compute the covariance using the
#correlation coefficient cor(x,y) and the individual variances and make sure
#this matches with the reported value.
var_x<-var(x)
var_y<-var(y)
#extracting cov from var(x,y)
reported_cov_xy<-var(xy)[1,2]
#correlation coeff
cor_xy<-cor(x,y)
#using formula
computed_cov_xy <- cor_xy * sqrt(var_x * var_y)
cat("Computed Cov(X, Y):", computed_cov_xy, "\n")
cat("Reported Cov(X, Y):", reported_cov_xy, "\n")

####
#(7) Uniform distribution: The function prefix here unif and the two parameters are the x-limits.
#(a) Generate 5 uniform random numbers between 0 and 1 and print them.
#to generate random numbers in Uniform distribution Uniform(0,1)
runif(5,min=0,max=1)
#
#b) Generate 5 random samples from a uniform distribution between 50 and 100
runif(5,min=50,max=100)

#(c) Generate 10,000 uniform deviates and plot a histogram with x-limits 1 and 2
unif_data<-runif(10000,min=1,max=2)
hist(unif_data,xlim=c(1,2),main="Histogram of Uniform deviates",col="burlywood",breaks=50)

###
#(8) Exponential distribution: Since this is derived from the Poisson distribution, the
#primary parameter here is λ. The function suffix in R is exp.

#(a) What is the probability density corresponding to x = 3 and λ = 2?
#PDF -dexp()
dexp(3,rate=2)

# (b) What is the quantile value corresponding to cumulative probability value of 0.995 for λ = 2?
qexp(0.995,rate=2)

# (c) Plot the exponential cumulative probability distributions on the same graph for λ = 2, 10, and 100
x=seq(0,1,length.out=100) #This generates a sequence of 100 evenly spaced values between 0 and 1.
cpdf_2=pexp(x,rate=2)
cpdf_10=pexp(x,rate=10)
cpdf_100=pexp(x,rate=100)
plot(x,cpdf_2,type='l',col="darkturquoise",ylim=c(0,1),xlab="x",ylab="Cumulative probability",main="Exponential CPDF",lwd=3)
lines(x,cpdf_10,col="red2",lwd=3)
lines(x,cpdf_100,col="darkmagenta",lwd=3)
legend("bottomright",legend=c("λ = 2", "λ = 10", "λ = 100"),col=c("darkturquoise","red2","darkmagenta"),lty=1,lwd=3)
par(mfrow=c(1,1))

# (d) Compute and print 4 random deviates from an exponential distribution with λ = 3
rexp(4,rate=3)
####
#(9) Gamma distribution: The function suffix in R is gamma. There are two parameters α
#(shape option) and θ(scale option).

# (a) Plot the PDFs on the same graph with alpha values of 1,2,3,4 and θ value 4 with
# colors black, blue, red and magenta respectively. This is one of the two graphs on
# a 1x2 grid. Plot the second graph with θ values of 1,2,3,4 and α = 4 with colors
# black, blue, red and magenta respectively.

#creating vector of x values
x=seq(0,100,length.out=500)
par(mfrow=c(1,2))

#1st graph we are fixing  θ=4 and varying alpha
plot(x,dgamma(x,shape=1,scale=4),type='l',col="black",lwd=2,xlab='x',ylab='Density',main="Gamma dtb-varying alpha,θ=4 ")
lines(x,dgamma(x,shape=2,scale=4),col="blue",lwd=2)
lines(x,dgamma(x,shape=3,scale=4),col="red",lwd=2)
lines(x,dgamma(x,shape=4,scale=4),col="magenta",lwd=2)
legend("topright", legend = c("α=1","α=2","α=3","α=4"), col = c("black", "blue", "red", "magenta"), lty = 1,cex=0.9)

#2nd graph - varying theta and fixing alpha
plot(x,dgamma(x,shape=4,scale=1),type='l',col="black",lwd=2,xlab='x',ylab="Density",main="Gamma dtb-varying theta,α=4")
lines(x,dgamma(x,shape=4,scale=2),col="blue",lwd=2)
lines(x,dgamma(x,shape=4,scale=3),col="red",lwd=2)
lines(x,dgamma(x,shape=4,scale=4),col="magenta",lwd=2)
legend("topright",legend=c("θ=1","θ=2","θ=3","θ=4"),col=c("black","blue","red","magenta"),lty=1,cex=0.9)
##
#(b) Compute and print the probability density corresponding to x = 6, α = 4 and θ = 1.
dgamma(6,shape = 4,scale = 1)

# (c) Cumulative probability up to x = 6
pgamma(6,shape=4,scale=1)

#(d) x value for cumulative probability of 0.95
qgamma(0.95, shape = 4, scale = 1)

#(e) Obtain 10,000 random deviates from the above gamma distribution and plot a histogram of this set.
gamma_samples=rgamma(10000,shape=4,scale=1)
hist(gamma_samples,breaks=30,xlab="x values",main="",col="plum")
###
#(10) Chi-square χ2 distribution: The suffix for the functions in R for this distribution is chisq. The key parameter is the degrees of freedom, r.
#(a) Plot the χ2 distribution with degree of freedom 2,3,5 and 10.
x <- seq(0, 30, length.out = 100)  #x-axis values
df_values <- c(2, 3, 5, 10)  # Degrees of freedom
par(mfrow=c(1,1))
plot(x,dchisq(x, df = df_values[1]), type = "l", lwd = 2, col = "darkred",
     xlab = "x", ylab = "Density", main = "Chi-Square Distribution")
lines(x, dchisq(x, df = df_values[2]), col = "darkcyan", lwd = 2)
lines(x, dchisq(x, df = df_values[3]), col = "darkorchid4", lwd = 2)
lines(x, dchisq(x, df = df_values[4]), col = "chartreuse3", lwd = 2)

legend("topright", legend = paste("df =", df_values), 
       col = c("darkred", "darkcyan","darkorchid4","chartreuse3"), lwd = 2)
#
#(b) Compute and print the probability density for x=6 and 5 degrees of freedom
dchisq(x=6,df=5)

#(c) Compute and print the cumulative probability up to x=6 and 10 degrees of freedom
dchisq(x=6,df=10)

#(d) Obtain the 85th quantile for this distribution for 6 degrees of freedom
qchisq(0.85,df=6)

#(e) Plot a histogram of 10,000 random deviates from this distribution with 6 degrees
# of freedom with 30 bins, red filled bars and text within the plot ”r=6” in an
# appropriate blank portion
chi_samples=rchisq(10000,df=6)
hist(chi_samples,col="red3",breaks=30,xlab="X values",ylab="Frequency",main="Histogram-Chi square dtb (df-6)")
# Add text label
text(15, 600,"r = 6", col = "black", cex = 1.25) #syntax- text(x,y,"labels",col,cex)

#
#(f) Assuming μ = 2 and σ = 1 compute the variable Z^2 = (x − μ)^2 /σ^2 and plot the χ2 PDF with 1 degree of freedom.
mean=2
sigma=1
x_vals=seq(-5,10,length.out=100)
z_sqd=((x_vals-mean)^2) / (sigma^2)
#plotting 
plot(z_sqd,dchisq(z_sqd,df=1),type='l',col="maroon",lwd=3,main="Chi-sq PDF with df=1",xlab="Z^2",ylab="Density")
#we know that the chi-sq values are non-negative (when we sq the negative numbers it crowds around 1), that's why it is 
#positively skewed.Also we know that as the df increases it approaches normal distbn. Since n=1 here, mean=1.
###

















