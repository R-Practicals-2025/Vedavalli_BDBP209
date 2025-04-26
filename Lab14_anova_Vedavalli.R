#IV. One-way ANOVA (multiple sample tests)

#(1) ANOVA test on people on the Titanic ship
#(a) Read in the data file called titanic.csv. Make histogram plots of groups of
# people marked as ‘1st’, ‘2nd’ and ‘3rd’ (use about 30 bins) to check whether the
# three samples have approximately the same variance and are looking approximately
# normal. Then we are justified in using ANOVA. Make the plots in a 3x1 grid. Our
# null hypothesis is that the mean age is same among the 3 groups of people.

titanic_data=read.csv("/home/ibab/Downloads/titanic_R.csv")
titanic_data$passenger_class =as.factor(titanic_data$passenger_class)
par(mfrow=c(3,1))
#for each class 1,2 and 3 we are plotting histograms
hist(titanic_data$age[titanic_data$passenger_class == "1st"],main="Age Distribution-1st Class",
     xlab="Age",col="plum")
hist(titanic_data$age[titanic_data$passenger_class=="2nd"],main="Age Distribution-2nd Class",
     xlab="Age",col="powderblue")
hist(titanic_data$age[titanic_data$passenger_class=="3rd"],main="Age Distribution-3rd Class",
     xlab="Age",col="palegreen")
#
#(b) To quantitatively verify the equal variance assumption, we are going to determine
# the mean and standard deviations from each group. Load the package dplyr, we will use two 
# functions group_by() and summarise(). Study the output of the following commands:
# titanic_by_passenger_class<- group_by(titanicData,passenger_class)
# summarise(titanic_by_passenger_class, group_mean=mean(age,na.rm=TRUE),
#           group_sd=sd(age,na.rm=TRUE)
# What do you find? Are the standard deviations similar between the groups? Print a statement showing 
# the conclusion of this comparison.

library(dplyr)

titanic_by_passenger_class=group_by(titanic_data, passenger_class)

titanic_summary=summarise(titanic_by_passenger_class,group_mean = mean(age, na.rm = TRUE),
  group_sd = sd(age, na.rm = TRUE))

print(titanic_summary)
#the std deviations of age across the 3 classes are relatively similar.This could support the
#assumption of ANOVA which is the homogeneity of variances
#
#(c) We fit the ANOVA model to the data using lm() function. This function takes
# a formula and data frame as arguments. A model formula takes the form of a
# response variable followed by a tilde( ) and then at least one explanatory variable.
# Here we will give age~passenger_class which tells R to ‘fit’ a model in which
# age of passengers are grouped by the variable passenger_class. The command therefore is
# lmresults <- lm(age~passenger_class, data=titanicData)
# anova(lmresults)
# The anova() function returns the ANOVA table as output. What is your statistical
# inference/decision from the table, and therefore what is the statistical conclusion?

#fitting a model (linear)
lmresults=lm(age~passenger_class,data=titanic_data)
#performing ANOVA
anova_results=anova(lmresults)
print(anova_results) #we can see three stars, which means it is very significant (signif difference)
#in mean ages across the 3 classes. And the p-value is much smaller (10^-16) we reject the H0 and we
#can conclude that there's difference in mean atleast in 1 group

#(d) The ANOVA tells us that at least one group has a mean different from the others,
# but does not tell us which group means are actually different. A Tukey-Kramer’s
# test tests the null hypothesis that there is no difference between the population
# means of all pairs of groups. This is invoked in R by using TukeyHSD() function.
# Execute the following command;
# TukeyHSD(aov(lmresults))
# Look at the columns labeled ‘diff’ and ‘p adj’. The p-values are calculated using a
# 95% confidence interval, and ‘lwr’ and ‘upr’ denote lower and upper bounds of the
# interval. From the output you should be able to see that the CIs do not include
# zero, and since the p-value is less than 0.05 in all the cases, the H0 is rejected for all
# pairs, and we will conclude that the means of the three populations are significantly
# different from each other.

tukey_results=TukeyHSD(aov(lmresults))
print(tukey_results)
#the diff- pairwise differences are significant
#none of the CIs include 0, we can say that the mean ages in all 3 classes differs significantly from each other
#ANOVA's conclusion is supported,that the age is not evenly distributed
#adj p value is less than 0.05 for all 3 classes- statistically significant differences
#
#(e) Let us also perform a Kruskal-Wallis test for the above data since the test does not
# need us to assume normal distribution like ANOVA. Execute
# kruskal.test(age~passenger,data=titanidData). Check whether the p value
# leads to the same conclusion as the parametric test above.
kruskal_result=kruskal.test(age ~ passenger_class,data = titanic_data)
print(kruskal_result)
#here also the p-val is less than 0.05, in the range of 1*10^-16 which is way too small,this confirms the ANOVA
#conclusion, that atleasr 1 group mean differs.
###


#(2)Cuckoo egg size problem:
#(a)Plot a multiple histogram showing cuckoo egg lengths by host species.

cuckoo_data <- read.csv("/home/ibab/Downloads/cuckooeggs.csv")
head(cuckoo_data)
str(cuckoo_data)
n_total <- nrow(malaria_data)
print(n_total)
unique_hosts=unique(cuckoo_data$host_species)
n_hosts=length(unique_hosts)

#Set up plotting layout (adjust based on number of hosts)
par(mfrow = c(ceiling(n_hosts/2), 2))  # 2 columns

# Plot histogram per host
for (h in unique_hosts) {
  hist(cuckoo_data$egg_length[cuckoo_data$host_species == h],
       main = paste("Host:", h),xlab = "Egg Length",col = "mediumvioletred",
       breaks = 6)}
#
#(b) Calculate a table that shows the mean and standard deviation of length of cuckoo
# eggs for each host species. Look at the graph and the table. For these data, would
# ANOVA be a valid method to test for differences between host species in the lengths
# of cuckoo eggs in their nests?
# Use tapply to compute mean and standard deviation
mean_table=tapply(cuckoo_data$egg_length, cuckoo_data$host_species, mean, na.rm = TRUE)
sd_table=tapply(cuckoo_data$egg_length, cuckoo_data$host_species,sd, na.rm = TRUE)

#Combine into a single data frame
summary_stats <- data.frame(Host = names(mean_table),Mean_Length = round(mean_table, 2),
  SD_Length = round(sd_table, 2))

print(summary_stats)
#
#(c)Use ANOVA to test for a difference between host species in the mean size of the
#cuckoo eggs in their nests. What is your conclusion?
anova_model=lm(egg_length ~ host_species, data = cuckoo_data)
anova_result=anova(anova_model)
print(anova_result) #3 STARS-highly significant, pval<0.05 so we reject H0, atleast 1 group has a significantly 
#different mean cuckoo egg length

#(d) Assuming that ANOVA rejected the null hypotheses of no mean differences, use a
# Tukey-Kramer test to decide which pairs of host species are significantly different
# from each other in cuckoo egg mean length. What is your conclusion?
tukey_result = TukeyHSD(aov(anova_model))
print(tukey_result)
#pairs with no signif diff (padj>0.05) is pied wagtail-hedge sparrow,robin-hedge sparrow,tree pipit-hedge sparrow
#pied wagtail-meadow pipit,robin-meadow pipt,Robin-Pied Wagtail,Tree Pipit-Pied Wagtail,Tree Pipit-Robin
#These results support the hypothesis that cuckoos adapt egg size to match their host species,
#as not all host species receive eggs of similar sizes.
###

#(3) Maize and malaria problem:
# (a) Plot a multiple histogram to show the relationship between level of maize production and the incidence of malaria.
malaria_data=read.csv("/home/ibab/Downloads/malaria vs maize.csv")
str(malaria_data)
head(malaria_data)
table(malaria_data)
yield_levels=unique(malaria_data$maize_yield)
par(mfrow = c(length(yield_levels), 1))

for (level in yield_levels) {
  hist(malaria_data$incidence_rate_per_ten_thousand[malaria_data$maize_yield == level],
       main = paste("Malaria Incidence rate per 10000-",level,"maize yield"),
       xlab = "Incidence per 10,000 people",
       col = "yellow3",breaks = "Sturges")
}
# (b) ANOVA is a logical choice of method to test differences in the mean rate of malaria between sites differing
#in level of maize production. Calculate the standard deviation of the incidence rate for each level of maize yield. Do these data seem to
#conform to the assumptions of ANOVA? Describe any violations of assumptions you identify.

#checking std devn for each yield level
tapply(malaria_data$incidence_rate_per_ten_thousand, malaria_data$maize_yield, sd, na.rm = TRUE)
#as the std devn in different groups vary significantly, ANOVA's assumptions (homogenity in variance) are violated.

# (c) Compute the log of the incidence rate and redraw the multiple histograms for
# different levels of maize yield. Calculate the standard deviation of the log incidence
# rate for each level of maize yield. Does the log-transformed data better meet the
# assumptions of ANOVA than did the untransformed data?

# Add log-transformed column to the data- log transformation stabilizes the variances and make the distb normal
#this will transform the data in such a way that the differences in the variance is much smaller,meeting the ANOVA's assumptions 
malaria_data$log_incidence=log(malaria_data$incidence_rate_per_ten_thousand)

# Plot histograms of log-transformed incidence
par(mfrow = c(length(yield_levels), 1))
for (level in yield_levels) {
  hist(malaria_data$log_incidence[malaria_data$maize_yield == level],
       main = paste("Log(Malaria Incidence) -", level, "maize yield"),
       xlab = "log(Incidence)", col = "mediumorchid4", breaks = 15)
}

# Check standard deviations of log-incidence
tapply(malaria_data$log_incidence, malaria_data$maize_yield, sd, na.rm = TRUE)
#The difference in the std deviations has gone down significantly, so it indeed meets the ANOVA assumptions better.

# (d) Test for an association between maize yield and malaria incidence.
#fitting the ANOVA model
anova_model=lm(log_incidence~maize_yield, data = malaria_data)
anova_result=anova(anova_model)
print(anova_result)
#pvalue < 0.05, and acc to the signif codes - 3 stars which means the difference is highly significant
#therefore there is a statistically significant difference in log malaria incidence between maize yield levels.
#we can conclude that maize production is associated with malaria incidence

###
#(4)Circadian rhythms of diseased animals:
#(a)Plot a histogram of each of the three groups. Do these data match the assumptions of an ANOVA?
circadian_data=read.csv("/home/ibab/Downloads/circadian mutant health.csv")
str(circadian_data)
head(circadian_data)
table(circadian_data)
unique_genotypes=unique(circadian_data$genotype)
n_groups=length(unique_genotypes)

par(mfrow = c(n_groups, 1))

for (g in unique_genotypes) {
  hist(circadian_data$days_to_death[circadian_data$genotype == g],
       main = paste("Lifespan Distribution -", g),
       xlab = "Lifespan (days)", 
       col = "darkslategray3", breaks ="Sturges")
}
#normality assumption is violated here,also the variances seems to be very different across the groups. So it
#doesn't match the assumptions of ANOVA.

#(b) Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups
#of flies.
kruskal.test(days_to_death ~ genotype, data = circadian_data)

#p val <0.05, we reject the H0, mwaning there's statistically signif diff in lifespan b/w atleast 2 of the genotypes


