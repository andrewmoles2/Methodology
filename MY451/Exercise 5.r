#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 5: Inference for two population means
#
#####################################################################################################

# 0. Add-on packages for today

# install.packages("labelled") # uncomment and run this, if this package is not yet installed
library(labelled)
# install.packages("summarytools") # uncomment and run this, if this package is not yet installed
library(summarytools)
# install.packages("descr") # uncomment and run this, if this package is not yet installed
library(descr) 
# install.packages("lattice") # uncomment and run this, if this package is not yet installed
library(lattice) 

#####################################################################################################
# 1. Reading in the data, and initial look at the data.

EUK.FI.FR <- readRDS("MY451/EU_Kids_Online_FI_FR.rds")

# Longer labels of the variables in this dataset. 
var_label(EUK.FI.FR)

# Summary statistics of the variables, for the two countries combined:
dfSummary(EUK.FI.FR)

# In particular, numbers of respondents from the two countries. 
freq(EUK.FI.FR$Country,plot=F)

#####################################################################################################
# 2. Summary statistics for comparing two groups (Finland and France).
# First variable: Age when the child first used the internet.

# Basic Summary statistics
by(EUK.FI.FR$First_Use,EUK.FI.FR$Country,dfSummary)

# Histograms
# This histogram function comes from the 'lattice' package
histogram(~First_Use|Country,data=EUK.FI.FR)

# Box plots
boxplot(First_Use~Country,data=EUK.FI.FR)

#####################################################################################################
# 3. Statistical inference for comparing the two population means:
# Two-sample t-test and confidence interval for the difference of the means. 
# First variable: Age when the child first used the internet.

# Using the function t.test to carry out the test and to calculate the confidence interval.
t.test(First_Use ~ Country,data=EUK.FI.FR,var.equal=T)

#####################################################################################################
# 4. 
# Add the necessary commands here, and run.
# ...

#####################################################################################################


#####################################################################################################
## Additional commands (not directly needed for this exercise)
## Using the variable First_Use for illustration

## The printed output from the t.test function does not show the difference of the 
## sample means or its standard error. If you wanted to see these, here is how you could obtain them 
## from the results of the function:
t.results <- t.test(First_Use ~ Country,data=EUK.FI.FR,var.equal=T) # Save the results of the test, here in an object called t.results
t.results$estimate # the two group means
t.results$estimate[1]-t.results$estimate[2] # The difference of the means. (Note: This comes out labelled as "mean in group Finland", but it is the difference of the means.)
t.results$stderr # standard error of the difference 
## Confirm that the t-test statistic is indeed obtained from these: 
(t.results$estimate[1]-t.results$estimate[2])/t.results$stderr

## Demonstration of how the test and confidence intervals could be calculated without the t.test function, 
## using even more basic R functions and the formulas for the test and the confidence intervals 
## (as shown in the lectures):

## Sample values of the variable, separately by the two groups:
y1 <- EUK.FI.FR$First_Use[EUK.FI.FR$Country=="Finland"] # Variable for the respondents in Finland.
y1 <- y1[!is.na(y1)] # Remove respondents for whom this variable is missing.
y2 <- EUK.FI.FR$First_Use[EUK.FI.FR$Country=="France"] # Variable for the respondents in France.
y2 <- y2[!is.na(y2)] # Remove respondents for whom this variable is missing.
## Group means
m1 <- mean(y1)
m2 <- mean(y2)
## Difference of the means
m.diff <- m1-m2
## Sample sizes by group
n1 <- length(y1)
n2 <- length(y2)

## List the means, sample sizes and mean difference
c(m1,m2,n1,n2,m.diff)

## Variances by group
s2.1 <- var(y1)
s2.2 <- var(y2)
## Standard error of the mean difference
s.diff <- sqrt(((n1-1)*s2.1+(n2-1)*s2.2)/(n1+n2-2))
se.diff <- s.diff*sqrt(1/n1+1/n2)              
se.diff

## t-test statistic and its p-value
t.diff <- m.diff/se.diff
t.diff # Print out the value of t.diff
df.diff <- n1+n2-2
df.diff
p.diff <- 2*pt(abs(t.diff),df=df.diff,lower.tail = F)
p.diff

# 95% confidence interval for the difference of means
ci95.diff <- m.diff + qt(.975,df.diff)*se.diff*c(-1,1)
ci95.diff

#####################################################################################################

