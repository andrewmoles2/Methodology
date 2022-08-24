#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 7: Estimation and inference for proportions
#
#####################################################################################################

# 0. Add-on packages for today

# In today's exercise we use only Base-R functions, so no add-on packages are needed

#####################################################################################################
# 1. Reading in the data

BSA2018 <- readRDS("MY451/BSA2018_question_experiment.rds")

#####################################################################################################
# 2. Confidence interval (and significance test) for a single proportion

# The calculations use this information:
table(BSA2018$SciImportant) # The counts (frequencies) of the two possible values
prop.table(table(BSA2018$SciImportant)) # The proportions of these values, i.e. the frequencies divided by the sample size (2903)

# Test and confidence interval
## Note: Here we consider the null hypothesis pi=0.6, for illustration.

prop.test(table(BSA2018$SciImportant),correct=F,p=.6)

#####################################################################################################
# 3. Significance test and confidence interval for comparison of two proportions 

# Cross-tabulation of the frequencies
table(BSA2018$expgrp,BSA2018$SciImportant)
# Table of row proportions. Here we assign it to an object called tmp.table, so that we can then do more with that table.
tmp.table <- prop.table(table(BSA2018$expgrp,BSA2018$SciImportant),margin=1)  
tmp.table
# difference of proportions of "Yes". 
# Here, for example, tmp.table[2,1] refers to the number in row 2 and column 1 of tmp.table.
tmp.table[1,1]-tmp.table[2,1]

prop.test(table(BSA2018$expgrp,BSA2018$SciImportant), correct=F)

#####################################################################################################
# 4. Significance test and confidence interval for comparison of two proportions, 
# separately for two groups defined by a third variable (here the variable higheduc)

## Split the data into two, based on the values of higheduc
BSA2018.hieduc <- BSA2018[BSA2018$higheduc==1,]
BSA2018.loeduc <- BSA2018[BSA2018$higheduc==0,]

## Tables of proportions for them separately
## Note: This command both assigns the table to the object tmp.table, and prints it on the console.)
print(tmp.table <- prop.table(table(BSA2018.hieduc$expgrp,BSA2018.hieduc$SciImportant),margin=1))
tmp.table[1,1]-tmp.table[2,1]
print(tmp.table <- prop.table(table(BSA2018.loeduc$expgrp,BSA2018.loeduc$SciImportant),margin=1))
tmp.table[1,1]-tmp.table[2,1]

## Tests and confidence intervals for them separately
prop.test(table(BSA2018.hieduc$expgrp,BSA2018.hieduc$SciImportant), correct=F)
prop.test(table(BSA2018.loeduc$expgrp,BSA2018.loeduc$SciImportant), correct=F)

#####################################################################################################
# 5. Add commands to carry out the analysis requested in part 5 of the exercise 
# ...


#####################################################################################################
# 6. Add commands to carry out the analysis requested in part 6 of the exercise 
# ...


#####################################################################################################
# Additional material 1: Calculating the confidence interval and test in 2, but directly from the basic formulas
# (but we don't really need to do this, since the function prop.test does it for us)

p.hat <- prop.table(table(BSA2018$SciImportant))[1]
p.hat # This just prints out the value of p, so that you can see what it is
n <- sum(table(BSA2018$SciImportant))
n
se.p <- sqrt(p.hat*(1-p.hat)/n)
se.p
q95 <- qnorm(.975) # This gives the precise value of the multiplier for the interval, which is approximately 1.96
q95

## Confidence interval:
ci.p <- p.hat + c(-1,1)*q95*se.p
ci.p # Note: The function prop.test uses a very slightly modified formula for the confidence interval of a single proportion, so it does not give exactly the same numbers.

## z-test statistic:
p0 <- 0.6  # The null hypothesis value of the probability pi
se.p0 <- sqrt(p0*(1-p0)/n)
z.test <- (p.hat-p0)/se.p0
z.test 
# Note: the function prop.test in fact reports the square of z - compare
z.test^2
# ... with the output from prop.test. 

# p-value (two-sided)
2*pnorm(abs(z.test),lower.tail = F)

# prop.test in fact uses the chi2 distribution with 1 degree of freedom to get the p-value for z-squared. 
# Both version of the test give exactly the same answer
pchisq(z.test^2,df=1,lower.tail = F)

#####################################################################################################
# Additional material 2: Calculating the confidence interval and test for the difference of the two differences
# of proportions in 4 above, i.e. the difference of the differences between the two experimental groups 
# between respondents with higher vs lower levels of education.

# Step 1: The differences and their standard errors for the two groups
# Here we extract these from the results of the function prop.test
res.hi <- prop.test(table(BSA2018.hieduc$expgrp,BSA2018.hieduc$SciImportant), correct=F)
res.lo <- prop.test(table(BSA2018.loeduc$expgrp,BSA2018.loeduc$SciImportant), correct=F)

phat.hi <- res.hi$estimate
phat.hi
phat.diff.hi <- phat.hi[1]-phat.hi[2]
phat.diff.hi
ci.hi <- res.hi$conf.int
se.diff.hi <- (ci.hi[2]-ci.hi[1])/(2*qnorm(.975)) # This is the width of the confidence interval divided by 2*1.96.
se.diff.hi

phat.lo <- res.lo$estimate
phat.lo
phat.diff.lo <- phat.lo[1]-phat.lo[2]
phat.diff.lo
ci.lo <- res.lo$conf.int
se.diff.lo <- (ci.lo[2]-ci.lo[1])/(2*qnorm(.975))
se.diff.lo

# Step 2: Combining these to get the confidence interval and test for the difference of differences

diffdiff.hilo <- phat.diff.hi-phat.diff.lo
diffdiff.hilo
se.diffdiff <- sqrt(se.diff.hi^2+se.diff.lo^2)

z.diffdiff <- diffdiff.hilo/se.diffdiff
z.diffdiff
pval.diffdiff <- 2*pnorm(abs(z.diffdiff),lower.tail = F)
pval.diffdiff
#####################################################################################################
