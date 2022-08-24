#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 10: Multiple Linear Regression
#
#####################################################################################################

# 0. Add-on packages for today

# install.packages("stargazer") # uncomment and run this, if this package is not yet installed
library(stargazer)

#####################################################################################################
# 1. Reading in the data, and initial data processing

GallupWorld <- readRDS("MY451/GallupWorld.rds")

View(GallupWorld) 

# Turn the income group variable into a "factor" variable
table(GallupWorld$IncomeGroup)

GallupWorld$IncomeGroup <- factor(GallupWorld$IncomeGroup, 
    labels=c("Low income","Lower middle income","Higher middle income","High income"))
table(GallupWorld$IncomeGroup)

#####################################################################################################
# 2. Initial analysis: Summary statistics of the survey-based media freedom variable
# for the whole sample, and separately by country income group

summary(GallupWorld$mediafree)
hist(GallupWorld$mediafree)

by(GallupWorld$mediafree,GallupWorld$IncomeGroup,summary)
boxplot(mediafree~IncomeGroup,data=GallupWorld)

#####################################################################################################
# LINEAR REGRESSION MODELS FOR THE SURVEY-BASED MEDIA FREEDOM VARIABLE
#
# 3. Model given only the country income group, in four categories

mod.fit <- lm(mediafree~IncomeGroup,data=GallupWorld)
# Basic table of regression results, and confidence intervals for the coefficients
summary(mod.fit)
confint(mod.fit)

# The same table of regression results, but now formatted using the stargazer add-on package.
# Note: This is simply to show how the tables in the lecture slides were obtained. 
# For this exercise, you do not otherwise need to use this package.
stargazer(mod.fit, style="all", type="text")

#####################################################################################################
# 4. Model given only the country income group, in two categories (High income vs. the rest)

# Create a new variable which combines the first three categories in one.
GallupWorld$IncomeGroup2 <- GallupWorld$IncomeGroup
table(GallupWorld$IncomeGroup2)
levels(GallupWorld$IncomeGroup2) <- c("Other","Other","Other","High")
table(GallupWorld$IncomeGroup2)

summary(lm(mediafree~IncomeGroup2,data=GallupWorld))

#####################################################################################################
# 5. Model given two-category income group and percentage of urban population 

mod.fit <- lm(mediafree~IncomeGroup2+urban_pct,data=GallupWorld)
summary(mod.fit)
confint(mod.fit) # Confidence intervals for the regression coefficients

# Examples of calculating fitted values from the estimated model 
# Fitted value at a single value of the explanatory variables 

predict(mod.fit,newdata=data.frame(IncomeGroup2="Other",urban_pct=50))

# Two show several fitted values at once, 
# create first a small dataset of the values at which you want to calculate the fitted values

pred.dat <- data.frame(
  IncomeGroup2=c("Other","Other","High","High"),
  urban_pct=c(40,80,40,80)
  )

pred.dat

pred.tmp <- predict(mod.fit,newdata=pred.dat)
data.frame(pred.dat, fitted.value=round(pred.tmp,1) )

#####################################################################################################
# 6. Models with further explanatory variables

# Adding survey-based estimate of internet access
summary(lm(mediafree~IncomeGroup2+urban_pct+internetaccess,data=GallupWorld))

# Adding a measure of corruption in the country
summary(lm(mediafree~IncomeGroup2+urban_pct+CPI,data=GallupWorld))

#####################################################################################################
# LINEAR REGRESSION MODEL FOR COUNTRY AVERAGE OF INDIVIDUALS' LIFE EVALUATION INDEX
#
# 7.
# Add commands here as needed to answer some of the quiz questions for this part of the exercise.
# ...

#####################################################################################################