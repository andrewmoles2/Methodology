#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 3: Descriptive statistics for continuous variables
#
#####################################################################################################

# 0. Add-on packages for today

# install.packages("readxl") # uncomment and run this, if this package is not yet installed
library(readxl)
# install.packages("plyr") # uncomment and run this, if this package is not yet installed
library(plyr)
# install.packages("summarytools") # uncomment and run this, if this package is not yet installed
library(summarytools)
# install.packages("car") # uncomment and run this, if this package is not yet installed
library(car)

#####################################################################################################
# 1. Reading in the data

London <- read_excel("MY451/london-borough-profiles.xlsx",sheet="Data",na=c(".","n/a"))

# What do the extra arguments do here?
# sheet="Data" : include only the "Data" sheet of the Excel file (and not the "Profiles" and "Chart-Map" sheets).
# na=c(".","n/a") : which entries in the .xlsx file should be treated as indicating missing data. This is 
#                   a full stop (.) for most of the variables, but the file also uses n/a in a couple of cases. 

View(London) # You can now see the dataset in an RStudio window. Look at it again after you have run the step below.

# A further step of necessary data processing: Include only rows which correspond to London Boroughs, and delete the rest
London <- London[!is.na(London$`Inner/ Outer London`),] 
# ! means "not" and is.na means "is missing" so  !is.na means "is not missing"
# !is.na(London$`Inner/ Outer London`) thus identifies those rows of the data frame where the variable`Inner/ Outer London` is not missing  

#####################################################################################################
# 2. Basic summary statistics for one variable
# Example variable: % of the resident population of the borough who were born abroad (2015)

# Renaming the variable with a shorter name, for convenience
# List of the names of the variables. You can copy and paste the long names from here, 
# if you want to use them later for the rename command for other variables:
names(London) 

London <- rename(London, replace=c("% of resident population born abroad (2015)"="pct.born.abroad"))
# The rename function comes from the plyr package.

# Summary statistics using Base-R functions
summary(London$pct.born.abroad)
sd(London$pct.born.abroad,na.rm=T) # standard deviation; na.rm=T [T is short for TRUE] removes missing values before doing the calculation

# With nicer formatting, using the dfSummary function from the summarytools package
dfSummary(London$pct.born.abroad)

#####################################################################################################
# 3. Desriptive plots for one variable

# Histogram
hist(London$pct.born.abroad) # Very basic version
# ... with some text and colour added:
hist(London$pct.born.abroad,
     main="",xlab="% of resident population born abroad (2015)",col="lightblue")

# In case you are interested in what colours are available in R, 
# give the command colours() to see their names, and 
# google "r colours" to find charts of what they look like. 

# Stem-and-leaf plot
stem(London$pct.born.abroad)

# Box plot
boxplot(London$pct.born.abroad)
boxplot(London$pct.born.abroad,ylab="% of resident population born abroad (2015)",col="gold")

# Using the Boxplot function from the car package to be able to label the outliers in the box plot.
# (note: names in R are case-sensitive, so "Boxplot" is not the same function as "boxplot")
London <- rename(London, replace=c("Area name"="borough"))
boxplot(London$pct.born.abroad,
        ylab="% of resident population born abroad (2015)",col="gold",
        id=list(labels=London$borough)) 

#####################################################################################################
# 4. Calculating summary statistics for all the variables in the dataset.

dfSummary(London)

#####################################################################################################
# 5. Comparing the statistics for two variables: 
#   Male vs. female employment rates

London <- rename(London, replace=c("Male employment rate (2015)"="employment.male"))
London <- rename(London, replace=c("Female employment rate (2015)"="employment.female"))

# A new variable: Difference of these within each borough
London$employment.diff <- London$employment.male-London$employment.female

# Summary statistics of these:
dfSummary(London[c("employment.male","employment.female","employment.diff")]) # Here we select only these variables from the data frame London

# Side-by-side box plots of the male and female employment rates
boxplot(list(London$employment.male,London$employment.female),names=c("Men","Women"),
        main="Employment rates (%) in London boroughs",col=c("palegreen","skyblue"))

# Box plot of the difference:
boxplot(London$employment.diff)

#####################################################################################################
# 6. Comparing the statistics for one variable between two groups:
# Percentage of the population born abroad, comparing Inner London vs. Outer London boroughs.

London <- rename(London, replace=c("Inner/ Outer London"="inner.outer"))

# Summary statistics separately:
tapply(London$pct.born.abroad,London$inner.outer,dfSummary)
# The function tapply here asks that the function dfSummary should be applied 
# to the variable pct.born.abroad, separately for the two groups of boroughs 
# which are defined by the values of the variable inner.outer, 
# i.e. separately for the Inner London and Outer London boroughs.

# This is how the Boxplot function (and boxplot function) can be used to draw the boxplot
# for pct.born.abroad, separately for the groups defined by the two values of inner.outer. 

boxplot(London$pct.born.abroad~London$inner.outer, na.action = "na.omit") # Here the na.action argument is needed to deal with the fact that the % variable is missing for the City of London.

#####################################################################################################
# 7. Some more analysis of similar kinds is needed before you are ready to answer 
# all of the multiple choice questions.
# Add the necessary commands here, and run.
# ...

#####################################################################################################
