#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 1 (exercise 1.2): Initial practice with the R software
#
#####################################################################################################

# 1. Reading in the data

MediaTimeUse <- read.csv("MY451/MediaTimeUse2.csv")

# The next command opens the dataset in one of the RStudio windows, so that you can see what it looks like.
# You can switch back to the script file by clicking on the Exercise 1.2.R tab
View(MediaTimeUse)  

#####################################################################################################
# 2. An example of analysis: a simple table of frequencies and proportions

# Table of frequencies
table(MediaTimeUse$Work) 

# Table of proportions
prop.table(table(MediaTimeUse$Work)) 
# ...or...
empl.table <- table(MediaTimeUse$Work) 
prop.table(empl.table) 

#####################################################################################################
# 3. An example of a plot: Bar chart 

barplot(table(MediaTimeUse$Work))
# The same plot, but with some additional arguments to change its appearance:
barplot(table(MediaTimeUse$Work),col="lightblue",xlab="Respondent's labour market status")

#####################################################################################################
# 4. Information on R functions: Help

?barplot # See the results in the Help window. You can also enter this in the Console window.
# or
help(barplot)

#####################################################################################################
# 5. Example of add-on packages in R.
# Here to do the same things as in 2 and 3, but more conveniently and with more nicely formatted output

# Install a package. This only needs to be done once.
install.packages("descr") # This installs a package called descr.

# General information on what the package does
help(package="descr") 

# Load the package 
library(descr)

# Using a function from this package to create the table and plot:
freq(MediaTimeUse$Work)
# ...with some extra options (for the plot, the function freq uses the same arguments as the function barplot above)
freq(MediaTimeUse$Work,col="lightblue",xlab="Respondent's labour market status")

#####################################################################################################
