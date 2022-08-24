#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 4: Cross-tabulations of two variables, and chi-squared tests of independence
#
#####################################################################################################

# 0. Add-on packages for today

# install.packages("haven") # uncomment and run this, if this package is not yet installed
library(haven)
# install.packages("labelled") # uncomment and run this, if this package is not yet installed
library(labelled)
# install.packages("descr") # uncomment and run this, if this package is not yet installed
library(descr)
# install.packages("rockchalk") # uncomment and run this, if this package is not yet installed
library(rockchalk)

#####################################################################################################
# 1. Reading in the data, from a Stata-format data file, using the haven package.

PublicVoice2 <- read_dta("MY451/Public Voice 2.dta")

# A look at the data and some initial data processing. See the class instructions for discussion of these. 
var_label(PublicVoice2)
lapply(PublicVoice2, attributes) # This applies the function attributes [which lists the "attributes" of a function] separately for every variable in PublicVoice2
PublicVoice2 <- data.frame(lapply(PublicVoice2,to_factor))
lapply(PublicVoice2, attributes) # The line above changed the way the information is stored in the variables

View(PublicVoice2)

#####################################################################################################
# 2. First cross-tabulation: Age group by self-reported likelihood of having had Covid-19.

# (2.0) For the C19Have variable.
CrossTable(PublicVoice2$AgeGroup,PublicVoice2$C19Have,format="SPSS",prop.c=F,prop.t=F,prop.chisq=F,chisq=T)

# Combine the first two categories of C19Have
PublicVoice2$C19Have3 <- combineLevels(PublicVoice2$C19Have,levs=c("Definitely yes","Probably yes"),newLabel = "Def or prob yes")
# The function combineLevels is from the package "rockchalk"
PublicVoice2$C19Have3 <- relevel(PublicVoice2$C19Have3, "Def or prob yes") # Re-order the categories so that this one is first.
# Check: Did the original 4 levels translate correctly into the new 3 levels
table(PublicVoice2$C19Have,PublicVoice2$C19Have3)

# From now on, we use the variable C19Have3
# (2.1) The table, including the row percentages and the results of the chi-squared test:
CrossTable(PublicVoice2$AgeGroup,PublicVoice2$C19Have3,format="SPSS",prop.c=F,prop.t=F,prop.chisq=F,chisq=T)

## (2.1b) With column percentages also included. Of these, we only want the percentages in the last ('Total') row.
CrossTable(PublicVoice2$AgeGroup,PublicVoice2$C19Have3,format="SPSS",prop.c=T,prop.t=F,prop.chisq=F,chisq=T)

# For comparison: Using base R functions
table(PublicVoice2$AgeGroup,PublicVoice2$C19Have3)
prop.table(table(PublicVoice2$AgeGroup,PublicVoice2$C19Have3), margin=1)
round(prop.table(table(PublicVoice2$AgeGroup,PublicVoice2$C19Have3), margin=1),3) # same as above, but rounded to 3 decimal places
chisq.test(PublicVoice2$AgeGroup,PublicVoice2$C19Have3)

#####################################################################################################
# 2. Tables and tests for other pairs of variables
# Add the necessary commands here, and run.
# ...


#####################################################################################################
# 3. Some more tables, now with vote in the Brexit referendum as explanatory variable
# Add the necessary commands here, and run.
# ...


#####################################################################################################
