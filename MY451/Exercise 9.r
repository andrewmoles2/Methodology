#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 9: Simple linear regression and 3-way tables
#
#####################################################################################################

# 0. Add-on packages for today

# install.packages("descr") # uncomment and run this, if this package is not yet installed
library(descr)

#####################################################################################################
# 1. Reading in the data

GSS2018.educ <- readRDS("MY451/GSS2018educ.rds")

head(GSS2018.educ) # This shows the first few rows of the dataset, for checking
# As usual, you can also look at the data with View(GSS2018.educ)

#####################################################################################################
# PART I: CORRELATIONS AND SIMPLE LINEAR REGRESSION BETWEEN CONTINUOUS EDUCATION VARIABLES

#####################################################################################################
# 2. Initial analysis: Summary statistics of the respondents' age and of the continuous education variables

summary(GSS2018.educ$age)

# For convenience, define an object which contains the names of the continuous education variables.
# We use it below to ask for statistics for just these variables.
educ.continuous.names <- c("educ","coeduc","maeduc","paeduc") 

summary(GSS2018.educ[educ.continuous.names])
boxplot(GSS2018.educ[educ.continuous.names])

#####################################################################################################
# 3. Correlations between the continuous education variables

# Note: Here the argument use="pairwise.complete.obs" means that each correlation is calculated 
# using all those respondents for whom both of the two variables are observed (not missing).

cor(GSS2018.educ[educ.continuous.names], use="pairwise.complete.obs")
# The same correlations, reported with 3 decimal places for neater presentation
round(cor(GSS2018.educ[educ.continuous.names], use="pairwise.complete.obs"),3)

#####################################################################################################
# 4. Simple linear regression for respondent's years of education given their father's years of education

# Scatterplot and fitted line, as last week
plot(GSS2018.educ$paeduc, GSS2018.educ$educ,
      xlab="Father's years of education",ylab="Respondent's years of education")
abline(lm(educ ~ paeduc, data=GSS2018.educ))

# The estimated regression model
fitted.model <- lm(educ ~ paeduc, data=GSS2018.educ)
summary(fitted.model)
confint(fitted.model) # Confidence intervals for the intercept and regression coefficientof the fitted model.

# Fitted values for respondent's years of education, given some specific values for father's years of education

coefficients(fitted.model)[1]+coefficients(fitted.model)[2]*5
coefficients(fitted.model)[1]+coefficients(fitted.model)[2]*12
coefficients(fitted.model)[1]+coefficients(fitted.model)[2]*15

#####################################################################################################
# 5.
# Add commands here as needed to answer some of the quiz questions for this part of the exercise.
# ...

#####################################################################################################
# PART II: THREE-WAY CONTINGENCY TABLES BETWEEN CATEGORICAL EDUCATION VARIABLES

#####################################################################################################
# 6. Initial analysis: Tables of one or two variables at a time
# Here we focus on respondent's and their partner's education

CrossTable(GSS2018.educ$degree,GSS2018.educ$codegree,format="SPSS",prop.c=F,prop.t=F,prop.chisq=F,chisq=T)

#####################################################################################################
# 7. Three-way table: 
# Respondent's and their partner's education, separately by age group of the respondent.

# In order to use the CrossTable function, we need to split the data by the third variable 
# (here age group) and the create the two-way tables separately for each such subset of data

table(GSS2018.educ$agegroup) # Check the labels of age group

data.tmp <- GSS2018.educ[GSS2018.educ$agegroup=="Younger than 50",]
CrossTable(data.tmp$degree,data.tmp$codegree,format="SPSS",prop.c=F,prop.t=F,prop.chisq=F,chisq=T)

data.tmp <- GSS2018.educ[GSS2018.educ$agegroup=="50 or older",]
CrossTable(data.tmp$degree,data.tmp$codegree,format="SPSS",prop.c=F,prop.t=F,prop.chisq=F,chisq=T)

# This works, but it requires several steps of editing of the code if you want to do three-way tables 
# for other combinations of variables. To make things easier, we define a short R function (called MY.3waytable)
# for creating these tables in another way. 
#
# Highlight and run the lines between START OF FUNCTION and END OF FUNCTION below. 
# Then run the line after END OF FUNCTION to run the function for these variables.

# START OF NEW FUNCTION
# Convenience function for producing three-way tables
MY.3waytable <- function (formula,data)
{
# Basic 3-way table, using the function xtabs
  tab <- xtabs(formula=formula,data=data)
  n.tabs <- dim(tab)[3]

# 2-way table of frequencies and row proportions, not conditioning on the third variable  
  tab.freq2 <- addmargins(tab,3)[,,n.tabs+1]
  chi2.2 <- chisq.test(tab.freq2)
    chi2.2$data.name <- substitute(data)
  tab.freq2 <- addmargins(tab.freq2,1)
  tab.pr2 <- prop.table(tab.freq2,1)
  tab.pr2 <- round(addmargins(tab.pr2,2),3)
  tab.freq2 <- addmargins(tab.freq2,2)

# 3-way tables of frequencies and row proportions, conditioning on the third variable  
  tab.freq3 <- addmargins(tab,3)[,,-(n.tabs+1)]
  chi2.3 <- apply(tab.freq3,3,chisq.test)
    for(i in seq(n.tabs))chi2.3[[i]]$data.name <- substitute(data)
  tab.freq3 <- addmargins(tab.freq3,1)
  tab.pr3 <- prop.table(tab.freq3,c(1,3))
  tab.pr3 <- round(addmargins(tab.pr3,2),3)
  tab.freq3 <- addmargins(tab.freq3,2)
  tab.freq3 <- ftable(aperm(tab.freq3,perm=c(3,1,2))) 
  tab.pr3 <- ftable(aperm(tab.pr3,perm=c(3,1,2))) 
  
# Formatting the results  
  vnames <- attr(terms(formula),"term.labels")
  result <- vector("list",2)
  names(result) <- c("Two-way","Three-way")
    result[[1]] <- list(
    title=paste("Two-way table of ", vnames[1], " by ", vnames[2],sep=""),
    frequencies=tab.freq2,`row proportions`=tab.pr2, `chi-squared test`=chi2.2)
    result[[2]] <- list(
      title=paste("Three-way tables of ", vnames[1], " by ", vnames[2], ", given ", vnames[3],sep=""),
      frequencies=tab.freq3,`row proportions`=tab.pr3, `chi-squared tests`=chi2.3)
  
  result
  }
# END OF NEW FUNCTION

# Order of variables in the function call:
# row variable (here degree), column variable (codegree), conditioning variable (agegroup)
MY.3waytable(~degree+codegree+agegroup,data = GSS2018.educ)

#####################################################################################################
# 8. 
# Add commands here as needed to answer some of the quiz questions for this part of the exercise.
# ...

#####################################################################################################