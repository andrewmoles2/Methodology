### MY465 Intermediate Quantitative Analysis
### Computer exercise 5: multiple linear regression 1
### We continue with the complete Boosting Belligerence dataset (from last week) 

#Opening the data file. 
load("MY465/bbc.RData")
View(bbc)
																	
# just in case you need this, here are the commands to install packages
install.packages("psych", dependencies = TRUE)
install.packages("questionr", dependencies = TRUE)
install.packages("sjmisc", dependencies = TRUE)
install.packages("sjPlot", dependencies = TRUE)
install.packages("snakecase", dependencies = TRUE)

# let's call on the various packges
library(psych)
library(snakecase)
library(questionr)
library(sjmisc)
library(sjPlot)

#the following gives us a table of frequencies for the muslim variable (a dummy variable
#where 0=not a muslim and 1=muslim)
frq(bbc, muslim, out="v")

#fitted multiple linear regression models 
#let us start with a simple linear regression model from last week
#Y=ogh, X=fairharm
model1 <- lm(ogh ~ fairharm, data = bbc)
summary(model1)
#as with the last computer exercise, the following reproduces puts the 
#findings in a neat table. "show.std" adds the standardized slope coefficient
#and the standardized 95% confidence intervals
tab_model(model1, show.se = TRUE, show.stat = TRUE, show.std = TRUE, digits = 3)
#the following drops the standardized slope coefficient
#and the standardized 95% confidence intervals
tab_model(model1, show.se = TRUE, show.stat = TRUE, digits = 3)

#now let us add another explanatory variable
#Y=pgh, X1=fairharm, X2=authingr
model2 <- lm(ogh ~ fairharm + authingr, data = bbc)
summary(model2)
tab_model(model2, show.se = TRUE, show.stat = TRUE, digits = 3)
tab_model(model2, show.se = TRUE, show.stat = TRUE, show.std = TRUE, digits = 3)

#let us put both sets of estimates into one table
#this allows us to see what happens once one includes
#authingr in the fitted model
tab_model(model1, model2, show.se = TRUE, show.stat = TRUE, show.std = TRUE, digits = 2)
#the following drops the standardized slope coefficient
#and the standardized 95% confidence intervals
tab_model(model1, model2, show.se = TRUE, show.stat = TRUE, digits = 2)

#let us add the dummy variable muslim
#Y=pgh, X1=fairharm, X2=authingr, X3=muslim
model3 <- lm(ogh ~ fairharm + authingr + muslim, data = bbc)
summary(model3)
tab_model(model3, show.se = TRUE, show.stat = TRUE, digits = 3)
tab_model(model3, show.se = TRUE, show.stat = TRUE, show.std = TRUE, digits = 3)

#let us put all the estimates into one table
tab_model(model1, model2, model3, show.se = TRUE, show.stat = TRUE, show.std = TRUE, digits = 3)
#that's a big table. We can simplify it a little bit
tab_model(model1, model2, model3, show.se = TRUE, show.stat = TRUE, digits = 2)

#To empty the R environment, in case you want to
rm(list=ls())