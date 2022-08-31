### MY465 Intermediate Quantitative Analysis
### Computer exercise 4: correlation and simple linear regression
### We continue with the Boosting Belligerence dataset but we are now using a more complete dataset
### that continues a larger number of variables (e.g. the moral foundations variables)

#Opening the data file. 
load("bbc.RData")
View(bbc)

#Install some packages
install.packages("psych")
install.packages("questionr")
install.packages("sjmisc")
install.packages("sjPlot")
install.packages('snakecase')
library(snakecase)
install.packages("PerformanceAnalytics")

#The following gives us tables of frequencies for:
#fairharm (combined endorsement of fairness and harm moral foundations);
#authingr (combined endorsement of authority and ingroup moral foundations); and,
#ogh (outgroup hostility)
library(psych)
library(questionr)
library(sjmisc)
library(sjPlot)
frq(bbc, fairharm, out="v")
frq(bbc, authingr, out="v")
frq(bbc, ogh, out="v")
#what about some graphs?
plot_frq(bbc$fairharm)
plot_frq(bbc$authingr)
plot_frq(bbc$ogh)

#Take a look at the distributions of these three variables. Note that we are going
#to treat these variables as continuous. This is, in practice, often done, even
#for variables like "fairharm" and "authingr" that do not have as many different
#values as one might normally expect a continuous-level variable to have.

#The following gives descriptive statistics for the same three variables
descr(bbc, fairharm, out="v")
descr(bbc, authingr, out="v")
descr(bbc, ogh, out="v")

#The most important things here are the means, the standard deviations (sd) and the ranges
#(minimum and maximum values of the variable)

#Correlation matrix between the three main variables of interest
library("PerformanceAnalytics")
corr <- bbc[, c(4,10,13)]
chart.Correlation(corr, histogram=TRUE, pch=19)
#note 4 is ogh (the 4th variable from the left in the dataset)
#10 is fairharm (the 10th variable from the left in the dataset)
#and 13 is authingr (the 13th variable from the left in the dataset)

#The chart.Correlation function produces a figure that will appear in the 'Plots' section
#of the right-hand quadrant.
#Which correlation is the highest? 
#Which correlations are positive and which correlations are negative?

#The following produces a scatterplot with fitted line for ogh given fairharm using the jitter option.
plot(jitter(bbc$fairharm), bbc$ogh , pch=20, col=rgb(0.1, 0.2, 0.8, 0.3) , xlab="X", ylab="Y", bty="n" )
lmfit<-lm(bbc$ogh~bbc$fairharm)
abline(lmfit)

#The following fits  a linear regression model between these variables, regressing
#ogh (i.e. specifies ogh as the response variable) on fairharm (i.e. specifies fairharm as the explanatory variable).
model1 <- lm(ogh ~ fairharm, data = bbc)
summary(model1)
tab_model(model1, show.se = TRUE, show.std = TRUE)
#Interpret the estimated regression (slope) coefficient for fairharm (under "Estimate") 
#and the R-squared statistic (under "Multiple R-squared").

#Based on the fitted model, what is the predicted level of outgroup hostility for
#a respondent with a level of 4 on the fairness/harm foundations variable?

#The following produces a scatterplot with fitted line for ogh given authingr using the jitter option.
plot_scatter(jitter(bbc$authingr), bbc$ogh, pch=20, col=rgb(0.1, 0.2, 0.8, 0.3) , xlab="X", ylab="Y", bty="n" )
lmfit<-lm(bbc$ogh~bbc$authingr)
abline(lmfit)

#The following fits  a linear regression model between these variables, regressing
#ogh (i.e. specifies ogh as the response variable) on authingr (i.e. specifies authingr as the explanatory variable).
model2 <- lm(ogh ~ authingr, data = bbc)
summary(model2)
tab_model(model2, show.se = TRUE, show.std = TRUE)

#Interpret the estimated regression (slope) coefficient.
#How were the t-test statistic and the p-value for the regression coefficient calculated?
#Is there a statistically significant linear association between
#outgroup hostility and combined endorsement of the authority and ingroup moral foundations?
#What is the 95% confidence interval for the coefficient?
#Based on the fitted model, what is the predicted level of outgroup hostility for
#a respondent with a level of 4 on the authority/ingroup loyalty foundations variable?

