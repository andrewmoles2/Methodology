### MY465 Intermediate Quantitative Analysis
### Class 4: correlation and simple linear regression
### We continue with the Boosting Belligerence dataset but we are now using a more complete file
### that continues a larger number of variables (e.g. the moral foundations variables)

#We will continue to Work within your MY465/classes folder [you may have called something different!]
#From Moodle, copy the 'blankworkspace' 'bbc.dta' (bbc.dta is short for boosting belligence complete.datafile)
# and 'class4script' into the folder. 
#Please note that there is also a 'class4scriptsupplementary' file on Moodle, which adds some bonus
#extras that are entirely optional and are not part of this computer class

#Double-click onto 'class4script'. If it opens in R Studio, you're good to go
#if it does not open R Studio, open R Studio youself and go to 'file', 
#click 'open' to open up 'class4script' and run the following:
setwd("[insert name of your folder]")

#Opening data files. Select the following two lines of code and click on 'run' in the top-right of this 
#left-hand quadrant
load("bbc.RData")
View(bbc)

# the following gives us descriptive statistics on all our variables
install.packages("psych")
library(psych)
describe(bbc)

# Let's describe some variables
install.packages("questionr")
install.packages("sjmisc")
install.packages("sjPlot")
library(questionr)
library(sjmisc)
library(sjPlot)
frq(bbc, fairharm, out="v")
frq(bbc, authingr, out="v")
frq(bbc, ogh, out="v")

#What about some descriptive statistics?
descr(bbc, fairharm, out="v")
descr(bbc, authingr, out="v")
descr(bbc, ogh, out="v")

#in base R
summary(bbc$fairharm)
summary(bbc$authingr)
summary(bbc$ogh)

#what about some graphs?
sjp.frq(bbc$fairharm)
sjp.frq(bbc$authingr)
sjp.frq(bbc$ogh)

#Correlation matrix between the three main variables of interest
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
corr <- bbc[, c(4,10,13)]
chart.Correlation(corr, histogram=TRUE, pch=19)
#note 4 is ogh (the 4th variable from the left in the dataset)
#10 is fairharm
#and 13 is authingr

#in base r
cor(bbc$fairharm, bbc$authingr, method = "pearson", use = "complete.obs")
cor(bbc$fairharm, bbc$ogh, method = "pearson", use = "complete.obs")
cor(bbc$authingr, bbc$ogh, method = "pearson", use = "complete.obs")

#Scatterplot and fitted regression model 

plot(jitter(bbc$ogh), bbc$fairharm , pch=20, col=rgb(0.1, 0.2, 0.8, 0.3) , xlab="X", ylab="Y", bty="n" )
lmfit<-lm(bbc$ogh~bbc$fairharm)
abline(lmfit)

model1 <- lm(ogh ~ fairharm, data = bbc)
summary(model1)
tab_model(model1, show.se = TRUE, show.std = TRUE)

plot(jitter(bbc$ogh), bbc$authingr , pch=20, col=rgb(0.1, 0.2, 0.8, 0.3) , xlab="X", ylab="Y", bty="n" )
lmfit<-lm(bbc$ogh~bbc$authingr)
abline(lmfit)

model2 <- lm(ogh ~ authingr, data = bbc)
summary(model2)
tab_model(model2, show.se = TRUE, show.std = TRUE)

#To empty the R environment
rm(list=ls())
