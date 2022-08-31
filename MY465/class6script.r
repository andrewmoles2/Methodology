### MY465 Intermediate Quantitative Analysis
### Copmputer exercise 6: multiple linear regression 2

#Opening the data file. 
load("MY465/bbc.RData")
View(bbc)

# just in case you need this, here are the commands to install packages
install.packages("psych", dependencies = TRUE)
install.packages("questionr", dependencies = TRUE)
install.packages("sjmisc", dependencies = TRUE)
install.packages("sjPlot", dependencies = TRUE)
install.packages("snakecase", dependencies = TRUE) 
install.packages("dplyr", dependencies = TRUE) 
install.packages("ggplot2")

# let's call on some packages
library(psych)
library(snakecase)
library(questionr)
library(sjmisc)
library(sjPlot)
library(dplyr)
library(ggplot2)

#let's get the mean of outgroup hostility for the 4 groups
#that are defined by combining prepost and left
group_by(bbc, prepost, left) %>% summarize(m = mean(ogh))

#Recall in the lecture that we talked about 565 people having a missing value 
#on the political ideology variable. This means that 565 research participants 
#did not answer the question about their political ideology.
#Missing values are indicated in the output as NA, so the mean for those who 
#did not answer the question pre-7/7 is 3.37 and the mean for those who 
#did not answer the question post-7/7 is 3.48.
#We will leave these 565 people aside in the analysis that follows.
frq(bbc, left, out="v")
#In other words, we will focus our analysis on the 1,466 people who did answer
#the political ideology question (688 said 'right-wing' and 778 said 'left-wing')

#let us examine the interaction between prepost and left
#recall that prepost takes two values (0=pre7/7, 1=post7/7)
#recall also that left also takes two variables (0=right, 1=left)
#let's create a crosstab of the two variables
sjt.xtab(bbc$prepost, bbc$left)
#Note that there are 433 people who have both a score of 1 on prepost
#and a score of 1 on left

#I have created what is called an 'interaction term': this is the
#multiplication of prepost and left. This gives us the frequencies:
frq(bbc, prepost_left, out="v")
#Note that there are 433 people who have a score of 1 on prepost_left
#This is no coincidence. The variable prepost_left is the multiplication
#of prepost and left.

model1 <- lm(ogh ~ prepost + left + prepost_left, data = bbc)
summary(model1)
tab_model(model1, show.se = TRUE, show.stat = TRUE, digits = 2)

#refer to slides 22 and 23 from lecture 6.2 slides
#fitted regression equation:
#outgrouphostility = 3.64 - 0.01*prepost - 0.40*left + 0.26*prepost_left
#work through the fitted values for each of the four groups:

#pre7/7 right:  ogh = 3.64 - 0.01*(0)- 0.40*(0) + 0.26*(0)=3.64
#pre7/7 left:   ogh = 3.64 - 0.01*(0)- 0.40*(1) + 0.26*(0)=3.24
#post7/7 right: ogh = 3.64 - 0.01*(1)- 0.40*(0) + 0.26*(0)=3.63
#post7/7 left:  ogh = 3.64 - 0.01*(1)- 0.40*(1) + 0.26*(1)=3.49

#note that the coefficient for the interaction term (prepost_left) 
#only 'kicks-in' when prepost=1 and left=1. This is simple maths:
#the interaction term (prepost_left) is simply the multiplication
#of the prepost variable and the left variable. Anything multiplied by zero
#equals zero, so the only instance when the interaction effect (prepost_left)
#equals one is when an individual has a one for prepost and a one for left 

#we now control for a number of variables (Muslim, age, region and ethnicity)
model2 <- lm(ogh ~ prepost + left + prepost_left + muslim + age + LondESE + ethwhite + ethblack + ethasian, data = bbc)
summary(model2)
tab_model(model2, show.se = TRUE, show.stat = TRUE, digits = 2)

#let us turn to the bullshit dataset. 
load("MY465/bs.RData")
View(bs)

#let's build up to testing an interaction effect. First, a linear model
#with freemarketideology as the outcome variable and fiscalcons and female
#as the two predictor variables:
model3 <- lm(freemarketideology ~ fiscalcons + female, data = bs)
summary(model3)
tab_model(model3, show.se = TRUE, show.stat = TRUE, digits = 2)

#let's test the interaction between fiscalcons and female
#fiscons_female is a variable that, like with prepost and left, is the 
#multiplication of fiscons and female. This means that males (female=0) all 
#have a score of 0 (because anything times zero equals zeros) and females (female=1)
#have whatever score they have for fiscons (because when you times something by one,
#the number does not change)
model4 <- lm(freemarketideology ~ fiscalcons + female + fiscalcons_female, data = bs)
summary(model4)
tab_model(model4, show.se = TRUE, show.stat = TRUE, digits = 2)

#The regression equation in this model are as follows: 
#For males (female=0), the intercept is -34.00 and the slope is 11.57.
#For females (female=1), the intercept is -34.00+20.70 and the slope is 11.57-7.17.

#For males (female=0), y=-34.00+11.57*x
#For females (female=1), y=-13.30+4.4*x

#Let's visualise the results
#the following calls the findings of the fitted linear model so it is ready
#for plotting
fit=lm(freemarketideology~fiscalcons*female,data=bs)
summary(fit)

#Let's visualise:
ggplot(bs,aes(y=freemarketideology,x=fiscalcons,color=factor(female)))+geom_point()+stat_smooth(method="lm",se=FALSE)

#This is a cute way of making an interactive plot. Note you need to install and load
#the ggiraphExtra package
install.packages("ggiraphExtra")
library(ggiraphExtra)
ggPredict(fit,colorAsFactor = TRUE,interactive=TRUE)

#finally, let's do a quadratic effect
#a fitted linear model, regressing receptivity to bullshit on endorsement of free market ideology
model5 <- lm(bullshit ~ freemarketideology, data = bs)
summary(model5)
tab_model(model5, show.se = TRUE, show.stat = TRUE, digits = 4)

#visualising the fitted model, as a linear effect
ggplot(bs, aes(x=freemarketideology, y=bullshit)) + geom_point()+stat_smooth(se=F, method='lm')

#a fitted linear model, regressing receptivity to bullshit on endorsement of free market ideology
#with a quadratic effect
model6 <- lm(bullshit ~ freemarketideology + freemarketideology_squared, data = bs)
summary(model6)
tab_model(model6, show.se = TRUE, show.stat = TRUE, digits = 4)

#visualising the fitted model, now as a quadratic effect
ggplot(bs, aes(x=freemarketideology, y=bullshit)) + geom_point()+stat_smooth(se=F, method='lm', formula=y~poly(x,2))
