### MY465 Intermediate Quantitative Analysis
### Computer exercise 7: multiple linear regression 3 (ANOVA and ANCOVA)

#We continue with Boosting Belligerence
#You need to download the latest version of bbc.RData from Moodle 
#(under week 8: linear regression 4)
#This is because you need to be working on the latest version of the dataset
#Open the data file
load("bbc.RData")
View(bbc)

# just in case you need this, here are the commands to install some packages
install.packages("psych", dependencies = TRUE)
install.packages("questionr", dependencies = TRUE)
install.packages("sjmisc", dependencies = TRUE)
install.packages("sjPlot", dependencies = TRUE)
install.packages("snakecase", dependencies = TRUE) 
install.packages("dplyr", dependencies = TRUE) 
install.packages("ggplot2", dependencies = TRUE)
install.packages("car", dependencies = TRUE)

# let's call on some packages
library(psych)
library(sjmisc)
library(sjPlot)
library(dplyr)
library(ggplot2)
library(car)

#part 1
#let's get the mean of outgroup hostility 
#before and after the terrorist attacks
group_by(bbc, prepost) %>% summarize(m = mean(ogh))
#note that the mean difference is 3.53-3.41=0.12

#let's test the statistical significance using simple linear regression
model1 <- lm(ogh ~ prepost, data = bbc)
summary(model1)
tab_model(model1, show.se = TRUE, show.stat = TRUE, digits = 2)
#note again that the mean difference is 0.12 (2 decimal places)

#let's test the statistical significance using one-way ANOVA
#in base R
model2.aov <- aov(ogh ~ prepost, data = bbc)
summary(model2.aov)

#and now using the package "car", which gives us more flexibility (so is preferable)
model3 = lm(ogh ~ prepost, data = bbc)
Anova(model3, type = "II")

#let's visualise the group mean difference
groups <- group_by(bbc, prepost) # this just prepares it for us to calculate everything within each condition
plot.bbc <- summarise(groups,
                       mean = mean(ogh, na.rm=TRUE),
                       sd = sd(ogh, na.rm=TRUE),
                       n = n(),
                       se=sd/sqrt(n),
                       ci = qt(0.975, df=n-1)*se)
ggplot(plot.bbc, aes(x=prepost, y=mean, group = factor(1))) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  ggtitle("Outgroup hostility by prepost")

#let's estimate the effect size, here the standardized difference (d)
#this is the mean difference (0.1214) 
#divided by the residual standard error (0.820)
#both taken from the fitted linear regression model
0.1214/0.820

#part 2
#let's assess the interaction between prepost and left
#using two-way ANOVA
#let us use the Car package. Note that for a statistically significant interaction
#effect, we specify type III (if the interaction isn't significant, we revert to type II) 
model4 = lm(ogh ~ prepost + left + prepost:left, data = bbc)
Anova(model4, type = "III")

#when interpreting the results of an interaction like this, it is best
#to visualise the means for each of the 4 groups 
interaction.plot(x.factor     = bbc$prepost,
                 trace.factor = bbc$left,
                 response     = bbc$ogh,
                 fun = mean,
                 type="b",
                 col=c("black","red"),          ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#the same using multiple linear regression
model5 <- lm(ogh ~ prepost + left + prepost_left, data = bbc)
summary(model5)
tab_model(model5, show.se = TRUE, show.stat = TRUE, digits = 2)

#recall the fitted values from last week 
#fitted regression equation: outgrouphostility = 3.64 - 0.01*prepost - 0.40*left + 0.26*prepost_left
#the fitted values for each of the four groups:

#pre7/7 right: 3.64 - 0.01*(0)- 0.40*(0) + 0.26*(0)=3.64
#pre7/7 left:3.64 - 0.01*(0)- 0.40*(1) + 0.26*(0)=3.24
#post7/7 right: 3.64 - 0.01*(1)- 0.40*(0) + 0.26*(0)=3.63
#post7/7 left: 3.64 - 0.01*(1)- 0.40*(1) + 0.26*(1)=3.49

#what about bringing in the 'missing' from the ideology variable?
#ideology3cats takes three levels: 1=left, 2=missing and 3=right
#one strength of ANOVA versus regression is that one can conveniently
#test the statistical significance of an interaction that involves more
#than two categories.

#let's use ANOVA to test the association between prepost and ideology_3cats
summary(aov(ogh~as.factor(ideology_3cats), bbc))
#the above shows that ideology_3cats is statistically significant
#but we don't know whether all the contrasts/comparisons are significant
#here there are 3: left vs missing, left vs right, and missing vs right
#let's do some pairwaise t-tests
pairwise.t.test(bbc$ogh, bbc$ideology_3cats, p.adj = "none")
#we can see that left and missing are not statistically significant (the p-value
#for this t-test is 0.24) but the other two contrasts are statistically significant 
#(right vs missing and right vs left)
#as we saw in the lecture, it is generally important to account for multiple comparisons
#the following does that (although there are only 3 categories, so it's not a big issue here)
#while also giving the mean difference in each of the three contrasts
TukeyHSD(aov(ogh~as.factor(ideology_3cats), bbc)) 
#note that the P-values are slightly different because we are using the Tukey procedure
#that takes into account multiple comparisons
#note also that the mean differences are larger for 1v3 (0.26) and 2v3 (0.21) than for 1v2 (0.05)

#this is a slightly different way of doing the above and it also incldues a visualisation
#you will need to install a new package called multcompView
install.packages("multcompView", dependencies = TRUE)
library(multcompView)
model6=lm( bbc$ogh ~ bbc$ideology3cats )
ANOVA=aov(model6)
summary(model6)
TUKEY <- TukeyHSD(x=ANOVA, 'bbc$ideology3cats', conf.level=0.95)
plot(TUKEY , las=1 , col="brown")
#the figure gives you the mean differences for the three contrasts, alongside 95% confidence 
#intervals

#let's do it with regression. we include two dummy variables for ideology (below, we drop
#ideology_missing, so 'missing' is the reference category)
#let's also include two interaction effects: prepostleft (this is prepost*ideology_left)
#and prepostright (this is prepost*ideology_right)
model7 <- lm(ogh ~ ideology_left + ideology_right + prepost + prepostleft + prepostright, data = bbc)
summary(model7)
tab_model(model7, show.se = TRUE, show.stat = TRUE, digits = 2)
#we therefore get two statistical tests, one for prepostleft and one for prepostright
#note that the reference category makes a difference
model8 <- lm(ogh ~ ideology_missing + ideology_right + prepost + prepostmissing + prepostright, data = bbc)
summary(model8)
tab_model(model8, show.se = TRUE, show.stat = TRUE, digits = 2)
#prepostright is now statistically significant! 
#this is because it is the contrast between left and right that is important here
#this gives the equivalent results using ANOVA
model9 = lm(ogh ~ ideology_missing + ideology_right + prepost + prepostmissing + prepostright,
            data = bbc)
Anova(model9, type = "III")
#Again, note that for a statistically significant interaction
#effect, we specify type III (if the interaction isn't significant, we revert to type II) 

#to conveniently get a multiple coefficient test (for prepostleft and prepostright 
#simultaneously) we can fit an ANOVA like this:
model10 = lm(ogh ~ prepost + ideology3cats + prepost:ideology3cats,
           data = bbc)
Anova(model10,
      type = "III")
#the P-value for prepost:ideology3cats is 0.011, so we can reject the null hypothesis
#for the interaction effect as a whole

#we can visualise the results from the ANOVA by plotting the means
interaction.plot(x.factor     = bbc$prepost,
                 trace.factor = bbc$ideology3cats,
                 response     = bbc$ogh,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
#note that the black line marked 1 is for left, the red line marked 2 is for missing
#and the green line marked 3 is for right
#the findings from the regression in model 7 above tells us that the association between
#prepost and ogh is statistically significantly different for left (black line) and 
#right (green line) but not for either of the contrasts involving missing (left vs missing 
#and right vs missing).

#part 3
#let's assess the interaction between prepost and age
#using ANCOVA. The following fits a model that includes
#the main effect of prepost, the main effect of age, and the interaction term
#note we use type III because there is an interaction
model11 = lm(ogh ~ prepost + age + prepost:age, data = bbc)
Anova(model11, type = "III")

#the same using multiple linear regression
model12 <- lm(ogh ~ prepost + age + prepost_age, data = bbc)
summary(model12)
tab_model(model12, show.se = TRUE, show.stat = TRUE, digits = 2)

#let's assess the role of prepost and age (without an interaction)
#using two-way ANOVA. Note we go to type II because there is no interaction included
model13 = lm(ogh ~ prepost + age, data = bbc)
Anova(model13, type = "II")

#the same using multiple linear regression
model14 <- lm(ogh ~ prepost + age, data = bbc)
summary(model14)
tab_model(model14, show.se = TRUE, show.stat = TRUE, digits = 2)

#Let's visualise:
ggplot(bbc,aes(y=ogh,x=age,color=factor(prepost)))+geom_point()+stat_smooth(method="lm",se=FALSE)

