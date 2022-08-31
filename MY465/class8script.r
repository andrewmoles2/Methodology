### MY465 Intermediate Quantitative Analysis
### Computer exercise 8 (week 9): binary logistic regression 1 

# let's install some new packages 
install.packages("aod")
install.packages("tidyverse")
install.packages("gifski")
install.packages("lattice")
install.packages("scales")
install.packages("kableExtra")
install.packages("pROC")
install.packages("descr")
install.packages("tidyr")
install.packages("rlang")

# let's call on some packages
library(ggplot2)
library(tidyverse)
library(gifski)
library(lattice)
library(scales)
library(kableExtra)
library(pROC)
library(aod)
library(dplyr)
library(knitr)
library(ggplot2)
library(descr)
library(questionr)
library(sjmisc)
library(sjPlot)

#First dataset
#we have simulated the Pfizer trial data
load("covidtrial.RData")
View(covidtrial)

#let's take a look at the results of the trial 
sjt.xtab(covidtrial$treatment, covidtrial$infection, show.row.prc = TRUE, digits = 5)

#Part two
#let's fit your first binary logistic regression
model1 <- glm(infection~treatment,data=covidtrial,family="binomial")
print(summary(model1),digits=5)
exp(-4.90381)
exp(-3.01518)
exp(cbind(coef(model1), confint(model1)))  

#Part three
# to get the fitted probability when treatment = 0 
pred_prob_0 <- predict(model1, newdata = data.frame(treatment = 0), type = "response")
pred_prob_0
# to get the fitted probability when treatment = 1
pred_prob_1 <- predict(  model1, newdata = data.frame(treatment = 1), type = "response")
pred_prob_1

#Second dataset
#load a new datafile: 'tf', which is short for 'trustface'
#these are the data from study 1 of  Wilson & Rule's (2015) 
#examination of whether people overgeneralize trustworthiness 
#in extreme criminal-sentencing decisions when it should not be judicially relevant
load("tf.RData")
View(tf)

#Part four
#let's do some basic frequencies to get to know the data
frq(tf, sent, out="v") 
frq(tf, glasses, out="v") 
#trust is a continuous-level variable so a histogram is a good option
ggplot(tf, aes(trust)) +
  geom_histogram()
#let's make it pointlessly pretty, just to show off the power of R!
ggplot(tf, aes(trust, fill = cut(trust, 100))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70) +
  theme_minimal() +
  labs(x = "trust", y = "n") +
  ggtitle("Histogram of trust")

#let's tweak the colours 
histogram <- ggplot(tf, aes(trust, fill = cut(trust, 100))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(240, 10), c = 120, l = 70) +
  theme_minimal() +
  labs(x = "trust", y = "n") +
  ggtitle("Histogram of trust")
histogram + scale_fill_discrete(h = c(180, 360), c = 150, l = 80)
#let's tweak the colours once more
histogram + scale_fill_discrete(h = c(90, 210), c = 30, l = 50)

#let's replicate a couple of pieces of analysis from the lecture
sjt.xtab(tf$glasses, tf$sent, show.row.prc = TRUE)
#from this, we can calculate the conditional probalities of Y=1 (sent=1) given glasses (glasses=0 and glasses=1)
#what's the conditional probability of getting the death penalty for people not wearing glasses?
#what's the conditional probability of getting the death penalty for people wearing glasses?

#this gives us the conditional probabilities of Y=1 (sent=1) given trust 
ggplot(mapping = aes(x = tf$trust, y = tf$sent)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_smooth(mapping = aes(color = "Logistic"), method = "glm", formula = y ~ x, method.args = list(family = binomial(link = "logit")), se = FALSE) +
  geom_point(size = 3, alpha = 0.50) +
  scale_y_continuous(breaks = seq(-1, 1, 0.20), labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#d55e00", "#0072b2")) +
  labs(x = expression(trust), y = NULL, title = bquote("Prediction:" ~ hat(pi)), color = "Model Form") 

#Part five
#let's fit a binary logistic regression model
#sent is the outcome variable and glasses and trust are the 
#two predictor variables
model2 <- glm(sent ~ glasses + trust, data=tf, family=binomial)
model2
summary(model2)
#To get the odds ratios and their confidence intervals, 
#we need to extract the coefficients and the
#confidence intervals, and then exponentiate them
exp(cbind(coef(model2), confint(model2)))  

#What is your interpretation of the odds ratio for glasses (1.52) and the odds ratio for trust (0.67)?
#Take a look at the lecture 8.4 slides

#What is the fitted probability of getting the death penalty (sent=1)
#when trust is set at its mean (2.78) and when glasses=0?
#What about when trust is set at its mean (2.78) and when glasses=1?
#Again, take a look at the lecture 8.4 slides

#Part six
#Let's add maturity to the model
model3 <- glm(sent ~ glasses + trust + maturity, data=tf, family=binomial)
model3
summary(model3)

#Part seven
#The following fixes maturity at its mean and plots the fitted probabilities
#for sent=1 given different levels of trust and glasses (like in lecture 8.4)
newdata2 <- with(tf, data.frame(trust = rep(seq(from = 1, to = 5, length.out = 100),
2), maturity = mean(maturity), glasses = factor(rep(0:1, each = 100))))
newdata3 <- cbind(newdata2, predict(model3, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
ggplot(newdata3, aes(x = trust, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
ymax = UL, fill = glasses), alpha = 0.2) + geom_line(aes(colour = glasses), 
size = 1)