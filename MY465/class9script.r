### MY465 Intermediate Quantitative Analysis
### Computer exercise 9: binary logistic regression 2 

#load the same datafile as last week's class  (class 8): 'tf'
#recall that these are the data from study 1 of  Wilson & Rule's (2015) 
#examination of whether people overgeneralize trustworthiness 
#in extreme criminal-sentencing decisions when it should not be judicially relevant
load("tf.RData")
View(tf)

# let's call on the package ggplot2 (for fitted probabilities) 
# and the package texreg (for a nicely presented logit table)
install.packages('texreg')
library(texreg) 
library(ggplot2)

#Part 1
#let's fit the binary logistic regression with 7 predictors
model1 <- glm(sent ~ trust + zAfro + attract + maturity + zfWHR + glasses + tattoos, data=tf, family=binomial)
model1
summary(model1)
# let's use texreg to produce a table of coefficients on the logit scale
# note that we also include 95% confidence intervals for the coefficients
screenreg(model1, ci.force = TRUE)
# To get the odds ratios (i.e. the exponentiated coefficients) and their confidence intervals, 
# we can to manually extract the coefficients and the
# confidence intervals, and then exponentiate them
exp(cbind(coef(model1), confint(model1)))  

#Part 2
#let's fit a binary logistic regression with 5 predictors (dropping tattoos and glasses)
model2 <- glm(sent ~ trust + zAfro + attract + maturity + zfWHR, data=tf, family=binomial)
model2
summary(model2)
# let's use texreg to produce a table of coefficients on the logit scale
screenreg(model2, ci.force = TRUE)
# To get the odds ratios and their confidence intervals, 
# we need to manually extract the coefficients and the
# confidence intervals, and then exponentiate them
exp(cbind(coef(model2), confint(model2)))  

# let's compare model 1 and 2 by putting both sets of estimates into one table
screenreg(list(model1, model2))

# you can use htmlreg to save your table, here saving it as a word document  
htmlreg(list(model1, model2), file = "model1_model2_comparison.doc")

# let's do a likelihood ratio test, to assess whether the model
# with 5 predictors fits the data as well as the model with 7 
# predictors. If it does, then this would be evidence that we
# can remove both glasses and tattoos from the fitted model
install.packages('lmtest')
library(lmtest)
lrtest(model1, model2)

#Part 3
#just to illustrate interaction effects in logistic regression
#let's dichotomise the trust variable into 0 (below the mean) and 1 (mean and above)
#we treat this new variable (trust_dummy) as the outcome variable
#and include ethnicity, glasses and maturity as the three predictor variables
model3 <- glm(trust_dummy ~ ethnicity + glasses + maturity, data=tf, family=binomial)
model3
summary(model3)
# let's use texreg to produce a table of coefficients on the logit scale
screenreg(model3, ci.force = TRUE)
# To get the odds ratios and their confidence intervals, 
# we need to manually extract the coefficients and the
# confidence intervals, and then exponentiate them
exp(cbind(coef(model3), confint(model3)))  

#Part 4
# let's visualise the fitted probability of being deemed trustworthy
# by facial maturity (a continuous-level variable) and glasses (a 
# dichomotous variable)

newdata2 <- with(tf, data.frame(maturity = rep(seq(from = 2.5, to = 6.75, length.out = 100),
                                            2), ethnicity = 0, glasses = factor(rep(0:1, each = 100))))
newdata3 <- cbind(newdata2, predict(model3, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
ggplot(newdata3, aes(x = maturity, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
 ymax = UL, fill = glasses), alpha = 0.2) + geom_line(aes(colour = glasses), size = 1)

#Part 5
#let's add an interaction effect involving ethnicity and glasses
model4 <- glm(trust_dummy ~ ethnicity + glasses + ethnicity_glasses + maturity, data=tf, family=binomial)
model4
summary(model4)
# To get the odds ratios and their confidence intervals, 
# we need to manually extract the coefficients and the
# confidence intervals, and then exponentiate them
exp(cbind(coef(model4), confint(model4)))  

#Part 6
# the easiest way to interpret the
# interaction effect is to (a) produce fitted probabilities and (b)
# get R to do it for you!

# to get the fitted probability when glasses = 0 and ethnicity = 0
# fixing maturity at the mean (5.5)
pred_prob_00 <- predict(
  model4, 
  newdata = data.frame(maturity = 5.5, glasses = "0", ethnicity = 0, ethnicity_glasses = 0), 
  type = "response"
)
pred_prob_00

# to get the fitted probability when glasses = 0 and ethnicity = 1
# fixing maturity at the mean (5.5)
pred_prob_01 <- predict(
  model4, 
  newdata = data.frame(maturity = 5.5, glasses = "0", ethnicity = 1, ethnicity_glasses = 0), 
  type = "response"
)
pred_prob_01

# to get the fitted probability when glasses = 1 and ethnicity = 0
# fixing maturity at the mean (5.5)
pred_prob_10 <- predict(
  model4, 
  newdata = data.frame(maturity = 5.5, glasses = "1", ethnicity = 0, ethnicity_glasses = 0), 
  type = "response"
)
pred_prob_10

# to get the fitted probability when glasses = 1 and ethnicity = 1
# fixing maturity at the mean (5.5)
pred_prob_11 <- predict(
  model4, 
  newdata = data.frame(maturity = 5.5, glasses = "1", ethnicity = 1, ethnicity_glasses = 1), 
  type = "response"
)
pred_prob_11

# Note that the predicted probabilities are:
# 0.5452555	[glasses = 0, ethnicity = 0, so ethnicity_glasses = 0 (because anything times 0=0)]
# 0.3966407	[glasses = 0, ethnicity = 1, so ethnicity_glasses = 0 (because anything times 0=0)]
# 0.8951709	[glasses = 1, ethnicity = 0, so ethnicity_glasses = 0 (because anything times 0=0)]
# 0.4561134 [glasses = 1, ethnicity = 1, so ethnicity_glasses = 1 (because 1x1=1)]

#see the exercise 9 output to take you through how to do fitted probabilities using
#the fitted regression equation

