### MY465 Intermediate Quantitative Analysis
### Computer exercise 3: comparison of means in two populations 
#(as discussed in the lecture, the two populations are the UK 
#just before and just after the July 7 London bombings)

#We are now going to open the dataset (named 'bb' as a shorthand for 'boosting belligerence') 
#as a RData file
#Select the following two lines of code and click on 'run' in the top-right of this 
#left-hand quadrant.
load("bb.RData")
View(bb)

#R packages are collections of functions and datasets developed by people across the world.
#They increase the power of R by improving existing base R functions and by adding new functionalities.
#We will use the "psych" package. Packages are stored in online repositories.
#The official repository is CRAN. The following command installs the package "psych" from CRAN. 
install.packages("psych")
#The "psych" package is described on CRAN as: 
#"A general purpose toolbox for personality, psychometric theory and experimental psychology."
#You only need to install a package on your computer once. 
#But you do need to load the package every time you start R. So let's load "psych"
library(psych)

#Part (d) 
#The following gives us descriptive statistics on all our variables
describe(bb)
#What is the mean of  the variable "imm" (attitudes towards immigrants)? 
#What is the mean of the variable "mus" (attitudes towards Muslims)? 
#We can get the same set descriptive statistics, but this time broken down using 
#the variable 'prepost', which denotes pre-terrorist attack (pre-7/7) vs. post-terrorist attack (post-7/7))
describeBy(bb, bb$prepost, )
#What are the means for "imm" pre-terrorist attack (prepost=0) and post-terrorist attack (prepost=1)?
#What are the means for "mus" pre-terrorist attack and post-terrorist attack?

#Part (e)
#Let's obtain box plots for the variable imm separately for pre-terrorist attack (pre-7/7) and 
#post-terrorist attack (post-7/7).
#We will use the package gplots. 
install.packages("gplots")
library("gplots")
boxplot2(imm~prepost,data=bb, frame = FALSE, top = TRUE, boxwex=0.3, col = c("#E69F00", "#56B4E9"), 
         main="Attitudes towards immigration by pre-terrorist attack vs post-terrorist attack",
        xlab="pre-7/7 versus post-7/7", ylab="Attitudes towards immigration")
#From part (a) we saw that the mean of imm pre-7/7 is 3.41 and the mean of imm  post-7/7 is 3.57. 
#So the mean level is a little higher post-7/7, but it is a very small difference. 
#This is reflected in the box-plots: the yellow box for pre-7/7 has very similar characteristics
#as the blue box for post-7/7.

#Two-group t-test and confidence intervals of the means difference
t.test(bb$imm[bb$prepost == 0], bb$imm[bb$prepost == 1], conf.level = 0.95, var.equal = TRUE)
#Please note that p-value = 7.793e-05 means 0.00007793 
#Is the relationship statistically significant? 

#Are you surprised given the very small mean difference in 'imm' by 'prepost' obtained above?
#Even very small differences can be statistically significant with a big enough sample size. 
#We should always think about both statistical significance and substantive significance.

#NOTE: Here the variable prepost has only two values. 
#If it had more groups and we wanted to compare only specific two 
#of them, we could do this by specifying which two groups
#(here we have specified group 0 [bb$prepost == 0] and group 1 [bb$prepost == 1])

#Part (f)
#Let's bring in political views/ideology. The survey for the boosting belligerence study measured people
#self-expressed political views. 
#We can differentiate (crudely but usefully) because people who self-identified as on the left and
#people who self-identified as on the right. 
#The dataset contains two variables: 'left' and 'right'.
#These are what are called 'dummy variables', i.e. they take one of two levels. 
#For 'left', 0 means someone who self-identified as on the right, 
#while 1 means someone who self-identified as on the left.
#For 'right', 0 means someone who self-identified as on the left, 
#while 1 means someone who self-identified as on the right.
#You just need to use only one variable because 'left' and 'right' contain exactly the same information, 
#just coded in different ways! 
#Let's use 'left', where left=0 equals right-wing and left=1 equals left-wing

#Let's repeat the boxplot for imm by prepost but also bring in political views.
#The following commands produces four boxplots, showing (from left to right in the chart)
#first the distribution of imm for pre-7/7 and right-wing
#second the distribution of imm for pre-7/7 and left-wing
#third the distribution of imm for post-7/7 and right-wing
#fourth the distribution of imm for post-7/7 and left-wing
boxplot(imm~left*prepost,data=bb, frame = FALSE, boxwex=0.3, col = c("blue", "red"), 
        main="Attitudes towards immigration by prepost and left",
        xlab="prepost (0=pre-7/7 and 1=post-7/7) and left (0=right-wing [blue] and 1=left-wing [red])", 
        ylab="Attitudes towards immigration")
#Have attitudes towards immigration seem to have changed among right-wing people (compare the two blue boxes)?
#Have attitudes towards immigration seem to have changed among left-wing people (compare the two red boxes)?

#Let's do a t-test for right-wing research participants
t.test(x = bb[bb$left == 0 & bb$prepost == 0, ]$imm,
       y = bb[bb$left == 0 & bb$prepost == 1, ]$imm,
       conf.level = 0.95, var.equal = TRUE)
#The p-value is 0.6093. Is the association statistically significant? 
#This provides boxplots just for right-wing people
boxplot2(imm~prepost,data=bb, frame = FALSE, subset = left =="0", 
         top = TRUE, boxwex=0.3, col = c("#E69F00", "#56B4E9"), 
         main="Attitudes towards immigration by pre-terrorist attack vs post-terrorist attack",
         xlab="pre-7/7 versus post-7/7", ylab="Attitudes towards immigration")

#Let's do a t-test for left-wing research participants
t.test(x = bb[bb$left == 1 & bb$prepost == 0, ]$imm,
       y = bb[bb$left == 1 & bb$prepost == 1, ]$imm,
       conf.level = 0.95, var.equal = TRUE)
#The p-value is 5.105e-07, which translates into 0.0000005105. 
#Is the association statistically significant? 
#This provides boxplots just for left-wing people
boxplot2(imm~prepost,data=bb, frame = FALSE, subset = left =="1", 
         top = TRUE, boxwex=0.3, col = c("#E69F00", "#56B4E9"), 
         main="Attitudes towards immigration by pre-terrorist attack vs post-terrorist attack",
         xlab="pre-7/7 versus post-7/7", ylab="Attitudes towards immigration")

#What is your interpretation? Does the hypothesised effect of prepost on attitudes towards immigration
#differ between left-wing respondents and right-wing respondents?

#This provides descriptive statistics for each variable, broken down by both prepost and left
describeBy(bb,list(bb$prepost,bb$left))

#Part (g)
#The following commands produces four boxplots, showing (from left to right in the chart)
#first the distribution of mus for pre-7/7 and right-wing
#second the distribution of mus for pre-7/7 and left-wing
#third the distribution of mus for post-7/7 and right-wing
#fourth the distribution of mus for post-7/7 and left-wing
boxplot(mus~left*prepost,data=bb, frame = FALSE, boxwex=0.3, col = c("blue", "red"), 
        main="Attitudes towards Muslims by prepost and left",
        xlab="prepost (0=pre-7/7 and 1=post-7/7) and left (0=right-wing [blue] and 1=left-wing [red])", 
        ylab="Attitudes towards Muslims")
#Have attitudes towards Muslims seem to have changed among right-wing people (compare the two blue boxes)?
#Have attitudes towards Muslims seem to have changed among left-wing people (compare the two red boxes)?

#Let's do a t-test for right-wing research participants
t.test(x = bb[bb$left == 0 & bb$prepost == 0, ]$mus,
       y = bb[bb$left == 0 & bb$prepost == 1, ]$mus,
       conf.level = 0.95, var.equal = TRUE)
#The p-value is 0.4481. Is the association statistically significant? 
#This provides boxplots just for right-wing people
boxplot2(mus~prepost,data=bb, frame = FALSE, subset = left =="0", 
         top = TRUE, boxwex=0.3, col = c("#E69F00", "#56B4E9"), 
         main="Attitudes towards Muslims by pre-terrorist attack vs post-terrorist attack",
         xlab="pre-7/7 versus post-7/7", ylab="Attitudes towards Muslims")

#Let's do a t-test for left-wing research participants
t.test(x = bb[bb$left == 1 & bb$prepost == 0, ]$mus,
       y = bb[bb$left == 1 & bb$prepost == 1, ]$mus,
       conf.level = 0.95, var.equal = TRUE)
#The p-value is 0.0452. 
#Is the association statistically significant? 
#This provides boxplots just for left-wing people
boxplot2(mus~prepost,data=bb, frame = FALSE, subset = left =="1", 
         top = TRUE, boxwex=0.3, col = c("#E69F00", "#56B4E9"), 
         main="Attitudes towards Muslims by pre-terrorist attack vs post-terrorist attack",
         xlab="pre-7/7 versus post-7/7", ylab="Attitudes towards Muslims")

#What is your interpretation? Does the hypothesised effect of prepost on attitudes towards Muslims
#differ between left-wing respondents and right-wing respondents?

#To empty the R environment
rm(list=ls())