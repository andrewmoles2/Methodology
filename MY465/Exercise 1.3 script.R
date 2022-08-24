### MY465 Intermediate Quantitative Analysis
### Exercise 1 part C: Descriptive statistics with tables and graphs of frequencies

#Opening data files. The following imports a stata dataset (.dta) into R
#install.packages("haven")
library(haven)
ess1 <- read_dta("MY465/ess1.dta")
View(ess1)

#Installing and loading packages that you will use throughout the course
install.packages("questionr")
install.packages("sjmisc")
install.packages("sjlabelled")
install.packages("sjPlot")
install.packages("dplyr")
library(questionr)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(dplyr)

#Describing variables
#Let's take a look at all of the variables in the datafile:
describe(ess1)

#Let's take a look at the distribution (i.e. the frequencies) of the variable 'badjob'
frq(ess1, badjob, out="v")
#sidenote: R requires you to continually remind it which datafile you want to use

#Let's produce a simple bar-chart for the variable badjob
plot_frq(ess1$badjob)

#To request multiple frequencies simultaneously, simply add them to the text.
#For example, here is 'badjob' and 'agegrp' 
frq(ess1, badjob, agegrp, out="v") 

#Looking at the association between attitudes towards the police ('badjob) and age ('agegrp')
#Let's do a crosstabulation without percentages
sjt.xtab(ess1$agegrp, ess1$badjob)
#Is there an association?

#It is difficult to tell! So let's do a crosstabulation with row percentages
sjt.xtab(ess1$agegrp, ess1$badjob, show.row.prc = TRUE)
#Is there an association?

#To produce a grouped bar chart we use the following command:
plot_xtab(ess1$badjob, ess1$agegrp, show.total = FALSE)

#to explore the full set of variables
describe(ess1)
