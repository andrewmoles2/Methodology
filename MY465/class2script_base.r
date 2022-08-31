### MY465 Intermediate Quantitative Analysis
### Class 2: Two-way contingency tables and Chi-square using base R (not pacakges)

#Let us first do the computer class #1 exercise, this time in R (last week we did it in Stata)
#Create a new folder (call it something like MY465/classes)
#Copy the 'blankworkspace' 'ess1.dta' and 'class2script' into the new folder
#double-click onto 'blankworkspace' - make sure it opens in R Studio

#Opening data files. Select the following three lines of code and click on 'run' in the top-right of this left-hand quadrant
library(haven)
ess1 <- read_dta("ess1.dta")
View(ess1)

#Describing variables
#Let's take a look at the distribution (i.e. the frequencies) of the variable 'badjob'
table(ess1$badjob)
#Let's look at the percentages
prop.table(table(ess1$badjob))
#remember, category number 1 is 'good job', category number 2 is 'neither good job nor bad job'
#and category number 3 is 'bad job', so 7.3% of respondents (176 people) said that the police are doing a 'bad job'
#sidenote: R requires you to continually remind it which datafile you want to use

#Let's look at the association between attitudes towards the police ('badjob') and age ('agegrp')
#Let's do a crosstabulation without percentages. Select the run the following three lines
age.badjob.tab <- table(age = ess1$agegrp, badjob = ess1$badjob)
age.badjob.tab
addmargins(age.badjob.tab)

#Is there an association?
#It is difficult to tell! So let's do a crosstabulation with row percentages
prop.table(age.badjob.tab, 1)
#remember, category number 1 for agegrp is '18-29', category number 2 is '30-49' and category number 3 is '50+'
#so 11.2% of '18-29' year olds said the the police are doing a 'bad job', 8.5% of '30-49' year olds
#said the police are doing a 'bad job', and 5.4% of '50+' year olds said the police are doing a 'bad job'
#Is there an association?

#Turning to the second exercise, which is on two-way contingency tables and Chi-square test of independent
#Looking at the association between perceived police legitimacy ('moralid1') and gender ('female')
#Let's do a crosstabulation with row percentages
female.moralid.tab <- table(female = ess1$female, moralid = ess1$moralid1)
female.moralid.tab
addmargins(female.moralid.tab)
prop.table(female.moralid.tab, 1)
#remember that female is coded 0='male' and 1='female' and moralid1 is coded from 1='strongly agree' and 5='strongly disagree'
#Is there an association?
#Let's run your first Chi-square test of independence!
chisq.test(female.moralid.tab)
#Is the association statistically significant?
#What is the null hypothesis being tested here? Remember to phrase it in terms of the population from which the sample was drawn

#Looking at the association between perceived police corruption ('corrupt1') and gender ('female')
#Let's do a crosstabulation with row percentages
female.corrupt.tab <- table(female = ess1$female, corrupt = ess1$corrupt1)
female.corrupt.tab
addmargins(female.corrupt.tab)
prop.table(female.corrupt.tab, 1)
#remember that corrupt1 is coded from 1='strongly agree' and 5='strongly disagree'
#Is there an association?
chisq.test(female.corrupt.tab)
#Is the association statistically significant?
#What is the null hypothesis being tested here?

#Looking at the association between perceived legitimacy ('moralid1') and age ('agegrp')
#Let's do a crosstabulation with row percentages
agegrp.moralid.tab <- table(agegrp = ess1$agegrp, moralid = ess1$moralid1)
agegrp.moralid.tab
addmargins(agegrp.moralid.tab)
prop.table(agegrp.moralid.tab, 1)
#Is there an association?
chisq.test(agegrp.moralid.tab)
#Is the association statistically significant?
#What is the null hypothesis being tested here?

#For the homework, assess the association between legal cynicism ('cynicism') and age ('agegrp')
#To do this, customise the syntax we have given you above
agegrp.cynicism.tab <- table(agegrp = ess1$agegrp, cynicism = ess1$cynicism)
agegrp.cynicism.tab
addmargins(agegrp.cynicism.tab)
prop.table(agegrp.cynicism.tab, 1)
#Is there an association?
chisq.test(agegrp.cynicism.tab)

#To empty the R environment
rm(list=ls())
