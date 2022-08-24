### MY465 Intermediate Quantitative Analysis
### Computer exercise 2: Two-way contingency tables and Chi-square

#Opening the data file
#This is how to import a stata data file (like we did  last week) using the package "haven"
#If you have not yet installed the package "haven", run
install.packages("haven")

library(haven)
ess1 <- read_dta("MY465/ess1.dta")

#Open up the data into the top-left quadrant
View(ess1)

#Loading some packages
library(questionr)
library(sjmisc)
library(sjPlot)

#Let's take a look at all of the variables in the datafile
describe(ess1)

#The first row indicates that there are a total of 2,422 rows of data
#each row corresponds to the survey responses for one person. 
#There are 27 variables in the dataset. 
#After the general summary of the dataset, R provides information about each of the 18 variables. 
#Look at the first variable, Gender
#The first row returns the variable name immediately after the $ sign. 
#This is followed by the variable label. 
#The second row summarises the responses for the first 10 rows of the dataset. 
#For example, person 1 is female, person 2 is male, person 3 is male, person 4 is male, and so on.

#Scroll down the list of variables until you find the one corresponding to 'badjob'
#To run a frequency of the variable badjob we use the following code
frq(ess1, badjob, out="v") 
#The first piece of text frq tells R that we wish to run a frequency. 
#The text inside the parentheses then tells R what we want a frequency of. 
#Here we have typed ess1, badjob, which tells R we would like a frequency of the variable badjob. 
#The command out="v" is used to display the results more clearly in the viewer window.

#In addition to a frequency table we can also graphically examine our data. 
#To produce a simple bar-chart for the variable badjob
plot_frq(ess1$badjob)
#The vertical axis records the frequency count for each category of attitude, 
#while the percentages are included above each bar. 
#We can see that most of the sample (n=1,727 or 71.7%) said that the police did a 'good job.'

#Constructing crosstabs/contingency tables
#The first thing to do is to decide which variable one wants to think of as the 
#independent variable (the one that has an effect on the other). 
#It makes  sense to think of age as the independent variable here.
#We want to put agegrp in the columns and badjob in the rows. 
#And remember, we also need to tell R which dataset we are using
sjt.xtab(ess1$agegrp, ess1$badjob)

#You have asked R to construct a table that will show you the number of all possible 
#combinations of attitudes towards the police ess1$badjob by different age groups ess1$age+grp. 
#As you can see, it is quite difficult to interpret the results without converting to percentages. 
#We are examining differences in attitudes towards police across age groups, 
#therefore we need to request column percentages. 
#This is done by adding the text show.col.prc = TRUE.
sjt.xtab(ess1$agegrp, ess1$badjob, show.row.prc = TRUE)

#Grouped bar charts
#We may also want to produce more complex bar charts to compare groups of people. 
#For example, we may be interested in whether different age groups report different 
#attitudes towards the police. To produce a grouped bar chart we use the following command
plot_xtab(ess1$badjob, ess1$agegrp, show.total=FALSE)
#We can see that older people (aged 50+) are more likely to report positive attitudes
#towards the police than middle-aged people (30-49), 
#and middle aged people are, in turn, more likely to report positive attitudes 
#towards the police than young people (aged 18-29).

#Importantly, we have to refer to the correct data frame using ess1$ every time 
#a new variable is mentioned. 
#We include the instructions show.total=FALSE to prevent R from also including a 
#total bar for each attitude category. See what happens if you remove this text. 
#You may also want to see what happens if you include the variables in the opposite order.

#Let's look at the association between perceived legitimacy ('moralid1') and gender ('female')
#Let's do a crosstabulation with row percentages
sjt.xtab(ess1$female, ess1$moralid1, show.row.prc = TRUE)
#Is there an association in the sample?
#Is the association statistically significant (look at the Chi-square statistic, df and p-value)?
#What is the null hypothesis being tested here?

#If we want to request multiple frequencies simultaneously, we simply add them to the text. 
#For example, suppose we also wanted to examine agegrp.
frq(ess1, badjob, agegrp, out="v") 

#Let's look at the association between perceived corrupt ('corrupt1') and gender ('female')
#Let's do a crosstabulation with row percentages
sjt.xtab(ess1$female, ess1$corrupt1, show.row.prc = TRUE)
#Is there an association?
#Is the association statistically significant?
#What is the null hypothesis being tested here?

#Let's look at the association between perceived legitimacy ('moralid1') and age ('agegrp')
#Let's do a crosstabulation with row percentages
sjt.xtab(ess1$agegrp, ess1$moralid1, show.row.prc = TRUE)
#Is there an association?
#Is the association statistically significant?
#What is the null hypothesis being tested here?

#Finally, let's assess the association between legal cynicism ('cynicism') and age ('agegrp')
#To do this, customise the syntax we have given you (i.e. swap moralid1 with cynicism)


