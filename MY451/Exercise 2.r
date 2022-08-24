#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 2: Descriptive statistics for categorical data
#
#####################################################################################################


# 1. Reading in the data

EUK2010 <- readRDS("MY451/EU_Kids_Online_2010_Week2.rds")

View(EUK2010) # Take a look at the dataset.

# As discussed in the instructions, at the end of this script you can see the commands
# which we used to create this data file.

# 2. Table of frequencies and proportions, and bar chart, for the variable internet.how.often. 

# install.packages("descr") # You need to un-comment and run this line, if you did not install the package last week.
library(descr) 

freq(EUK2010$internet.how.often) 

# Tidying up the bar chart (for this, the add-on function freq uses the same options as the function barplot)
freq(EUK2010$internet.how.often,
        names=c("Every day or \n almost every day","Once or twice\n a week",
                "Once or twice\n a month","Less than\n once a month"),
        main="How often do you use the internet?", ylab="Proportion in the sample",
        sub="Data: EU Kids Online II survey, 2010", col="lightcoral"
)
# Note: The \n means "new line".

# 3. Contingency table of how often the child uses the internet, separately for the four age groups. 
# Here the first argument is the row variable for the table, the second argument the column variable.
CrossTable(EUK2010$age.group4,EUK2010$internet.how.often) 

# Same, but with some further options to make the table easier to read:
CrossTable(EUK2010$age.group4,EUK2010$internet.how.often,format="SPSS",prop.c=F,prop.t=F,prop.chisq=F) 

# Here the options do the following (see also the help file, i.e. enter ?CrossTable in the Console window).
#   format="SPSS" asks for the table to be formatted in a way which resembles the output from the SPSS statistical package.
# Whether this is nicer than the default format is a matter of taste.
#   prop.c=F [where F is short for FALSE] leaves out the column percentages
#   prop.t=F leaves out the "table percentages", i.e. the frequencies divided by the total sample size (25011)
#   prop.chisq=F leaves out contributions to the chi-squared statistics (this is related to the chi-squared test, which is explained later in the course)

# 4. Similar analyses for the variable n.contacts, as explained in the instructions.
# Add the commands below, and run them. 
# ...

# 5. Further analysis, to answer some of the multiple-choice questions. 
# Think about what tables you need to answer the questions. 
# Then add the commands below, and run them. 
# ...


#####################################################################################################
#
# Additional: Commands which were used to create the small data file used today, starting from 
# the full survey data file in SPSS format.
# 
# library(foreign)
# library(plyr)
# 
# # Reading in thedata from an SPSS data file
# EUK2010 <- read.spss("eu_kids_online_2010.sav",to.data.frame = T)
# 
# # Selecting only some variables to keep
# keep.these <- c(
#   "uniqueID",
#   "country",
#   "agechild",
#   "sexchild",
#   "QC303",
#   "QC316",
#   "QC112",
#   "QC113",
#   "QC120a","QC120b","QC120c","QC120f",
#   "QC329a","QC329b","QC329c","QC329d","QC329e","QC329f"
# )
# EUK2010 <- EUK2010[keep.these]
# 
# # Renaming some variables in the original survey data, for convenience.
# EUK2010 <- rename(EUK2010, replace=c("QC303"="internet.how.often","QC316"="n.contacts"))
# 
# # Creating grouped age variables from a continuous age variable (age in years).
# EUK2010$age.group4 <- cut(EUK2010$agechild, breaks=c(0,10,12,14,20),labels=c("9-10","11-12","13-14","15-16"))
# 
# # Creating the variable bullied.how.often, which combines information from original variables 
# # QC112 (has child ever been bullied) and QC113 (if yes, how often)
# table(EUK2010$QC112,EUK2010$QC113,useNA="always")
# v.tmp <- factor(EUK2010$QC113,levels=c(levels(EUK2010$QC113),"Never"))
# v.tmp[EUK2010$QC112=="No"] <- "Never"
# table(v.tmp) # Check that this looks correct.
# EUK2010$bullied.how.often <- v.tmp
# 
# # Creating the variable QC329.anyoftese, which indicates whether the parent has has done 
# # any of the mitigation activities desribed by the QC329 questions.
# # Note: Here | is a logical "Or".
# EUK2010$QC329.anyofthese <- EUK2010$QC329a=="Yes" |
#   EUK2010$QC329b=="Yes" |
#   EUK2010$QC329c=="Yes" |
#   EUK2010$QC329d=="Yes" |
#   EUK2010$QC329e=="Yes" |
#   EUK2010$QC329f=="Yes" 
# EUK2010$QC329.anyofthese <- factor(EUK2010$QC329.anyofthese, labels=c("No","Yes"))
# table(EUK2010$QC329.anyofthese)
# 
# # Saving the resulting dataset as a separate data file in an R (.rds) data format.
# saveRDS(EUK2010,"EU_Kids_Online_2010_Week2.rds")
#####################################################################################################


