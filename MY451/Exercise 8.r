#####################################################################################################
#
# LSE, Department of Methodology
# Introduction to quantitative methods 
# Week 8: Correlation and Simple Linear Regression
#
#####################################################################################################

# 0. Add-on packages for today

# In today's exercise we use only Base-R functions, so no add-on packages are needed

#####################################################################################################
# 1. Reading in the data

decathlon100 <- readRDS("MY451/decathlon_top100.rds")
View(decathlon100)

#####################################################################################################
# 2. Association between the result for an event (here 100m, in seconds)
# and the points for that event.
# This illustrats what a perfect linear association looks like.

# Correlation:
cor(decathlon100$r.100m,decathlon100$p.100m)

# Scatterplot
# Here the first variable goes on the horizontal axis (X-axis) of the plot,
# and the second variable on the vertical axis (Y-axis) of the plot.
plot(decathlon100$r.100m,decathlon100$p.100m)

# Simple linear regression
# Here we save the results of the linear regression in an object that we call tmp (short for "temporary").
# This way, we can then use these results for other things below, by referring to this object by name.
# In the call the lm function, the p.100m ~ r.100m is a model formula. 
# The first variable name in it, before the "~" (here p.100m) denotes the response variable, 
# and the variable name after the "~" (here r.100m) denotes the explanatory variable.

tmp <- lm(p.100m ~ r.100m, data=decathlon100) 
summary(tmp)

# Scatterplot with the least-squares fitted line added  
plot(decathlon100$r.100m,decathlon100$p.100m)
abline(tmp) # abline adds the fitted line from the fitted regression model in tmp to the plot.

#####################################################################################################
# 3. Association between the points for two different events (here 100m and long jump)

# Correlation:
cor(decathlon100$p.100m, decathlon100$p.longjump)

# Scatterplot
plot(decathlon100$p.100m,decathlon100$p.longjump)

# Simple linear regression
tmp <- lm(p.longjump ~ p.100m, data=decathlon100)
summary(tmp)

# Scatterplot with the least-squares fitted line
plot(decathlon100$p.100m,decathlon100$p.longjump)
abline(tmp)

# The same plot, but with some additional options to change the appearance of the plot
plot(decathlon100$p.100m,decathlon100$p.longjump,
      pch=16,  # This changes the point symbol to a solid circle
     xlab="Points for 100 metres",
     ylab="Points for long jump",
     )
abline(tmp, lwd=3, col="blue") # lwd specifies the width of the line


#####################################################################################################
# 4. Correlations and scatterplots between the points for all pairs of the ten events, plus the total points 

# For convenience, extract a data frame which only includes these variables
names(decathlon100)
names(decathlon100)[5:15]
dec.tmp <- decathlon100[,names(decathlon100)[5:15]] # Select variables by name
head(dec.tmp)  # Check

cor(dec.tmp)  # The correlation matrix of these variables
round(cor(dec.tmp),3)  # Same, but with numbers rounded to 3 decimal places

# All the pairwise scatterplots in one plot
pairs(dec.tmp, lower.panel = NULL) 

# Note: Use the "zoom" button in the Plots window to make it bigger. 
# Use the plot function (as in exercise 2) to see any individual plots of these more clearly, for example:

plot(dec.tmp$p.100m,dec.tmp$p.1500m)
abline(lm(dec.tmp$p.1500m~dec.tmp$p.100m))

#####################################################################################################
# 5. Simple linear regression for total points given points for individual events

tmp <- lm(p.total ~ p.100m, data=decathlon100)
summary(tmp)

# Scatterplot with the least-squares fitted line
plot(decathlon100$p.100m,decathlon100$p.total)
abline(tmp)

# The mysterious-looking command below gets the names and values of the variables for the plot from 
# the results of the linear regression (the object tmp) above. It gives the same plot as 
# the one above, but without having to type the names of the variables. Thus it will still work 
# even if you try a model with different variables (and assign its results to an object called tmp).

plot(tmp$model[,2],tmp$model[,1],xlab=names(tmp$model)[2],ylab=names(tmp$model)[1])
abline(tmp)

# Example of fitted value: Prediction of total points for someone who gets 1000 points for the pole vault
coefficients(tmp) # Check: This is how you can extract the coefficients from the fitted model

coefficients(tmp)[1]+coefficients(tmp)[2]*1000 
# Here the numbers in square brackets refer to the intercept [1] and slope [2] coefficients in object tmp.

# Note: You could also calculate this by entering the specific values for the intercept and slope coefficients like this, 
7080.472444 + 1.605926*1000
# but it is easier not to have to do that, so the command above is easier.

# Copy and edit the commands above as needed to answer the quiz questions.
# ...


#####################################################################################################
