####################################################################################################
# STEVE G MWANGI
# Computer Assignment 4
# R LAB 4
# 02/10/2020
###################################################################################################
# Investigating predictors of county-level crime totals of serious crime incidents in 1990.
# These data are from 440 of the most populous US counties
# Data Set includes:
# ID : ID number of the county
# County : Name of County
# State : County is in
# LandArea : County Square Miles
# TotalPop: County Population
# TotalSeriousCrime: Total number of Serious Crime
# PercHS : Percent of population age >= 25 with atleast HS Diploma
# PercBach : Percent of population with atleast a bachelors
# PercPov : Percent of population with income below poverty level
# PercUnemp :Percent unemployed
# PerCapInc : PerCapita Income
# TotInc : Total Person Income
# Region : Geographic Region in US with 1 : NE, 2 : North Central, 3 : S, 4: W
###################################################################################################
setwd(choose.dir())
cdi.df = read.csv(file.choose())
###################################################################################################
# Verifying correct data
# Viewing the first 6 lines of the data frame
head(cdi.df) # Checking the first 6 lines of the data frame
dim(cdi.df) # Function to verify that the data fram has 13 Columns and 440 Rows
summary(cdi.df)
###################################################################################################
# Adding a new column for the population density by taking the total population and dividing it 
# by the land area.
cdi.df$PopDens=cdi.df$TotalPop/cdi.df$LandArea
# Creating another column for Crimes / 1000 People by dividing the total serious crimes by the
# total population and then multiplying by 1000
cdi.df$PerThousCrimes = cdi.df$TotalSeriousCrime/cdi.df$TotalPop * 1000
###################################################################################################
# Creating a new Data Frame for the Western Region : 4
# The qualifier inside the bracke tells R to only keep the
# Rows that satisfy the condition indicated
cdi4.df=cdi.df[cdi.df$Region==4,]
###################################################################################################
#C4 Creating 6 Scatter plots for the Western Region selected above
summary(cdi4.df)

# A
par(mfrow=c(2,3), mar=c(3.5,3.5,3.5,0.5),mgp=c(2.25,0.5,0),las=1) # Creating a 2 By 3 layout for the 6 plots
# Scatterplots
plot(cdi4.df$PerCapInc, col = "red" , cdi4.df$PerThousCrimes, xlab="Per Capita Personal Income",
     ylab="Crime Rate per 1000 people", pch=16, main = "A. Per Capita Vs. Crime Rate")
cdi4a.lm = lm(PerThousCrimes~PerCapInc, data = cdi4.df) # Creating a linear model for the abline for a
# Adding fitted line
abline(cdi4a.lm$coefficients, lwd=1)

# BB
plot(cdi4.df$PopDens, col = "blue" , cdi4.df$PerThousCrimes, xlab="Population Density",
     ylab="Crime Rate per 1000 people", pch=16, main = "B. Population Density Vs. Crime Rate")
cdi4b.lm = lm(PerThousCrimes~PopDens, data = cdi4.df) # Creating a linear model for the abline for b
# Adding fitted line
abline(cdi4b.lm$coefficients, lwd=1)

# CCC
plot(cdi4.df$PercHS, col = "green" , cdi4.df$PerThousCrimes, xlab="Percent of Population with HS Diplomas",
     ylab="Crime Rate per 1000 people", pch=16, main = "% w/ HS Diploma Vs.Crime Rate")
cdi4c.lm = lm(PerThousCrimes~PercHS, data = cdi4.df) # Creating a linear model for the abline for c
# Adding fitted line
abline(cdi4c.lm$coefficients, lwd=1)

# DDDD
plot(cdi4.df$PerCapInc, col = "black" , cdi4.df$PopDens, xlab="Per Capita Personal Income",
     ylab="Population Density", pch=16, main = "D. Per Capita Income Vs. Population Density")
cdi4d.lm = lm(PopDens~PerCapInc, data = cdi4.df) # Creating a linear model for the abline for d
# Adding fitted line
abline(cdi4d.lm$coefficients, lwd=1)

# EEEEE
plot(cdi4.df$PerCapInc, col = "orange" , cdi4.df$PercHS, xlab="Per Capita Personal Income",
     ylab="Percent of Population with HS Dimplomas", pch=16, main = "E. Per Capita Income Vs. % w/ HS Diploma")
cdi4e.lm = lm(PercHS~PerCapInc, data = cdi4.df) # Creating a linear model for the abline for e
# Adding fitted line
abline(cdi4e.lm$coefficients, lwd=1)

# FFFFFF
plot(cdi4.df$PopDens, col = "purple" , cdi4.df$PercHS, xlab="Population Density",
     ylab="Percent of Population with HS Dimplomas", pch=16, main = "Population Density Vs. % w/ HS Diploma")
cdi4f.lm = lm(PercHS~PopDens, data = cdi4.df) # Creating a linear model for the abline for f
# Adding fitted line
abline(cdi4f.lm$coefficients, lwd=1)
###################################################################################################
#C6 Creating a multiple regression model with the number of serious crimes predicted by
# a linear combination of per-capita income, population densit, and percentage of population
# with high school diplomas.
cdi4.lm = lm(PerThousCrimes~PerCapInc+PopDens+PercHS, data = cdi4.df)
summary(cdi4.lm)
###################################################################################################