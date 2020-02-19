####################################################################################################
# STEVE G MWANGI
# Computer Assignment 5
# R LAB 5
# 02/17/2020
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
# Here we rename the rows to be indices pertaining to this dataframe only in one sequential order
rownames(cdi4.df)=1:nrow(cdi4.df)
# And now we estimate the Linear Model
cdi4.lm=lm(PerThousCrimes~PerCapInc+PopDens+PercHS, data=cdi4.df)
###################################################################################################
# INFLUENCE calculates important measures we use to evaluate our regression model
cdi4.infl=influence(cdi4.lm)
# First three leverage values
cdi4.infl$hat[1:3]
###################################################################################################
# C3 Creating a vector of the internally standardized residuals using the vector of leverage values
# First create an object storing the results of the summary of the lm object
cdi4.lm.sum=summary(cdi4.lm)
# Look at the "attributes" of this new object
attributes(cdi4.lm.sum)
# We have sigma, residual standard error. We use it to calculate our standardized residuals
cdi4.stdRes = cdi4.lm$residuals/(cdi4.lm.sum$sigma*sqrt(1-cdi4.infl$hat))
# rstandard function also calculates the standardized residuals directly
cdi4.stdRes = rstandard(cdi4.lm)
summary(cdi4.lm)
summary(cdi4.stdRes)
# Creating a Boxplot and Normal Quantile Plot of the Standardized residuals
par(mfrow=c(1,2), mar=c(3.5,3.5,3.5,0.5),mgp=c(2.25,0.5,0),las=1) # Creating a 1 By 2 layout for the 2 plots
boxplot(cdi4.stdRes, ylab="Standardized Residuals", col = "orange", main="Standardized Residuals using Vector")
qqnorm(cdi4.stdRes, ylab="Standardized Residuals", main = "Residual Quantiles", col = "red", pch = 16)
qqline(cdi4.stdRes)
###################################################################################################
# Scatterplots
par(mfrow=c(2,2), mar=c(3.5,3.5,3.5,0.5),mgp=c(2.25,0.5,0),las=1) # Creating a 2 By 2 layout for the 4 plots

# Predictor 1
plot(cdi4.df$PerCapInc, cdi4.stdRes, col = "green" , xlab="Per Capita Personal Income",
     ylab="Standardized Residuals", pch=16, main = "Std. Residuals Vs. PerCap Income")

# Predictor 2
plot( cdi4.df$PopDens, cdi4.stdRes, col = "pink" , xlab="Population Density",
     ylab="Standardized Residuals", pch=16, main = "Std. Residuals Vs. PopDensity")

# Predictor 3
plot( cdi4.df$PercHS, cdi4.stdRes, col = "red" , xlab="Percent of Population with HS Diplomas",
     ylab="Standardized Residuals", pch=16, main = "Std. Residuals vs % w/ HS Diploma")

# Fitted Values
plot(cdi4.lm, col = "blue" , pch=16, which = 3, main = "Std. Residuals Vs. Fitted")
###################################################################################################
# C7 Cook's distance
cdi4.cooks=cooks.distance(cdi4.lm)
# Creating an index plot of Cook's distance
par(mfrow=c(1,1), mar=c(3.5,3.5,0.5,0.5),mgp=c(2.5,0.5,0),las=1)
plot(cdi4.cooks,ylab="Cook's distance", col = "violet", pch=16)
# Draw horizontal lines at 1 and 2
abline(h=c(1,2),lwd=2)
###################################################################################################
# C9 C10
# Using R's built in diagnostic tools
par(mfrow=c(2,2), mar=c(3.5,3.5,1.5,0.5),mgp=c(2.5,0.5,0),las=1)
# Diagnostic plot
plot(cdi4.lm)
###################################################################################################
# C11
# The points flagged on our diagnostic plots are:
# 15, 44, 52
# We will use the command that returns these points/rows of the cdi4.df object across all columns.
diag.table = cdi4.df[c(15,44,52),]
write.table(diag.table, file = file.choose(), sep = "\t",
            row.names = TRUE, col.names = NA)

