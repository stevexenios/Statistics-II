###################################################################################################
# Steve 
# INTRO
###################################################################################################
#OBJECTIVES:
#1. Introduction to R and RStudio
###################################################################################################
#C1.
#Submitted R Script.
#--------------------------------------------------------------------------------------------------
# Writing Objects
# obj1 = 2
#Arithmetic: +, - , *,/,^,(raise to the power of)
2+3
##[1] 5
2-3
## [1] -1
2*3 
## [1] 6  
2/3  
## [1] 0.6666667
2^3  
## [1] 8 
# Other mathematical functions:
log(2) # takes the natural logarithm
## [1] 0.6931472  
exp(2) # equivalent to e^2  
## [1] 7.389056  
abs(-2)# takes the absolute value  
## [1] 2 
sqrt(2) # takes the square root 
## [1] 1.414214
#Creating Objects and assign a value to the object
obj1 = 2
#Multiplying the value and assigning it to a another object
obj2 = 3 * obj1
obj1
obj2
#Order of operations is preserved with parenthesis
2 + 3 * 4 ## 14
(2+3)*4 ## 20
# Function to get working directory
getwd()
# Function to set working directory
setwd(getwd())
# This can also be dones by clicking on Session > Set Working Directory > Choose Directory
# Readiing a csv file, and assigning contents to a data frame
anscombe.df = read.csv(file.choose())
# Verifying it was read correctly by printing first 6 lines
head(anscombe.df)
##     X1 Y1
## 1 8.04 10
## 2 6.95  8
## 3 7.58 13
## 4 8.81  9
## 5 8.33 11
## 6 9.96 14
#To view entire object use View with caps V..
View(anscombe.df)
# Getting the names of the columns using
names(anscombe.df)
# Referring to columns using names
anscombe.df$X1
# Referring to columns using [rows, columns]
anscombe.df[,1] # all rows of first column
anscombe.df[1,1] # Row 1 of first column
# Mean of first and second column
mean(anscombe.df$X1)
mean(anscombe.df$Y1)
# sd and Variance of X1
sd(anscombe.df$X1)
var(anscombe.df$X1)
# 5 number stats for the first column
summary(anscombe.df$X1)
# 5 number stats for the df
summary(anscombe.df)
# Creating a new Column that is a Multiple of Column X1
anscombe.df$X1a = anscombe.df$X1 * 2
# Verify created column
#Histogram of the column X1
head(anscombe.df)
hist(anscombe.df$X1)
#Boxplot of the column X1
boxplot(anscombe.df$X1)
# Scatter plot of Y1 on Y axis, and X1 on X axis
plot(anscombe.df$X1, anscombe.df$Y1)
help.search("anova")
#--------------------------------------------------------------------------------------------------
# R GRAPHING
# Creating a random set of 100 x variables using rnorm that takes in number of vars, mean of all vars, and sd of vars
x.fake =  rnorm(100, 10, 2)
# Creating a similar set for 100 y variables,
# which have a linear relationship with the X vars, with a random error component added
y.fake = 100 - 1.5 * x.fake + rnorm(100, 0, 2)

# BOXPLOTS
# mfrow sets up number of rows, followed by columns for the number of plots = rows x columns 
# mar sets up the margine for sides 1,2,3,4 of each plot
# mpg specifies the position at which the axis labels, axis tick marks and axis lines are draw
# las 1 specifies that reading alignment of the numbers is that of reading direction
par(mfrow = c(1, 2), mar = c(3, 3, 0.5, 0.5), mgp = c(2, 0.5,0), las = 1)
# by default, boxplot are vertical
boxplot(x.fake, ylab = "Fake X data")
boxplot(y.fake, ylab = "Fake y data")
# Horizontal boxplots
boxplot(x.fake, xlab = "Fake X data", horizontal = TRUE)
boxplot(x.fake, xlab = "Fake Y data", horizontal = TRUE)

# HISTOGRAMS
# Increase top (side = 3) for inserting title with main command
par(mfrow = c(1, 2), mar = c(3, 3, 2, 0.5), mgp = c(2, 0.5,0), las = 1)
hist(x.fake, xlab = "Fake X data", col = "blue", main = "Thes are fake data", ylim =c(0,30))
hist(y.fake, xlab = "Fake Y data", col = "red", main = "These are also fake data", ylim =c(0,30))

# QQPLPOTS
# qnorm plots the scatterplots, comparing the theoretical to the observed
# qqline plots the theoretical line, with lwd setting it s width from scale of thin = 1...thick = ..largenumber
par(mfrow = c(1, 1), mar = c(3, 3, 2, 2), mgp = c(2, 0.5,0), las = 1)
# pch specifies the type of symbol to use for the points using an indexed list 
plot(x.fake, 
       y.fake, 
       xlab = "Fake X data", 
       ylab = "Fake Y data",
       pch = 16,
       col = "orange",
       main = "Fake data")
# Add a triangular point using points at the specified x,y coordinates
points(x = 6, y = 80, col ="cyan", pch = 17)
# Add a legend at the spefied region
legend(x = "topright", legend = c("Fake Data", "AFakePoint"), pch = c(16, 17), col = c("orange", "cyan"))
getwd()
#--------------------------------------------------------------------------------------------------
###################################################################################################
###################################################################################################
###################################################################################################
#---------------------------------------------END--------------------------------------------------

