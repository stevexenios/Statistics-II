####################################################################################################
# STEVE G MWANGI
# R LAB 2
# 01/20/2020
###################################################################################################
setwd(choose.dir())
foot.df = read.csv(file.choose())
###################################################################################################
#C2
# Viewing the first 6 lines of the data frame
head(foot.df)
# Determine the number of observations in the data set
dim(foot.df)
###################################################################################################
#C4
# Scatter plot with Shoe print length on X axis and Height on Y axis
plot(foot.df$Shoe.Print,
     foot.df$Height, 
     xlab = "Shoe Print Length in cm", 
     ylab = "Height in cm", 
     main = "Scatter plot of Height vs. Shoe Print ",
     xlim = c(24, 36),
     ylim = c(150, 200),
     col = "black",
     pch = 16)
###################################################################################################
#C6
# Creating a linear model of Height ~ Shoe Print from the data in the data frame
lm(Height~Shoe.Print, data = foot.df)
model1 = lm(Height~Shoe.Print, data = foot.df)
# Getting  the summary of the model
summary(model1)
###################################################################################################

###################################################################################################

###################################################################################################
