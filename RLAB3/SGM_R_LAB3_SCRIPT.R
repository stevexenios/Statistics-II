####################################################################################################
# STEVE G MWANGI
# R LAB 3
# 01/26/2020
###################################################################################################
setwd(choose.dir())
foot.df = read.csv(file.choose())
###################################################################################################
# Verifying correct data
# Viewing the first 6 lines of the data frame
head(foot.df)
dim(foot.df)
summary(foot.df)
###################################################################################################
#C2 Creating a linear model of Height ~ Shoe Print from the data in the data frame
foot.lm = lm(Height~Shoe.Print, data = foot.df)
summary(foot.lm)
###################################################################################################
#C3 Find the following values: 
# First, getting the means
y.bar = mean(foot.df$Height)
y.bar
x.bar = mean(foot.df$Shoe.Print)
x.bar
?plot
# Getting SST = SUM OF SQUARED TOTAL DEVIATIONS (SUM(Y_obs-Y_mean)^2)
foot.sst = sum((foot.df$Height-y.bar)^2)
foot.sst
# For SSE and SSR, we need to fit the model and calculate the fitted values
y.hat = foot.lm$fitted.values
# y.hat
# SSR
foot.ssr = sum((y.hat-y.bar)^2)
foot.ssr # [1] 2616.28
# SSE
foot.sse = sum((foot.df$Height-y.hat)^2)
foot.sse # [1] 1342.475
# SST = SSR + SSE
foot.sst
foot.ssr+foot.sse # [1] 3958.755
###################################################################################################
# C4. (2) Use your values for the partitioned SS, verify the value of R^2
# Multiple R-squared:  0.6609,	Adjusted R-squared:  0.652 
1-foot.sse/foot.sst
foot.ssr/foot.sst
cor.sqrd = (cor(foot.df$Height, foot.df$Shoe.Print))^2
cor.sqrd
###################################################################################################
# C5. Verfying that sum squared of total deviations is equivalent to the sum of the:
# sum of squared errors and sum of the squared regressions 
round(foot.sst, 3) == round(foot.ssr+foot.sse, 3)
###################################################################################################
# C6. (2) Use R to calculate prediction and confidence intervals for estimates from your lm.
# Create a newdata frame with same column name as name of column of values we are to predict
# sorted
newdata = data.frame(Shoe.Print = sort(foot.df$Shoe.Print))
?predict
# Using the predict function, that takes an object=>foot.lm, the result of a model fitting function 
# and interval specifying that we want a prediction, based of newdata.
# The newdata specifies the first place to look for explanatory variables to be used for prediction.
foot.pred = predict(foot.lm, interval = "prediction", newdata = newdata)
# Predicted Values
head(foot.pred)
foot.ci = predict(foot.lm, interval = "confidence", newdata = newdata)
# CI Values
head(foot.ci)
###################################################################################################
# C7
par(mfrow=c(1,1), mar=c(3.5,3.5,0.5,0.5),mgp=c(2.25,0.5,0),las=1)
# Scatterplot
plot(foot.df$Shoe.Print, col = "red" , foot.df$Height, xlab="Shoe print length (cm)",
     ylab="Height (cm)", pch=16)
# Adding fitted line
abline(foot.lm$coefficients, lwd=2)
###################################################################################################
# C9 Adding lines for the CI for the mean value of Y
# at a given value of X, and for the prediction of a 
# NEW value of Y at a given value of X.
# Code to add a line whose coordinates are the x-values
# in newdata, and the corresponding:
# LOWER prediction limits
lines(newdata$Shoe.Print, foot.pred[,2], col="green", lwd=2)
# UPPER prediction limits
lines(newdata$Shoe.Print, foot.pred[,3], col="green", lwd=2)
# LOWER CI limits
lines(newdata$Shoe.Print, foot.ci[,2], col="blue", lwd=2)
# UPPER CI limits
lines(newdata$Shoe.Print, foot.ci[,3], col="blue", lwd=2)
legend(x="topleft", legend=c("FittedLine", "95% CI", "95% PI"), col=c("black", "green", "blue"), lty = 1)
###################################################################################################
# C11
# Using R to make Prediction values based on the above model
x.pred<-c(15,22.1,35.9,28.2,25.7)
newdata2 = data.frame(Shoe.Print = sort(x.pred))
xx.pred = predict(foot.lm, interval = "prediction", newdata = newdata2)
# Predicted Values
head(xx.pred)
write.table(xx.pred, file = file.choose(), sep = "\t",
            row.names = TRUE, col.names = NA)
###################################################################################################
# C12
summary(foot.df$Shoe.Print)
###################################################################################################
# C13
summary(foot.pred)
###################################################################################################
