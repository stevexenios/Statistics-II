####################################################################################################
# STEVE G MWANGI
# Computer Assignment 6
# R LAB 6
# 03/11/2020
###################################################################################################
setwd(choose.dir())
scenic.df<-read.csv(file.choose())
dim(scenic.df)

# Coding the MedSchAff column as a factor
scenic.df$MedSchAff = factor(scenic.df$MedSchAff)
###################################################################################################
# Creating a Summary Table of MedSchAff column and report the number of hospitals affiliated with
# a medical school and the number not affiliated.
# C2
summary(scenic.df$MedSchAff)

# C3
plot(scenic.df)

# C4
scenic.lm = lm(InfectionRisk~LenStay+Age+NumBeds+PatientsPerDay+NumNurse, data = scenic.df)
car::vif(scenic.lm)

# Dropping a column
scenic.df = scenic.df[, !names(scenic.df)%in% "NumBeds"]
# New Column with Patients/Nurse
scenic.df$PatientsPerNurse = scenic.df$PatientsPerDay/scenic.df$NumNurse

# C6
scenic.lm = lm(InfectionRisk~LenStay+Age+PatientsPerNurse, data = scenic.df)
car::vif(scenic.lm)

# Using Built-in Model Selection
scenic2.lm = lm(InfectionRisk~LenStay*MedSchAff+Age*MedSchAff+PatientsPerNurse*MedSchAff, data=scenic.df)

# AIC default
scenicStep.aic=step(scenic2.lm)
scenicStep.aic
summary(scenicStep.aic)

# Diagnostics
# Using R's built in diagnostic tools
par(mfrow=c(2,2), mar=c(3.5,3.5,1.5,0.5),mgp=c(2.5,0.5,0),las=1)
# Diagnostic plot
plot(scenicStep.aic)

###################################################################################################