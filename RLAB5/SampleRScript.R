####################################################################################################
# STEVE G MWANGI
# MILESTONE 2
###################################################################################################
###################################################################################################
setwd(choose.dir())
f.df = read.csv(file.choose())
###################################################################################################
# Verifying correct data
# Viewing the first 6 lines of the data frame
head(f.df) # Checking the first 6 lines of the data frame
dim(f.df) # Function to verify that the data fram has 13 Columns and 440 Rows
summary(f.df)
