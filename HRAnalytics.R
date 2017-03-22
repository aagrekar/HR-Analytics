install.packages('gsheet')
library(gsheet)
# Get data
origData <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/19-Zv4KiYXw20Dmtj97BfcE6Cri4paA2lnALa6H3w7pc/edit#gid=205206323'))

# Add employee Id column
employeeID <- c(1:dim(origData)[1])

# Combine employee Id column with origData and convert to dataframe
HRData <- as.data.frame(cbind(employeeID, origData))

# Name the first column
colnames(HRData)[1] <- c('employee_ID')

# Change the sales column name to department
names(HRData)[names(HRData) == "sales"] <- "department"

# LD: change salary from character to ordinal
HRData$salary <- ordered(HRData$salary, levels=c("low", "medium", "high"))

# AA: changed the remaining variables to their correct form & checked if no column is missed.
HRData$department <- as.factor(HRData$department)
HRData$Work_accident <- as.factor(HRData$Work_accident)
HRData$left <- as.factor(HRData$left)
HRData$promotion_last_5years <- as.factor(HRData$promotion_last_5years)

# List factors and numeric variables
factVars <- colnames(HRData)[sapply(HRData, is.factor)] 
numVars  <- colnames(HRData)[sapply(HRData, is.numeric)]

# Check for missing variables
stopifnot(length(numVars) + length(factVars) == length(colnames(HRData)))

# Look for NA, 0, 999 values
summary(HRData)
# Calculate fraction null values in salary and department
Null_Counter <- apply(HRData[10:11], 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == '0'))/length(x))
Null_Counter

#Calculate fraction null values in remaining col
Null_Counter <- apply(HRData[2:9], 2, function(x) length(which(x == "" | is.na(x) | x == "NA"))/length(x))
Null_Counter
