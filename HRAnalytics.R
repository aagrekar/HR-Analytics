install.packages('gsheet')
library(gsheet)
origData <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/19-Zv4KiYXw20Dmtj97BfcE6Cri4paA2lnALa6H3w7pc/edit#gid=205206323'))
employeeID <- c(1:dim(origData)[1])
HRData<- as.data.frame(cbind(employeeID, origData))
colnames(HRData)[1] <- c('employee_ID')
names(HRData)[names(HRData) == "sales"] <- "department"
# LD: change salary from character to ordinal
HRData$salary <- ordered(HRData$salary, levels=c("low", "medium", "high"))
# AA: changed the remaining variables to their correct form & checked if no column is missed.
HRData$sales <- as.factor(HRData$sales)
HRData$Work_accident <- as.factor(HRData$Work_accident)
HRData$left <- as.factor(HRData$left)
HRData$promotion_last_5years <- as.factor(HRData$promotion_last_5years)

factVars <- colnames(HRData)[sapply(HRData, is.factor)] 
numVars  <- colnames(HRData)[sapply(HRData, is.numeric)]
stopifnot(length(numVars) + length(factVars) == length(colnames(HRData)))
