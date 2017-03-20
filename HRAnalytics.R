#install.packages('gsheet')
library(gsheet)
origData <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/19-Zv4KiYXw20Dmtj97BfcE6Cri4paA2lnALa6H3w7pc/edit#gid=205206323'))
employeeID <- c(1:dim(origData)[1])
HRData<- as.data.frame(cbind(employeeID, origData))
colnames(HRData)[1] <- c('employee_ID')
# LD: change salary from character to ordinal
HRData$salary <- ordered(HRData$salary, levels=c("low", "medium", "high"))
# AA: changed the remaining variables to their correct form & checked if no column is missed.
HRData$sales <- as.factor(HRData$sales)
HRData$Work_accident <- as.factor(HRData$Work_accident)
HRData$left <- as.factor(HRData$left)
HRData$promotion_last_5years <- as.factor(HRData$promotion_last_5years)
# GN: Renamed the 'sales' column since it's the department name
colnames(HRData)[10] <- "department"

factVars <- colnames(HRData)[sapply(HRData, is.factor)] 
numVars  <- colnames(HRData)[sapply(HRData, is.numeric)]
stopifnot(length(numVars) + length(factVars) == length(colnames(HRData)))

# GN: Created a new directory called 'hr_graphs' to store graphs 
library(ggplot2)
mainDirectory <- getwd()
graphDirectory <- paste(getwd(),"/hr_graphs",sep = "")

if (!dir.exists(graphDirectory)) {
  dir.create(graphDirectory)
} 

setwd(graphDirectory)
# Centers plot titles
theme_update(plot.title = element_text(hjust = 0.5))

# Barplot of Promotion v. Left
pdf("left_promoted_barplot.pdf")
ggplot(HRData, aes(x = left, y = (..count..), fill = promotion_last_5years)) + 
       geom_bar(position = "dodge",width = .5) + 
       #geom_text(stat = 'count', aes(label = (..count..)),position = position_dodge(0.5)) +
       scale_fill_discrete("Promoted In Last 5 Years", labels = c("Not Promoted","Promoted")) +
       scale_x_discrete("Left Company", labels = c("Stayed", "Left")) +
       ylab("No. of Employee") + ggtitle("Employee Left Company   v.   Employee Promoted in Last 5 Years") 
dev.off()

# Scatterplot of Satisfaction v. Left
pdf("left_satisfaction_scatter.pdf")
ggplot(HRData, aes(x = left, y = satisfaction_level)) +
       scale_x_discrete("Left Company", labels = c("Stayed", "Left")) +
       scale_y_continuous("Satisfaction Level") +
       ggtitle("Employee Satisfcation v. Employee Left Company") +
       geom_jitter(alpha = 0.25, color = "darkblue")
dev.off()

# Barplot of Salary Level v. Left
pdf("left_salary_bar.pdf")
ggplot(HRData,aes(x = salary, y = (..count..), fill = left)) + 
       geom_bar(position = "dodge",width = .5) + 
       scale_fill_discrete("Left Company", labels = c("Stayed","Left")) +
       scale_x_discrete("Salary Level", labels = c("Low", "Medium", "High")) +
       scale_y_continuous("No. of Employees") +
       ggtitle("Employee Salary Level v. Employee Left Company")
dev.off()

# Barplot of Department v. Left
pdf("left_department_bar.pdf")
ggplot(HRData,aes(x = department, y = (..count..), fill = left)) + 
       geom_bar(position = "dodge",width = .5) + 
       scale_fill_discrete("Left Company", labels = c("Stayed","Left")) +
       scale_x_discrete("Department Name") +
       scale_y_continuous("No. of Employees") +
       ggtitle("Department v. Employee Left Company") + coord_flip()
dev.off()

setwd(mainDirectory)
