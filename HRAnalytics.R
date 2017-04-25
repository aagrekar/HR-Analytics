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

# SG: List factors and numeric variables
factVars <- colnames(HRData)[sapply(HRData, is.factor)] 
numVars  <- colnames(HRData)[sapply(HRData, is.numeric)]

# SG: Check for missing variables
stopifnot(length(numVars) + length(factVars) == length(colnames(HRData)))

# SG: Look for NA, 0, 999 values
summary(HRData)
# Calculate fraction null values in salary and department
Null_Counter <- apply(HRData[10:11], 2, function(x) length(which(x == "" | is.na(x) | x == "NA" | x == "999" | x == '0'))/length(x))
Null_Counter

# SG: Calculate fraction null values in remaining col
Null_Counter <- apply(HRData[2:9], 2, function(x) length(which(x == "" | is.na(x) | x == "NA"))/length(x))
Null_Counter

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

# Barplot of last evaluation Vs Employee leaving
pdf("left_evaluation_bar.pdf")
ggplot(HRData,aes(x = HRData$last_evaluation, y = (..count..), fill = left)) + 
  geom_bar(stat = "count") + 
  scale_fill_discrete("Left Company", labels = c("Stayed","Left")) +
  scale_x_continuous("Last evaluation") +
  scale_y_continuous("No. of Employees") +
  ggtitle("Last Evaluation v. Employee Left Company")
dev.off()

# Barplot of average monthly hours Vs Employee leaving
pdf("left_avg_monthly_hours_bar.pdf")
ggplot(HRData,aes(x = HRData$average_montly_hours, y = (..count..), fill = left)) + 
  geom_bar(stat = "count") + 
  scale_fill_discrete("Left Company", labels = c("Stayed","Left")) +
  scale_x_continuous("Average Monthly hours") +
  scale_y_continuous("No. of Employees") +
  ggtitle("Last Evaluation v. Employee Left Company")
dev.off()

# Barplot of number of project Vs Employee leaving
pdf("left_no.of_project_bar.pdf")
ggplot(HRData,aes(x = HRData$number_project, y = (..count..), fill = left)) + 
  geom_bar(position = "dodge",width = .5) + 
  scale_fill_discrete("Left Company", labels = c("Stayed","Left")) +
  scale_x_continuous("No. of Project") +
  scale_y_continuous("No. of Employees") +
  ggtitle("No. of Project v. Employee Left Company")
dev.off()


# Boxplot for numeric variables
numericColumns <- HRData[,sapply(HRData,is.numeric)]
numCols <- numericColumns[,-1]
for(name in colnames(numCols)){
  pdf(paste(name, '_boxplot.pdf'))
  boxplot(numCols[[name]], main= name, horizontal = TRUE)
  dev.off()
}

setwd(mainDirectory)

#correlation matrix
library(corrplot)
CorMat <- cor(HRData)
corrplot(CorMat, method = "pie")



###### XG BOOST ######

# GN: I did a quick sampling just to get the model in place. The data frames can be updated as needed
# when we sort out how we will be handling training and test data.

install.packages('xgboost')
install.packages('caret')
library(xgboost)
library(caret)

set.seed(1234)

inTrain <- createDataPartition(y = HRData$left, p = 0.8, list = FALSE)
AllTrain <- HRData[inTrain,]
AllTest <- HRData[-inTrain,]
NumTrain <- numCols[inTrain,]
NumTest <- numCols[-inTrain,]
stopifnot(nrow(AllTrain) + nrow(AllTest) == nrow(HRData))

# Create a data matrix on all the numeric values
NumTrainMatrix <- data.matrix(NumTrain)
NumTrainLabel <- as.numeric(AllTrain$left) - 1
NumTestMatrix <- data.matrix(NumTest)
NumTestLabel <- as.numeric(AllTest$left) - 1

# This loop will continue generating xgboost models until
# the accuracy on the test predictions has converged.
#
# The outer loop iterates through maximum depths and the
# inner loop iterates through rounds. 

# GN: Need to tune max depth

NumTestEval <- data.frame(NumTestLabel)
NumTestAccuracy <- c(0)
i = 2
converged <- FALSE
while (converged == FALSE) {
  # Run model
  xgtemp <- xgboost(data = NumTrainMatrix, label = NumTrainLabel, 
                    max_depth = 8, eta = 0.3, nthread = 2, 
                    nrounds = i, objective = "binary:logistic", verbose = 1)
  # Generate predictions
  predtemp <- predict(xgtemp, NumTestMatrix)
  # Add those predictions to a new column in eval
  predtitle <- paste('prediction', i,sep="")
  acctitle <- paste('correct', i, sep="")
  # Compute the accuracy
  NumTestEval[,predtitle] <- predtemp
  NumTestEval[,predtitle][NumTestEval[,predtitle] > cutoff] <- 1
  NumTestEval[,predtitle][NumTestEval[,predtitle] <= cutoff] <- 0
  NumTestEval[,acctitle] <- NumTestEval[,predtitle] - NumTestEval$NumTestLabel
  NumTestAccuracy[i] <- nrow(NumTestEval[NumTestEval[,acctitle] == 0,])/nrow(NumTestEval)
  # If the accuracy hasn't converged, keep going
  converged <- NumTestAccuracy[i] == NumTestAccuracy[i-1]
  i = i+1
}

# This matrix shows how long it took to predict certain observations correctly
NumTestCorrect <- NumTestEval[seq(3,ncol(NumTestEval),2)]
NumTestCorrect <- NumTestCorrect[NumTestCorrect$correct2 != 0,]
NumTestCorrect[,"TotalWrong"] <- -1*rowSums(NumTestCorrect)

setwd(graphDirectory)
pdf("xg_accuracy_plot.pdf")
NumTestAccuracy <- data.frame(NumTestAccuracy)
# Plots the accuracy as iterations of xgboost increases
ggplot(data = NumTestAccuracy, aes(x = seq_along(NumTestAccuracy), y = NumTestAccuracy)) + 
  scale_y_continuous("Accuracy", limits = c(0.9, 1)) +
  labs(x = "No. of Iterations") +
  ggtitle("Accuracy of XGBoost After n Iterations") +
  geom_point(size = 2, colour = "darkblue")
dev.off()
setwd(mainDirectory)

xgFinalTestAccuracy <- tail(NumTestAccuracy, n = 1)
print(paste("XGBoost Test Accuracy: ",xgFinalTestAccuracy," --  Xgboost converged in ",i," steps.", sep=""))

# SG: Naive Baye's
library(e1071)
NB <- naiveBayes(left~., data = AllTrain[-1])

# the summary tells us that the model provides a-priori probabilities of no-recurrence and recurrence
# events as well as conditional probability tables across all attributes.
print(NB)
summary(NB)

probs <- predict(NB, newdata = AllTest[-1], type = 'raw')
default.pred <- (probs[,'0'] <= probs[,'1'])*1

## Measure performance
confusionMatrix(data = default.pred, reference = unlist(AllTest[,'left']), dnn = c('Predicted Default', 'Actual Default'))

table(default.pred)
