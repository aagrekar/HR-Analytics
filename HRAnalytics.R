#install.packages('xgboost')
#install.packages('caret')
#install.packages('gsheet')
#install.packages('igraph')
library(gsheet)
library(xgboost)
library(caret)
library(ggplot2)
library(igraph)
library(randomForest)
library(e1071)
library(corrplot)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

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
numericColumns <- HRData[,sapply(HRData,is.numeric)]
numericColumns <- numericColumns[,-1]

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

factVars <- colnames(HRData)[sapply(HRData, is.factor)] 
numVars  <- colnames(HRData)[sapply(HRData, is.numeric)]
stopifnot(length(numVars) + length(factVars) == length(colnames(HRData)))

# Partition into test and training data set
set.seed(3048371)

inTrain <- createDataPartition(y = HRData$left, p = 0.8, list = FALSE)
AllTrain <- HRData[inTrain,]
AllTest <- HRData[-inTrain,]
NumTrain <- numericColumns[inTrain,]
NumTest <- numericColumns[-inTrain,]
stopifnot(nrow(AllTrain) + nrow(AllTest) == nrow(HRData))

# Create a data matrix on all the numeric values
NumTrainMatrix <- data.matrix(NumTrain)
NumTrainLabel <- as.numeric(AllTrain$left) - 1
NumTestMatrix <- data.matrix(NumTest)
NumTestLabel <- as.numeric(AllTest$left) - 1

# GN: Created a new directory called 'hr_graphs' to store graphs 
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

### ADD TO EDA SLIDE
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

## ADD TO EDA SLIDE
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

ggplot(HRData,aes(x = HRData$time_spend_company, y = (..count..), fill = left)) + 
  geom_bar(position = "dodge",width = .5) + 
  scale_fill_discrete("Left Company", labels = c("Stayed","Left")) +
  scale_x_continuous("Years With Company") +
  scale_y_continuous("No. of Employees") +
  ggtitle("Years Spent With Company v. Employee Left Company")

# Conditional probability plots


# Boxplot for numeric variables
for(name in colnames(numericColumns)){
  pdf(paste(name, '_boxplot.pdf'))
  boxplot(numericColumns[[name]], main= name, horizontal = TRUE)
  dev.off()
}

setwd(mainDirectory)

#correlation matrix
HRDataEDA<- HRData #for EDA purpose to get a better idea
str(HRDataEDA)
HRDataEDA$department <- as.numeric(HRDataEDA$department)
HRDataEDA$salary <- as.numeric(1:3)[match(HRDataEDA$salary, c('low', 'medium', 'high'))]
HRDataEDA$Work_accident <- as.numeric(HRDataEDA$Work_accident)
HRDataEDA$promotion_last_5years<-as.numeric(HRDataEDA$promotion_last_5years)

CorMat <- cor(HRDataEDA[-8])
corrplot(CorMat, method = "shade", use = "complete.obs")

# Linear model
targetVar <- HRData$left
xVars <- c(numVars[-1],factVars[-2])
createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(includeIntercept){
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
  } else {
    modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
  }
  return(modelForm)
}
logForm <- createModelFormula("left", names(AllTrain), includeIntercept = TRUE)
logModel <- glm(logForm,family=binomial(link='logit'),data=AllTrain)
log.preds <- predict(logModel, newdata = AllTest, type='response')
log.preds <- ifelse(log.preds >= 0.5, 1, 0)
confusionMatrix(data = log.preds, reference = AllTest$left)

###### XG BOOST ######

# GN: I did a quick sampling just to get the model in place. The data frames can be updated as needed
# when we sort out how we will be handling training and test data.

# This loop will continue generating xgboost models until
# the accuracy on the test predictions has converged.
#
# The outer loop iterates through maximum depths and the
# inner loop iterates through rounds. 

# GN: Need to tune max depth

xgInit <- xgboost(data = NumTrainMatrix, label = NumTrainLabel,
                  nrounds = 10,
                  object ="binary:logistic")
xgb.predictions <- predict(xgInit, NumTestMatrix)
xgb.predictions <- ifelse(xgb.predictions >= 0.5, 1, 0)
confusionMatrix(data = xgb.predictions, reference = AllTest$left,
                dnn = c('Predicted Default', 'Actual Default'))

## Evaluation of XGBoost
# Satisfaction level has the highest gain
initImp <- xgb.importance(feature_names = colnames(NumTrainMatrix), model = xgInit)
xgb.plot.importance(importance)


## Report conditional probabilities!
# SG: Naive Baye's
NB <- naiveBayes(left~., data = AllTrain)
probs <- predict(NB, newdata = AllTrain, type = 'raw')
default.pred <- (probs[,'0'] <= probs[,'1'])*1


## Measure performance
confusionMatrix(data = default.pred, reference = unlist(AllTrain$left)
                , dnn = c('Predicted Default', 'Actual Default'))

table(default.pred)

# Random Forest
#for random Forest employee ID is not needed
fitRF <- randomForest(left~.,data=AllTrain[-1],importance=TRUE,ntree=150,na.action=na.roughfix)
fitRF
varImpPlot(fitRF)
plot(fitRF,log="y")


# performance on the Test set
PredictRF <- predict(fitRF, AllTest, type = "response")
conf<- table(PredictRF,AllTest$left)
confusionMatrix(conf)

#AA : Neural Networks

mmFormula <- as.formula(~ left + satisfaction_level + last_evaluation + number_project + 
                          average_montly_hours + time_spend_company + Work_accident + 
                          promotion_last_5years + department + salary)
nnTrain <- model.matrix(mmFormula , AllTrain)
nnTest  <- model.matrix(mmFormula, AllTest)

colnames(nnTrain)[2] <- c("left")
colnames(nnTest)[2]  <- c("left")

nnTagetVar <- colnames(nnTrain)[2]
nnXVars    <- colnames(nnTrain)[-c(1,2)]

nnFormula <- createModelFormula(nnTagetVar, nnXVars)

nnModel <- neuralnet(nnFormula, nnTrain, hidden = 6, learningrate = 0.1, err.fct = "sse", act.fct = "logistic", rep = 1,
                  threshold = 0.5)

wts <- nnModel$weights[[1]][[1]]

nnProbs <- compute(nnModel, covariate = nnTest[,3:dim(nnTest)[2]])$net.result
nnpred <- ifelse(nnProbs > 0.5, 1, 0)
confusionMatrix(data = nnpred, reference = AllTest$left, dnn = c('Predicted Default', 'Actual Default'))                
