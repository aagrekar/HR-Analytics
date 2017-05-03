#install.packages('gsheet')
#install.packages('xgboost')
#install.packages('caret')
#install.packages('ggplot2')
#install.packages('igraph')
#install.packages('corrplot')
#install.packages('e1071')
#install.packages('randomForest')
#install.packages('neuralnet')
library(gsheet)
library(xgboost)
library(caret)
library(ggplot2)
library(igraph)
library(corrplot)
library(e1071)
library(randomForest)
library(neuralnet)

# AA: Get data and format.
origData <- as.data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/19-Zv4KiYXw20Dmtj97BfcE6Cri4paA2lnALa6H3w7pc/edit#gid=205206323'))
employeeID <- c(1:dim(origData)[1])                        # Add employee Id column
HRData <- as.data.frame(cbind(employeeID, origData))       # Combine employee Id column with origData and convert to dataframe
colnames(HRData)[1] <- c('employee_ID')                    # Name the first column
names(HRData)[names(HRData) == "sales"] <- "department"    # Change the sales column name to department

# LD: change salary from character to ordinal
HRData$salary <- ordered(HRData$salary, levels=c("low", "medium", "high"))

# AA: changed the remaining variables to their correct form & checked if no column is missed.
HRData$department            <- as.factor(HRData$department)
HRData$Work_accident         <- as.factor(HRData$Work_accident)
HRData$left                  <- as.factor(HRData$left)
HRData$promotion_last_5years <- as.factor(HRData$promotion_last_5years)

factVars       <- colnames(HRData)[sapply(HRData, is.factor)] 
numVars        <- colnames(HRData)[sapply(HRData, is.numeric)]
numericColumns <- HRData[,sapply(HRData,is.numeric)]
numericColumns <- numericColumns[,-1]

stopifnot(length(numVars) + length(factVars) == length(colnames(HRData)))

# SG: Look for NA, 0, 999 values
summary(HRData)
Null_Counter <- apply(HRData[10:11], 2, function(x) length(which(x == ""    | 
                                                                 is.na(x)   | 
                                                                 x == "NA"  | 
                                                                 x == "999" | 
                                                                 x == '0'))/length(x))   # Calculate fraction null values in salary and department
Null_Counter

Null_Counter <- apply(HRData[2:9], 2, function(x) length(which(x == ""  | 
                                                               is.na(x) | 
                                                               x == "NA"))/length(x))    # Calculate fraction null values in remaining col
Null_Counter

# GN: Partition into test and training data set
set.seed(3048371)

inTrain <- createDataPartition(y = HRData$left, p = 0.8, list = FALSE)
allTrain <- HRData[inTrain,]
allTest <- HRData[-inTrain,]
numTrain <- numericColumns[inTrain,]
numTest <- numericColumns[-inTrain,]
stopifnot(nrow(allTrain) + nrow(allTest) == nrow(HRData))

# Create a data matrix on all the numeric values
numTrainMatrix <- data.matrix(numTrain)
numTrainLabel <- as.numeric(allTrain$left) - 1
numTestMatrix <- data.matrix(numTest)
numTestLabel <- as.numeric(allTest$left) - 1

# GN: Created a new directory called 'hr_graphs' to store graphs 
mainDirectory <- getwd()
graphDirectory <- paste(getwd(),"/hr_graphs",sep = "")

if (!dir.exists(graphDirectory)) {
  dir.create(graphDirectory)
} 

summary(HRData)
out <- capture.output(summary(HRData))
cat("summary", out, file="summary.txt", sep="n", append=TRUE)

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

#SM: Barplot of last evaluation Vs Employee leaving
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

# Barplot of time spend vs Employee leaving
pdf("left_time_spend.pdf")
ggplot(HRData,aes(x = HRData$time_spend_company, y = (..count..), fill = left)) + 
  geom_bar(position = "dodge",width = .5) + 
  scale_fill_discrete("Left Company", labels = c("Stayed","Left")) +
  scale_x_continuous("Time spend in the company") +
  scale_y_continuous("No. of Employees") +
  ggtitle("Time spend v. Employee Left Company")
dev.off()

# Segmenting the satisfaciton level data
highPerfEmployees <- HRData[HRData$left == 1 & HRData$satisfaction_level <= 0.12,]
percHighPerf <- dim(highPerfEmployees)[1]/dim(HRData[HRData$left == 1,])[1]

lowPerfEmployees <- HRData[HRData$left == 1 & HRData$satisfaction_level <= 0.46 & HRData$satisfaction_level >= .37,]
percLowPerf <- dim(lowPerfEmployees)[1]/dim(HRData[HRData$left == 1,])[1]

#SM: Boxplot for numeric variables
for(name in colnames(numericColumns)){
  pdf(paste(name, '_boxplot.pdf'))
  boxplot(numericColumns[[name]], main= name, horizontal = TRUE)
  dev.off()
}

setwd(mainDirectory)

#SM: correlation matrix
HRDataEDA<- HRData                            #for EDA purpose to get a better idea
str(HRDataEDA)
HRDataEDA$department <- as.numeric(HRDataEDA$department)
HRDataEDA$salary <- as.numeric(1:3)[match(HRDataEDA$salary, c('low', 'medium', 'high'))]
HRDataEDA$Work_accident <- as.numeric(HRDataEDA$Work_accident)
HRDataEDA$promotion_last_5years<-as.numeric(HRDataEDA$promotion_last_5years)

png("corr.png")
corMat <- cor(HRDataEDA[-8])
corrplot(corMat, method = "shade", use = "complete.obs")

# LD:
##############################################################################################################################
#
#   The following section demonstrates the Logistic Regression.
#
##############################################################################################################################
targetVar <- c("left")
xVars <- c(numVars[-1],factVars[-2])

# Function to generate the model formula
createModelFormula <- function(targetVar, xVars, includeIntercept = TRUE){
  if(length(targetVar) == 1){
    if(includeIntercept){
      modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ ')))
    } else {
      modelForm <- as.formula(paste(targetVar, "~", paste(xVars, collapse = '+ '), -1))
    }
  } else {
    if(includeIntercept){
      modelForm <- as.formula(paste(paste(targetVar, collapse = "+"), "~", paste(xVars, collapse = '+ ')))
    } else {
      modelForm <- as.formula(paste(paste(targetVar, collapse = "+"), "~", paste(xVars, collapse = '+ '), -1))
    }
  }
  
  return(modelForm)
}
modelForm <- createModelFormula(targetVar, xVars)
logModel <- glm(modelForm,family=binomial(link='logit'),data=allTrain)
log.probs <- predict(logModel, newdata = allTest, type='response')
log.preds <- ifelse(log.probs >= 0.5, 1, 0)
glmconfMat <- confusionMatrix(data = log.preds, reference = allTest$left)
                      
# SM: probability of employee leaving
allTest[head(order(log.probs,decreasing = TRUE)),]
log.probs[head(order(log.probs,decreasing = TRUE))]

# GN:
##############################################################################################################################
#
#   The following section demonstrates the XGBoost Method.
#
##############################################################################################################################

#  this is a normal model with no tuning.
xgInit <- xgboost(data = numTrainMatrix, label = numTrainLabel,
                  nrounds = 10,
                  object ="binary:logistic")
xgb.probs <- predict(xgInit, numTestMatrix)
xgb.preds <- ifelse(xgb.probs >= 0.5, 1, 0)
xgb.init.confusion <- confusionMatrix(data = xgb.preds, reference = allTest$left, dnn = c('Predicted Default', 'Actual Default'))

# Satisfaction level has the highest gain
initImp <- xgb.importance(feature_names = colnames(numTrainMatrix), model = xgInit)
xgb.plot.importance(initImp)

# Tuned version of xgboost

allTrainCopy <- allTrain                                                   # A copy with different labels for 'left' to make tuning work
levels(allTrainCopy$left) <- make.names(c('Stayed', 'Left'))

xg.ctrl <- trainControl(method = "repeatedcv", repeats = 1, number = 3,    # Training control parameters
                        classProbs = TRUE, 
                        allowParallel = T)

xgb.grid <- expand.grid(nrounds = 1000,                                    # Grid of values to try
                        eta = c(0.01,0.05,0.1),
                        max_depth = c(2,4,6,8,10,14),
                        gamma = 1, 
                        colsample_bytree = 1,
                        min_child_weight = 1,
                        subsample = 1
)
xgb.formula <- createModelFormula('left', numVars[-1])

# This takes a while!
xgb.tune <- train(xgb.formula,    
                  data = allTrainCopy,
                  method = "xgbTree",
                  trControl=xg.ctrl,
                  tuneGrid=xgb.grid,
                  verbose=T,
                  metric="Kappa",
                  nthread =3)

xgb.tuned.preds <- predict(xgb.tune, numTestMatrix)
xgb.tuned.numpreds <- ifelse(xgb.tuned.preds == 'Stayed', 0, 1)

xgbconfMat <- confusionMatrix(data = xgb.tuned.numpreds, reference = allTest$left, dnn = c('Predicted Default', 'Actual Default'))

xgb.tuned.importance <- xgb.importance(feature_names = colnames(numTrainMatrix), model = xgb.tune$finalModel)
xgb.plot.importance(xgb.tuned.importance)

#SG:                       
##############################################################################################################################
#
#   The following section demonstrates the Naive Baye's Method.
#
##############################################################################################################################

NBModel <- naiveBayes(modelForm, data = allTrain)
nb.probs <- predict(NBModel, newdata = allTest, type = 'raw')
nb.pred <- (nb.probs[,'0'] <= nb.probs[,'1'])*1

# Measure performance
nbconfMat <- confusionMatrix(data = nb.pred, reference = unlist(allTest$left), dnn = c('Predicted Default', 'Actual Default'))

## SM:probability of employee leaving
nb.probs<-as.data.frame(nb.probs)
allTest[head(order(nb.probs$`1`,decreasing = TRUE)),]
nb.probs[head(order(nb.probs$`1`,decreasing = TRUE)),2]
                      
# SM:
##############################################################################################################################
#
#   The following section demonstrates the Random Forest Method.
#
##############################################################################################################################

#for random Forest employee ID is not needed
RFModel <- randomForest(left~.,data=allTrain[-1],importance=TRUE,ntree=150,na.action=na.roughfix)
varImpPlot(RFModel)
plot(RFModel,log="y")

#performance on the Test set
rf.preds <- predict(RFModel, allTest[-1], type = "response")
rfconfMat <- confusionMatrix(data = rf.preds, reference = allTest$left, dnn = c('Predicted Default', 'Actual Default'))

#probability of employee leaving
rf.probs <- predict(RFModel, allTest, type = "prob")
rf.probs<-as.data.frame(rf.probs)
allTest[head(order(rf.probs$`1`,decreasing = TRUE)),]
rf.probs[head(order(rf.probs$`1`,decreasing = TRUE)),2]
                      
#AA : 
##############################################################################################################################
#
#   The following section demonstrates the Neural Networks Method.
#
##############################################################################################################################

mmFormula <- as.formula(~ left + satisfaction_level + last_evaluation + number_project + 
                          average_montly_hours + time_spend_company + Work_accident + 
                          promotion_last_5years + department + salary)        # formula to be used to convert the data frame into model 
                                                                              # matrix with one column for each level of the categorical variable
nnTrain <- model.matrix(mmFormula , allTrain)             # Training model matrix
nnTest  <- model.matrix(mmFormula, allTest)               # Test model matrix

nnTrain <- cbind(nnTrain, allTrain$left == 1)             # creating separate column for employees who have left
nnTrain <- cbind(nnTrain, allTrain$left == 0)             # creating separate column for employees who have not left.
colnames(nnTrain)[21:22] <- c("left", "notleft")

colnames(nnTrain)[2] <- c("left")
colnames(nnTest)[2]  <- c("left")

nnTagetVar <- colnames(nnTrain)[21:22]
nnXVars    <- colnames(nnTrain)[-c(1,2,21,22)]

nnFormula <- createModelFormula(nnTagetVar,nnXVars)

wts <- matrix(1, 19, 6)

# Setting up a model with 6 nodes in one hidden layer using the thumb rule of 2/3 rd nodes of the total input and output nodes.
nnModel <- neuralnet(nnFormula, nnTrain, hidden = 6, 
                     learningrate = 0.01, err.fct = "sse", act.fct = "logistic", rep = 1,
                     threshold = 0.5, startweights = wts, stepmax = 1e6)

nnProbs <- compute(nnModel, covariate = nnTest[,3:dim(nnTest)[2]])$net.result

idx <- apply(nnProbs, 1, which.max)
nnPred <- c('1', '0')[idx]
nnconfMat <- confusionMatrix(data = nnPred, reference = allTest$left, dnn = c('Predicted Default', 'Actual Default'))

# comparision graph between model accuracy, recall and precision
perfPara <- NULL
perfPara$Accuracy <- glmconfMat$overall['Accuracy']
perfPara$Accuracy <- rbind(perfPara$Accuracy,nbconfMat$overall['Accuracy'])
perfPara$Accuracy <- rbind(perfPara$Accuracy,nnconfMat$overall['Accuracy'])
perfPara$Accuracy <- rbind(perfPara$Accuracy,xgbconfMat$overall['Accuracy'])
perfPara$Accuracy <- rbind(perfPara$Accuracy,rfconfMat$overall['Accuracy'])


perfPara$Precision <- glmconfMat$byClass['Pos Pred Value']
perfPara$Precision <- rbind(perfPara$Precision,nbconfMat$byClass['Pos Pred Value'])
perfPara$Precision <- rbind(perfPara$Precision,nnconfMat$byClass['Pos Pred Value'])
perfPara$Precision <- rbind(perfPara$Precision,xgbconfMat$byClass['Pos Pred Value'])
perfPara$Precision <- rbind(perfPara$Precision,rfconfMat$byClass['Pos Pred Value'])

perfPara$Recall <- glmconfMat$byClass['Sensitivity']
perfPara$Recall <- rbind(perfPara$Recall,nbconfMat$byClass['Sensitivity'])
perfPara$Recall <- rbind(perfPara$Recall,nnconfMat$byClass['Sensitivity'])
perfPara$Recall <- rbind(perfPara$Recall,xgbconfMat$byClass['Sensitivity'])
perfPara$Recall <- rbind(perfPara$Recall,rfconfMat$byClass['Sensitivity'])

perfPara <- as.data.frame(perfPara)
names(perfPara) <- c("Accuracy","Precision","Recall")

plot(x=1:5, y=perfPara$Accuracy, main = "Model comparison", type = "b", col = "Red", xaxt = 'n', ylab = "Performance", xlab = "Models")
axis(1, at = 1:5, labels = c("GLM","NB","NN","XGB","RF"))
lines(x = 1:5, y = perfPara$Precision, type = "b", col = "Green")
lines(x=1:5, y = perfPara$Recall, type = "b", col = "Blue")
legend("bottomright", legend = c("Accuracy", "Precision", "Recall"), col=c("Red","Blue","Green"), lty=1)

# Plotting Variable Importance of Random Forest (The best model)
featImp <- NULL
idx <- order(varImp(RFModel)[,"1"], decreasing = FALSE)
featImp$Importance <- varImp(RFModel)[idx,"1"]
featImp$varnames <- row.names(varImp(RFModel))[idx]
featImp <- as.data.frame(featImp)


barplot(featImp$Importance[5:9], horiz = TRUE, names.arg = featImp$varnames[5:9], 
        main = "Feature Importance", xlab = "Importance by Gain")
