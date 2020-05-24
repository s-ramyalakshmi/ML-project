#Loading the required libraries
library('caret')
#Seeting the random seed
set.seed(1)


#Loading the hackathon dataset
data <- read.csv('data.csv')
str(data)


#sum(is.na(data))
#preProcValues <- preProcess(data, method = c("medianImpute","center","scale"))
#library('RANN')

#sum(is.na(data_processed))
data_processed <- data

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(data_processed$class, p=0.8, list=FALSE)
trainSet <- data_processed[ index,]
testSet <- data_processed[-index,]


#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
predictors<-c("X1", "X2", "X3", "X4")
outcomeName<-"class"


#Logistic Regression
model_lr <- train(trainSet[,predictors], trainSet[,outcomeName], method='glm', trControl=fitControl, tuneLength=3)

#Predicting using knn model
testSet$pred_lr <- predict(object = model_lr, testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$class,testSet$pred_lr)
