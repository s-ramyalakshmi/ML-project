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
outcomeName <- "class"


#Training the random forest model
model_rf <- train(trainSet[,predictors],trainSet[,outcomeName], method='rf', trControl=fitControl, tuneLength=3)
#Predicting using random forest model
testSet$pred_rf <- predict(object = model_rf, testSet[, predictors])

confusionMatrix(testSet$class, testSet$pred_rf)

a <- confusionMatrix(testSet$class, testSet$pred_rf)$overall['Accuracy']






#Training the knn model
model_knn <- train(trainSet[,predictors],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Predicting using knn model
testSet$pred_knn <- predict(object = model_knn,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$class,testSet$pred_knn)

b <- confusionMatrix(testSet$class, testSet$pred_knn)$overall['Accuracy']







#Training the naive model
model_naive <- train(trainSet[,predictors],trainSet[,outcomeName],method='naive_bayes',trControl=fitControl,tuneLength=3)

#Predicting using knn model
testSet$pred_naive <- predict(object = model_naive,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$class,testSet$pred_naive)

c <- confusionMatrix(testSet$class, testSet$pred_naive)$overall['Accuracy']

a
b
c


trainSet$OOF_pred_rf<-model_rf$pred$R[order(model_rf$pred$rowIndex)]
trainSet$OOF_pred_knn<-model_knn$pred$R[order(model_knn$pred$rowIndex)]
trainSet$OOF_pred_naive<-model_naive$pred$R[order(model_naive$pred$rowIndex)]

testSet$OOF_pred_rf<-predict(model_rf,testSet[predictors],type='prob')$R
testSet$OOF_pred_knn<-predict(model_knn,testSet[predictors],type='prob')$R
testSet$OOF_pred_naive<-predict(model_naive,testSet[predictors],type='prob')$R

predictors_top<-c('OOF_pred_rf','OOF_pred_knn','OOF_pred_naive') 

model_gbm<- 
  train(trainSet[,predictors_top],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

testSet$gbm_stacked<-predict(model_gbm,testSet[,predictors_top])
testSet$gbm_stacked
testSet$class

confusionMatrix(testSet$class,testSet$gbm_stacked)$overall['Accuracy']
