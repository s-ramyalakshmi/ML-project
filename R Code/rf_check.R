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


#Training the c50
model_c50 <- train(trainSet[,predictors],trainSet[,outcomeName],method='C5.0',trControl=fitControl,tuneLength=3)

#Predicting using c50 model
testSet$pred_c50 <- predict(object = model_c50,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$class,testSet$pred_c50)


d <- confusionMatrix(testSet$class, testSet$pred_c50)$overall['Accuracy']


#Training the mlp
model_mlp <- train(trainSet[,predictors],trainSet[,outcomeName],method='mlp',trControl=fitControl,tuneLength=3)

#Predicting using mlp model
testSet$pred_mlp <- predict(object = model_mlp,testSet[,predictors])

#Checking the accuracy of the mlp
confusionMatrix(testSet$class,testSet$pred_mlp)


f <- confusionMatrix(testSet$class, testSet$pred_mlp)$overall['Accuracy']

#Training the mlp
model_lda <- train(trainSet[,predictors],trainSet[,outcomeName],method='mlpWeightDecay',trControl=fitControl,tuneLength=3)

#Predicting using mlp model
testSet$pred_lda <- predict(object = model_lda,testSet[,predictors])

#Checking the accuracy of the mlp
confusionMatrix(testSet$class,testSet$pred_lda)


g <- confusionMatrix(testSet$class, testSet$pred_lda)$overall['Accuracy']

a
b
c
d
f
g


trainSet$OOF_pred_rf<-model_rf$pred$R[order(model_rf$pred$rowIndex)]
trainSet$OOF_pred_knn<-model_knn$pred$R[order(model_knn$pred$rowIndex)]
trainSet$OOF_pred_naive<-model_naive$pred$R[order(model_naive$pred$rowIndex)]
trainSet$OOF_pred_c50<-model_c50$pred$R[order(model_c50$pred$rowIndex)]
trainSet$OOF_pred_mlp<-model_mlp$pred$R[order(model_mlp$pred$rowIndex)]
trainSet$OOF_pred_lda<-model_lda$pred$R[order(model_lda$pred$rowIndex)]



testSet$OOF_pred_rf<-predict(model_rf,testSet[predictors],type='prob')$R
testSet$OOF_pred_knn<-predict(model_knn,testSet[predictors],type='prob')$R
testSet$OOF_pred_naive<-predict(model_naive,testSet[predictors],type='prob')$R
testSet$OOF_pred_c50<-predict(model_c50,testSet[predictors],type='prob')$R
testSet$OOF_pred_mlp<-predict(model_mlp,testSet[predictors],type='prob')$R
testSet$OOF_pred_lda<-predict(model_lda,testSet[predictors],type='prob')$R



predictors_top<-c('OOF_pred_rf', 'OOF_pred_knn','OOF_pred_naive', 'OOF_pred_mlp') 

model_gbm<- 
  train(trainSet[,predictors_top],trainSet[,outcomeName],method="rf",trControl=fitControl,tuneLength=3)

testSet$gbm_stacked<-predict(model_gbm,testSet[,predictors_top])
testSet$gbm_stacked
testSet

confusionMatrix(testSet$class,testSet$gbm_stacked)$overall['Accuracy']

