#Loading the required libraries
library('caret')
#Seeting the random seed
set.seed(1)




#Loading the hackathon dataset
data <- read.csv(url('https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv'))
str(data)


sum(is.na(data))
preProcValues <- preProcess(data, method = c("medianImpute","center","scale"))
library('RANN')
data_processed <- predict(preProcValues, data)

sum(is.na(data_processed))

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(data_processed$Loan_Status, p=0.75, list=FALSE)
trainSet <- data_processed[ index,]
testSet <- data_processed[-index,]


#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome",
              "CoapplicantIncome")
outcomeName<-'Loan_Status'



#Training the random forest model
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)
#Predicting using random forest model
testSet$pred_rf<-predict(object = model_rf,testSet[,predictors])

confusionMatrix(testSet$Loan_Status,testSet$pred_rf)

a <- confusionMatrix(testSet$Loan_Status,testSet$pred_rf)$overall['Accuracy']





#Training the knn model
model_knn <- train(trainSet[,predictors],trainSet[,outcomeName],method='knn',trControl=fitControl,tuneLength=3)

#Predicting using knn model
testSet$pred_knn <- predict(object = model_knn,testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$Loan_Status,testSet$pred_knn)


b <- confusionMatrix(testSet$Loan_Status,testSet$pred_knn)$overall['Accuracy']




#Logistic Regression
model_lr <- train(trainSet[,predictors], trainSet[,outcomeName], method='glm', trControl=fitControl, tuneLength=3)

#Predicting using knn model
testSet$pred_lr <- predict(object = model_lr, testSet[,predictors])

#Checking the accuracy of the random forest model
confusionMatrix(testSet$Loan_Status,testSet$pred_lr)

c <- confusionMatrix(testSet$Loan_Status,testSet$pred_lr)$overall['Accuracy']



#Predicting the probabilities
testSet$pred_rf_prob<-predict(object = model_rf,testSet[,predictors],type='prob')
testSet$pred_knn_prob<-predict(object = model_knn,testSet[,predictors],type='prob')
testSet$pred_lr_prob<-predict(object = model_lr,testSet[,predictors],type='prob')

#Taking average of predictions
testSet$pred_avg<-(testSet$pred_rf_prob$Y+testSet$pred_knn_prob$Y+testSet$pred_lr_prob$Y)/3

#Splitting into binary classes at 0.5
testSet$pred_avg<-as.factor(ifelse(testSet$pred_avg>0.5,'Y','N'))
confusionMatrix(testSet$Loan_Status,testSet$pred_avg)

d <- confusionMatrix(testSet$Loan_Status,testSet$pred_avg)$overall['Accuracy']




#The majority vote
testSet$pred_majority<-as.factor(ifelse(testSet$pred_rf=='Y' & testSet$pred_knn=='Y','Y',ifelse(testSet$pred_rf=='Y' & testSet$pred_lr=='Y','Y',ifelse(testSet$pred_knn=='Y' & testSet$pred_lr=='Y','Y','N'))))
confusionMatrix(testSet$Loan_Status,testSet$pred_majority)
e <- confusionMatrix(testSet$Loan_Status,testSet$pred_majority)$overall['Accuracy']


a
b
c
d
e

#Predicting the out of fold prediction probabilities for training data
trainSet$OOF_pred_rf<-model_rf$pred$Y[order(model_rf$pred$rowIndex)]
trainSet$OOF_pred_knn<-model_knn$pred$Y[order(model_knn$pred$rowIndex)]
trainSet$OOF_pred_lr<-model_lr$pred$Y[order(model_lr$pred$rowIndex)]

#Predicting probabilities for the test data
testSet$OOF_pred_rf<-predict(model_rf,testSet[predictors],type='prob')$Y
testSet$OOF_pred_knn<-predict(model_knn,testSet[predictors],type='prob')$Y
testSet$OOF_pred_lr<-predict(model_lr,testSet[predictors],type='prob')$Y
predictors_top<-c('OOF_pred_rf','OOF_pred_knn','OOF_pred_lr') 



#GBM as top layer model 
model_gbm<- 
  train(trainSet[,predictors_top],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)

#Logistic regression as top layer model
model_glm<-
  train(trainSet[,predictors_top],trainSet[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)

#predict using GBM top layer model
testSet$gbm_stacked<-predict(model_gbm,testSet[,predictors_top])

#predict using logictic regression top layer model
testSet$glm_stacked<-predict(model_glm,testSet[,predictors_top])

confusionMatrix(testSet$Loan_Status,testSet$glm_stacked)

f <- confusionMatrix(testSet$Loan_Status,testSet$gbm_stacked)$overall['Accuracy']
g <- confusionMatrix(testSet$Loan_Status,testSet$glm_stacked)$overall['Accuracy']

a
b
c
d
e
f
g



