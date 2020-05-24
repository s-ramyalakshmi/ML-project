#Loading the required libraries
library('caret')
#Seeting the random seed
set.seed(1)


#Loading the hackathon dataset
data <- read.csv('dataset3.csv')
str(data)


#sum(is.na(data))
#preProcValues <- preProcess(data, method = c("medianImpute","center","scale"))
#library('RANN')
#data_processed <- data
#sum(is.na(data_processed))

#Spliting training set into two parts based on outcome: 75% and 25%
index <- createDataPartition(data_processed$rating, p=0.7, list=FALSE)
trainSet <- data_processed[ index,]
testSet <- data_processed[-index,]


#Defining the training controls for multiple models
fitControl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

#Defining the predictors and outcome
predictors<-c("buying", "maint", "doors", "persons", lug,
              "class")
outcomeName<-"class"

#Training the random forest model
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)
#Predicting using random forest model
testSet$pred_rf<-predict(object = model_rf,testSet[,predictors])

testSet$pred_rf
testSet$class

confusionMatrix(testSet$class,testSet$pred_rf)
