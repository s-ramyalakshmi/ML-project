#randomforest
library(randomForest)
library(caret)
library(e1071)


data <- read.csv("data.csv")
set.seed(123)  
trainingRowIndex <- sample(1:nrow(data), 0.7*nrow(data))

train <- data[trainingRowIndex,]
test <- data[-trainingRowIndex,]
summary(train)
#train
rftrain <- randomForest(class~., data = train)
pred <- predict(rftrain, test)

confusionMatrix(predict(rftrain, test), test$class)
