library(C50)
data <- read.csv("data.csv")


set.seed(123)  
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))

train <- data[trainingRowIndex,]
test <- data[-trainingRowIndex,]

model <- C5.0(train[-1], train$class)

pred <- predict(model, test)

confMat <- table(test$class, pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy
