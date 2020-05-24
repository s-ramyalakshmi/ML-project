library(rpart)

data <- read.csv("data.csv")


set.seed(100)  
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))
trainingRowIndex

train <- data[trainingRowIndex,]
test <- data[-trainingRowIndex,]

tree = rpart(class ~ X1 + X2 + X3 + X4, data = train, method = 'class', parms = list(split = "information"))

t_pred = predict(tree, test, type="class")


confMat <- table(test$class,t_pred)
accuracy <- sum(diag(confMat))/sum(confMat)
accuracy
