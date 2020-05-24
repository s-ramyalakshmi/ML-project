library("e1071")
data <- read.csv("data.csv")

set.seed(123)  
trainingRowIndex <- sample(1:nrow(data), 0.7*nrow(data))

train <- data[trainingRowIndex,]
test <- data[-trainingRowIndex,]


model = lm(formula = class~X1+X2+X3+X4, data = train)
pred = predict(model, test, type = "response")
pred
