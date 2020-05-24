library("e1071")
data <- read.csv("data.csv")

set.seed(123)  
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))

train <- data[trainingRowIndex,]
test <- data[-trainingRowIndex,]

mbays <- naiveBayes(train[-1], train$class,laplace=1)
pred <- predict(mbays, test[-1], type="raw")




cumm_pred <- rep("", nrow(pred))
cumm_pred2 <- rep("", nrow(pred))

for(i in 1:nrow(pred)) {
  if(pred[i,1] >= pred[i,2]) {
    if(pred[i,1] >= pred[i,3]) {
      cumm_pred[i] = "B"
    } else {
      cumm_pred[i] = "R"
    }
  } else {
    if(pred[i,2] >= pred[i,3]) {
      cumm_pred[i] = "L"
    } else {
      cumm_pred[i] = "R"
    }
  }
}


cumm_pred
cumm_pred2[] <- lapply(test$class, as.character)

c1 = 0
c2 = 0
for(i in (1:nrow(test))) {
  if(cumm_pred[i] == cumm_pred2[i]) {
    c1 = c1 + 1
  } else {
    c2 = c2 + 1
  }
}

c1
c2
accuracy = c1 / (c1 + c2) * 100
accuracy


