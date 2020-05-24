library(class)
data <- read.csv("data.csv")

set.seed(100)  
trainingRowIndex <- sample(1:nrow(data), 0.8*nrow(data))


da_train <- data[trainingRowIndex, -1]
da_test <- data[-trainingRowIndex, -1]

da_train_labels <- data[trainingRowIndex, 1]
da_test_labels <- data[-trainingRowIndex, 1]
da_test_labels
max_k = 0
note_k = 0

for(p in 1:100) {

  da_test_pred <- knn(train = da_train, test = da_test, cl = da_train_labels, k=p)
  c1 = 0
  c2 = 0
  for(i in (1:nrow(da_test))) {
    if(da_test_pred[i] == da_test_labels[i]) {
      c1 = c1 + 1
    } else {
      c2 = c2 + 1
    }
  }
  
  #confMat <- table(da_test_labels, da_test_pred)
  #accuracy <- sum(diag(confMat))/sum(confMat)
  accuracy = c1 / (c1 + c2) * 100
  if(max_k < accuracy) {
    max_k = accuracy
    note_k = p
  }

}

note_k
max_k
