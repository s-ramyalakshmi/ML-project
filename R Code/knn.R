
da <- read.csv("data.csv",stringsAsFactors = FALSE)
da$class[da$class == "L"] = 0
da$class[da$class == "B"] = 1
da$class[da$class == "R"] = 2
da
da_n <- da[-1] #removing first field
da_n
#table(da$diagnosis)
#summary(da[c("radius_mean","area_mean","smoothness_mean")])


#normalize <- function(x){
#  return((x-min(x))/(max(x)-min(x)))
#}
#normalize(c(1,2,3,4,5))


#da_n <- as.data.frame(lapply(da[2:31],normalize)) 

#summary(da_n$area_mean)

n_values = 0.7*nrow(da)
n_values
da_n

da_train <- da_n[0:n_values,]
da_test <- da_n[((n_values+1):nrow(da_n)),]
da_test
da_train

da_train_labels <- da[0:n_values,1]
da_test_labels <- da[((n_values+1):nrow(da_n)),1]
library(class)
da_test_pred <- knn(train= da_train,test = da_test,cl=da_train_labels,k=21)

actuals_preds <- data.frame(cbind(actuals=da_test_labels, predicteds=da_test_pred))
da_test_pred
actuals_preds
summary(da_test_pred)

correlation_accuracy <- cor(actuals_preds)

library(gmodels)


CrossTable(x=da_test_labels,y=da_test_pred,prop.chisq=FALSE)
