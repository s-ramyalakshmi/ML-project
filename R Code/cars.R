cars
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

cor(cars$speed, cars$dist)

linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)

summary(linearMod)

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.9*nrow(cars))  # row indices for training data
nrow(cars)
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
actuals_preds
correlation_accuracy <- cor(actuals_preds)  # 82.7%
correlation_accuracy
head(actuals_preds)