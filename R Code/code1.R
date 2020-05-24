data_set <- read.csv("data.csv")
summary(data_set)

head(data_set, n=10)

set.seed(20)
clusters <- kmeans(data_set[,2:5], 5)
clusters
