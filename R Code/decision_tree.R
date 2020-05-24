# Load the party package. It will automatically load other
# dependent packages.
library(party)
library(rpart)

summary(readingSkills)

nrow(readingSkills)
x <- readingSkills

# Create the input data frame.
trainIndex  <- sample(1:nrow(x), 0.9 * nrow(x))
train <- x[trainIndex,]
test <- x[-trainIndex,]

# Give the chart file a name.
png(file = "decision_tree.png")

tree = rpart(nativeSpeaker ~ age + shoeSize + score, data = train, method = 'class', parms = list(split = "information"))


t_pred = predict(tree,test,type="class")
t_pred
test$nativeSpeaker
confMat <- table(test$nativeSpeaker,t_pred)

accuracy <- sum(diag(confMat))/sum(confMat)
accuracy
# Plot the tree.
plot(tree)

# Save the file.
dev.off()
