sigma <- matrix(c(10,3,3,2),2,2)
points1<-mvrnorm(n = 100, c(-1, -1), sigma)
points2<-mvrnorm(n = 100, c(2, 1), sigma)
points1b<-cbind(points1, rep(1, 100))
points2b<-cbind(points2, rep(-1, 100))
allPoints<-rbind(points1b, points2b)
alpha     <- 0.8
inTrain   <- sample(1:nrow(allPoints), alpha * nrow(allPoints))
train.set <- allPoints[inTrain,]
test.set  <- allPoints[-inTrain,]
allPointsDataFrame<-as.data.frame(train.set)
allTest<-as.data.frame(test.set)
tree.model <- tree(allPointsDataFrame[,'V3']~allPointsDataFrame[,'V1'] + allPointsDataFrame[,'V2'])
my.prediction <- predict(tree.model, allTest) # gives the probability for each class
plot(tree.model)
text(tree.model)
summary(tree.model)
err<-1-sum(my.prediction==allTest)/nrow(allTest)
print(err)