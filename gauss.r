require("MASS")
require("e1071")
library(mlbench)
library(rpart)
sigma <- matrix(c(1000,1,3,2),2,2)
allPoints1<-mvrnorm(n = 100, c(-2, -1), sigma)
allPoints2<-mvrnorm(n = 100, c(2, 1), sigma)
points1b<-cbind(points1, rep(1, 100))
points2b<-cbind(points2, rep(-1, 100))
allPoints<-rbind(points1b, points2b)
alpha     <- 0.8
inTrain   <- sample(1:nrow(allPoints), alpha * nrow(allPoints))
train.set <- allPoints[inTrain,]
test.set  <- allPoints[-inTrain,]
allPointsDataFrame<-as.data.frame(train.set)
c <- allPointsDataFrame$V3
classTesting<-append(rep(1, 20), rep(-1, 20))
w <- allPointsDataFrame$V1
l <- allPointsDataFrame$V2
lda.allPointsDataFrame <- lda(c~w+l, data=allPointsDataFrame)
allTestingPointsDateFrame <- as.data.frame(test.set)
colnames(allTestingPointsDateFrame) <- c("w", "l")
y.pred <- predict(lda.allPointsDataFrame,allTestingPointsDateFrame)
errorTesting <- 1 -sum(y.pred$class==classTesting)/length(classTesting)
myallPointsDataFrame<- matrix(nrow=160, ncol=2)
myallPointsDataFrame[,1] <- w
myallPointsDataFrame[,2] <- l
class<- c
min.w <- min(w)
max.w <- max(w)
min.l <- min(l)
max.l <- max(l)
x<- seq(min.w, max.w, length=80)
y<- seq(min.l, max.l, length=80)
myallPointsDataFrameT<-expand.grid(w=x, l=y)
n <- length(x)
y.pred <- predict(lda.allPointsDataFrame,
as.data.frame(myallPointsDataFrame))
error <- 1 - sum(y.pred$class==class)/length(class)
y.pred <- predict(lda.allPointsDataFrame, myallPointsDataFrameT)
plot(w, l, xlim=c(min.w, max.w), ylim=c(min.l, max.l), type = "n")
text(w, l, substr(c, 1, 1))
z <- y.pred$posterior[,1] - y.pred$posterior[,2]
contour(x, y, matrix(z, n), add=TRUE, levels=0.5, labcex = 0,
drawlabels = FALSE)
print(error)
