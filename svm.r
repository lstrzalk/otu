require("MASS")
require("e1071")
n <- 2
A <- matrix(runif(n^2)*2-1, ncol=n)
sigma <- t(A) %*% A
B <- matrix(runif(n^2)*2-1, ncol=n)
points1<-mvrnorm(n = 100, c(-1, -1), sigma)
points2<-mvrnorm(n = 100, c(2, 1), sigma)
points1b<-cbind(rep(1, 100), points1)
points2b<-cbind(rep(-1, 100), points2)
allPoints<-rbind(points1b, points2b)
allPointsDataFrame<-as.data.frame(allPoints)
c <- allPointsDataFrame$V1
allPointsDataFrame$V1 <- as.factor(allPointsDataFrame$V1)
w <- allPointsDataFrame$V2
l <- allPointsDataFrame$V3
svm.model<-svm(as.factor(c)~w+l, data=allPointsDataFrame,type="C-classification", kernel="linear", cost=10, cross=0,scale=FALSE)
print(summary(svm.model))
y.theoretical <- fitted(svm.model)
print(table(y.theoretical, allPointsDataFrame[,1]))
print((table(y.theoretical, allPointsDataFrame[,1])[1,2] + table(y.theoretical, allPointsDataFrame[,1])[2,1]) / (nrow(allPointsDataFrame))*100)

#print(table(c, y.theoretical))
min.w <- min(w)
max.w <- max(w)
min.l <- min(l)
max.l <- max(l)
#plot(svm.model, allPointsDataFrame, w~l, svSymbol="o", dataSymbol="o")
zerojeden <- ifelse((svm.model$decision.values > 0), 1, 0)
#plot(allPoints[,2], allPoints[,3], col = zerojeden + 1, xlim = c(-5,5), ylim=c(-5,5))
plot(allPoints[,2], allPoints[,3], col = allPoints[,1]+2, xlim = c(min.w,max.w), ylim=c(min.l,max.l))
x<- seq(min.w, max.w, length=100)
y<- seq(min.l, max.l, length=100)
myallPointsDataFrameT<-expand.grid(w=x, l=y)
n <- length(x)
y.pred <- predict(svm.model, myallPointsDataFrameT)
z <- y.pred
contour(x, y, matrix(z, n), add=TRUE, levels=0, labcex = 0, drawlabels = FALSE)
v <- t(svm.model$coefs) %*% svm.model$SV
b <- -svm.model$rho
abline(a=-b/v[1,2], b=-v[1,1]/v[1,2], col="black", lty=1)
abline(a=-(b-1)/v[1,2], b=-v[1,1]/v[1,2], col="orange", lty=3)
abline(a=-(b+1)/v[1,2], b=-v[1,1]/v[1,2], col="orange", lty=3)