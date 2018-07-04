remove(list=ls())
zipdata <- as.matrix(read.table("~/Desktop/zip.train"))

dim(zipdata)

#zipdata[1,1:16]
#(matrix((zipdata[1,2:257]),nrow=16, ncol=16))

par(mar=c(0,0,0,0))
image((matrix((zipdata[1,2:257]),nrow=16, ncol=16)))


image((matrix((zipdata[1,2:257]),nrow=16, ncol=16))[,16:1],col = gray((0:255)/255))
Seems we have the negative image. 
image((matrix(( - zipdata[1,2:257]), nrow=16, ncol=16))[,16:1], col = gray((0:255)/255), axes=FALSE)

zipdata3 <- zipdata[zipdata[,1]==3,]
zipdata8 <- zipdata[zipdata[,1]==8,]
nrow(zipdata3)
set.seed(10)
random3 <- sample(1:nrow(zipdata3),size=10)

zipdata3stack <- matrix(zipdata3[random3[1],2:257], 16, 16)[,16:1]
dim(zipdata3stack)
for (i in 2:length(random3))
    {
    zipdata3stack <- cbind(zipdata3stack, matrix(zipdata3[random3[i],2:257], 16, 16)[,16:1])
    }


image(zipdata3stack)

par(mar=c(0,10,0,10))
image(-zipdata3stack, col= gray((0:255)/255),axes=FALSE)

set.seed(10)
random8 <- sample(1:nrow(zipdata8),size=10)

zipdata8stack <- matrix(zipdata8[random3[1],2:257], 16, 16)[,16:1]
dim(zipdata8stack)
for (i in 2:length(random8))
    {
    zipdata8stack <- rbind(zipdata8stack, matrix(zipdata8[random8[i],2:257], 16, 16)[,16:1])
    }


par(mar=c(5,0,5,0))
image(-zipdata8stack, col= gray((0:255)/255),axes=FALSE)


X <- as.data.frame(zipdata[,2:257])
y <- zipdata[,1]

X_pca<- princomp(X,2)
X_pred <- predict(X_pca)
plot(X_pca)

index <- (y==1) | (y==0)
plot( X_pred[index,1], X_pred[index,2])


plot( X_pred[y==1,1], X_pred[y==1,2], pch="1",col="red",xlim=c(-20,10), ylim=c(-15,10))
points( X_pred[y==0,1], X_pred[y==0,2], pch="0", col="blue")

index <- (y==3) | (y==8)
plot( X_pred[y==3,1], X_pred[y==3,2], pch="3",col=adjustcolor("red", alpha=0.2),xlim=c(-20,10), ylim=c(-15,10))
points( X_pred[y==8,1], X_pred[y==8,2], pch="8", col=adjustcolor("blue", alpha=0.2))

require(MASS)
xy <- as.data.frame(zipdata)
colnames(xy) <- c("digit",paste("pixel_",1:256,sep=""))
xy$digit <- as.factor(xy$digit)
xy_lda <- lda(digit~.,data=xy)
xy_pred <- predict(xy_lda)$x


index <- (y==1) | (y==0)
plot( xy_pred[index,1], xy_pred[index,2])

plot( xy_pred[y==1,1], xy_pred[y==1,2], pch="1",col="red",xlim=c(-10,10), ylim=c(-5,5))
points( xy_pred[y==0,1], xy_pred[y==0,2], pch="0", col="blue")

library(scatterplot3d) 
s<-scatterplot3d( xy_pred[y==1,1], xy_pred[y==1,2], xy_pred[y==1,3],color="red", pch="1",xlim=c(-10,10), ylim=c(-10,10),
                zlim=c(-10,10))
s$points3d( xy_pred[y==0,1], xy_pred[y==0,2], xy_pred[y==0,3],col="blue", pch="0")

#install.packages("mylibrary")
require(e1071)

plot(xy_pred[,1],xy_pred[,2],pch=paste(round(y)),col=adjustcolor((round(y+1)), alpha=0.2))


plot(xy_pred[y==9,1],xy_pred[y==9,2],pch="9",col=2,xlim=c(-10,10),ylim=c(-10,10))
points(xy_pred[y==4,1],xy_pred[y==4,2],pch="4",col=1)

zip_pc <- rbind(xy_pred[y==9,1:2],xy_pred[y==4,1:2])
dim(zip_pc)

zip_pc <- cbind(rep(c(9,4),c(sum(y==9),sum(y==4))), zip_pc)
zip_pc <- as.data.frame(zip_pc)
colnames(zip_pc) <- c("digit", "pc1", "pc2")
zip_pc$digit <- as.factor(zip_pc$digit) 

svm.model <- svm (digit~., data=zip_pc, kernel="linear")

n=50
zip_pc_test <- expand.grid(seq(-10,10,length=n),seq(-10,10,length=n))
colnames(zip_pc_test) <- c("pc1","pc2")

svm.pred <- predict(svm.model, zip_pc_test)

plot(zip_pc_test[,1],zip_pc_test[,2], col=svm.pred,pch=paste(svm.pred))

svm.model <- svm (digit~., data=zip_pc, kernel="radial")
svm.pred <- predict(svm.model, zip_pc_test)
plot(zip_pc_test[,1],zip_pc_test[,2], col=svm.pred,pch=paste(svm.pred))

