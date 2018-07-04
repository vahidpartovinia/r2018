remove(list=ls())

x_tr <- seq(0, 2, length=200)
y_tr <- exp(3*x_tr)

plot(x_tr, y_tr,type="l")

set.seed(10)
y <- y_tr+rnorm(length(x_tr), mean=0, sd=25)

plot(x_tr,y)

plot(x_tr,y)
points(x_tr, y_tr, type="l", col="red")

regdata <- as.data.frame(cbind(y_tr, x_tr, x_tr^2))
colnames(regdata) <- c("y","x","z")
#regdata
#lr.fit <- lm(y~x, data=regdata)
class(lr.fit)
lr.fit <- lm(y~x+z, data=regdata)
yhat <- predict(lm(y~x+z, data=regdata))


plot(x_tr,y)
points(x_tr, y_tr, type="l", col="red")
points(x_tr, yhat,type="l",col="green")

as.numeric(y>90)
z<-((y>90)-0.5)*2

z

plot(x_tr, z)

traindata <- as.data.frame(cbind(z, x_tr))
colnames(traindata) <- c("z", "x")
#traindata
zhat <- predict(lm(z~x, data=traindata))

zclass <- 2*(zhat>0)-1

mean(zclass==z)

plot(x_tr,z, col= z+3)
points(x_tr, zhat, type="l", col="green")
points(x_tr, zclass, type="l", col="cyan")

require(MASS)
n <- 50
X1 <- mvrnorm(n, mu=c(1,1), Sigma=diag(c(1,1)))
X2 <- mvrnorm(n, mu=c(-1,-1), Sigma=diag(c(1,1)))
X <- rbind(X1,X2)
z <- rep(c(-1,1), each=n)            

plot(X[,1], X[,2],col=z+3)

traindata <- as.data.frame(cbind(z, X))
colnames(traindata) <- c("z", "x1", "x2")
lm.obj <- lm( z~x1+x2, data=traindata)
zhat <- predict(lm.obj)

n <- 50
X.grid <- expand.grid(seq(-4, 4, length=n), seq(-4, 4, length=n))
testdata <- as.data.frame (cbind(z, X.grid)) 
colnames(testdata) <- c("z", "x1", "x2")
zhat.grid <- predict(lm.obj, newdata=testdata)
z.gridclass <- 2*(zhat.grid>0)-1
plot(X.grid[,1], X.grid[,2], col=z.gridclass+3)
