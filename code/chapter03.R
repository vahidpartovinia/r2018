
remove(list=ls())
set.seed(15)
n <- 100
center <- 2
x <- c(rnorm(n, mean=-center, sd=1), rnorm(n, mean=center, sd=1))
y <- c(rnorm(n, mean=-center, sd=1), rnorm(n, mean=center, sd=1))

xy <- cbind(x,y)

plot(xy[,1], xy[,2])

xy_hclust <- hclust(dist(x), method="average")
plot(xy_hclust)

xy_dendro <- as.dendrogram(xy_hclust)
plot(xy_dendro, leaflab="none",type="triangle")
abline(h=2)

#? plot.dendrogram

xy_group <- cutree(xy_hclust, k=2)

plot(xy[,1],xy[,2],col=xy_group, pch=paste(xy_group))

data(mtcars)
par(mar=c(10,5,0,0))
plot(as.dendrogram(hclust(dist(mtcars))))

xy_kmeans=kmeans(xy,centers=2)
plot(xy[,1],xy[,2],col=xy_kmeans$cluster, pch=paste(xy_kmeans$cluster))
points(xy_kmeans$centers[,1],xy_kmeans$centers[,2],pch=3,lwd=3,col="blue",cex=5)

library(imager)
# you may need to install mageMagick library
#url="http://img.webmd.com/dtmcms/live/webmd/consumer_assets/site_images/articles/health_tools/is_my_cat_normal_slideshow/photolibrary_rf_photo_of_cat_eating_red_yarn.jpg"
cat <- load.image("~/Desktop/cat1.jpg")
catgray<-grayscale(cat)
plot(catgray)

dim(cat)
dim(catgray)
nrow <- dim(catgray)[1]
ncol <- dim(catgray)[2]


cat_kmeans<-kmeans(as.vector(catgray),centers=2)
image(matrix(cat_kmeans$cluster,nrow,ncol)[,335:1],col=c("black","white"))


