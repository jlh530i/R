#Basic cluster analysis of housing in Copenhagen
library(cluster)

#import 
dat <- read.table("http://data.princeton.edu/wws509/datasets/copen.dat", header=TRUE)

#create dummy variables for categorical data - needs improvement
matrix <- model.matrix(~ 0+ ., data = dat)

#scale
dat2 <- scale(matrix)

#PCA
pca <- princomp(dat2, cor=FALSE)
#skree
plot(pca, type = 'lines')

# Determine number of clusters
wss <- (nrow(dat2)-1)*sum(apply(dat2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(dat2, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Clustering
fit <- kmeans(dat2, 4)
# get cluster means 
aggregate(dat2,by=list(fit$cluster),FUN=mean)
# append cluster assignment
clusterdat <- data.frame(dat2, fit$cluster)

#cluster assignments
assigned <- cbind(dat,clusterdat$fit.cluster)

#visualize
clusplot(dat2, fit$cluster)
