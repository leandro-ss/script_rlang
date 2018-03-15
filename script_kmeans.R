###################################################################
############# NAO MONITORADO ######################################
###################################################################

# Hierarchical Clustering

# Importing the dataset
dataset = read.csv('dataset/mall_customers.csv')
dataset = dataset[4:5]

# Splitting the dataset into the Training set and Test set
 install.packages('caTools')
 library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Using the dendrogram to find the optimal number of clusters
dendrogram_x = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')

plot(dendrogram_x)

y_dendrogram = cutree(dendrogram_x, 5)

# Fitting Hierarchical Clustering to the dataset

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_dendrogram,
         #lines = 0,
         shade = TRUE,
         color = TRUE,
         stand = FALSE,
         labels= 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


############################################################################################
############################################################################################
############################################################################################


# # Hierarchical Clustering

# # Importing the dataset
# dataset = read.csv('sample/mall_customers.csv')
# dataset = dataset[4:5]

# library(ggplot2)
# ggplot(dataset, aes(Annual.Income, Spending.Score)) + geom_point()


## plotting iris (data frame) in a 2-dimensional plot and partitioning
## into 3 clusters.
data(iris)
iris.x <- iris[, 1:4]
cl3 <- pam(iris.x, 3)$clustering
op <- par(mfrow= c(1,1))

clusplot(iris.x, cl3, color = TRUE)
U <- par("usr")
## zoom in :
rect(0,-1, 2,1, border = "orange", lwd=2)
clusplot(iris.x, cl3, color = TRUE, xlim = c(0,2), ylim = c(-1,1))
box(col="orange",lwd=2); mtext("sub region", font = 4, cex = 2)
##  or zoom out :
clusplot(iris.x, cl3, color = TRUE, xlim = c(-4,4), ylim = c(-4,4))
mtext("`super' region", font = 4, cex = 2)
rect(U[1],U[3], U[2],U[4], lwd=2, lty = 3)

# reset graphics
par(op)

############################################################################################
############################################################################################
############################################################################################

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()

#summary(iris)
#print (iris)

m=as.matrix(cbind(iris$Petal.Length, iris$Petal.Width),ncol=2)
cl=(kmeans(m,3))

iris$cluster=factor(cl$cluster)
centers=as.data.frame(cl$centers)

ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, color=cluster )) +
  geom_point() +
  geom_point(data=centers, aes(x=V1,y=V2, color='Center')) +
  geom_point(data=centers, aes(x=V1,y=V2, color='Center'), size=52, alpha=.3, legend=FALSE)


############################################################################################
############################################################################################
############################################################################################



## plotting votes.diss(dissimilarity) in a bivariate plot and
## partitioning into 2 clusters
data(votes.repub)
votes.diss <- daisy(votes.repub)
pamv <- pam(votes.diss, 2, diss = TRUE)
clusplot(pamv, shade = TRUE)
## is the same as
votes.clus <- pamv$clustering
clusplot(votes.diss, votes.clus, diss = TRUE, shade = TRUE)
## Now look at components 3 and 2 instead of 1 and 2:
str(cMDS <- cmdscale(votes.diss, k=3, add=TRUE))
clusplot(pamv, s.x.2d = list(x=cMDS$points[, c(3,2)],



