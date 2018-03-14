# Clustering - start with kmeans

set.seed(8953)
iris2 <- iris
iris2$Species <- NULL
(kmeans.result <- kmeans(iris2, 3))

# How accurate is this - compared to species
# Setosa can be easily separated - some overlap in the other two
table(iris$Species, kmeans.result$cluster)

# Then we want to plot the clusters
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")],
       col = 1:3, pch = 8, cex = 2)

# Kmedoids deals better with outliers - paml picks the ideal number of clusters
library(fpc)
pamk.result <- pamk(iris2)
# number of clusters
pamk.result$nc

# check clustering against actual species
table(pamk.result$pamobject$clustering, iris$Species)

# Clustering Plot and silhouette of this
layout(matrix(c(1, 2), 1, 2)) # 2 graphs per page
plot(pamk.result$pamobject)

# Using pam() means we can choose the desired number of clusters
library(cluster)
# group into 3 clusters
pam.result <- pam(iris2, 3)
table(pam.result$clustering, iris$Species)

layout(matrix(c(1, 2), 1, 2)) # 2 graphs per page
plot(pam.result)

# Hierarchical clustering - with the tree cut into 3 clusters
set.seed(2835)
# draw a sample of 40 records from the iris data, so that the
# clustering plot will not be over crowded
idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx, ]
# remove class label
irisSample$Species <- NULL
# hierarchical clustering
hc <- hclust(dist(irisSample), method = "ave")
# plot clusters
plot(hc, hang = -1, labels = iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k = 3)
# get cluster IDs
groups <- cutree(hc, k = 3)

# An alternative - density based clustering
# the 0 cluster shows noise - they are not assigned to one of the three clusters
library(fpc)
iris2 <- iris[-5] # remove class tags
ds <- dbscan(iris2, eps = 0.42, MinPts = 5)
# compare clusters with original class labels
table(ds$cluster, iris$Species)

plot(ds, iris2)
plot(ds, iris2[c(1, 4)])
# Plot by cluster number
plotcluster(iris2, ds$cluster)

# Clustering with added noise - useful for prediction
# create a new dataset for labeling
set.seed(435)
idx <- sample(1:nrow(iris), 10)
# remove class labels
new.data <- iris[idx,-5]
# add random noise
new.data <- new.data + matrix(runif(10*4, min=0, max=0.2),
                              nrow=10, ncol=4)
# label new data
pred <- predict(ds, iris2, new.data)

# Results of prediction for 10 plants - 8 are assigned to the correct labels (3,3,2)
table(pred, iris$Species[idx])

# How does this look - where do they go?
plot(iris2[c(1, 4)], col = 1 + ds$cluster)
points(new.data[c(1, 4)], pch = "+", col = 1 + pred, cex = 3)

