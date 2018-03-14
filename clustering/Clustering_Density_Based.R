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
new.data <-iris[idx,-5]

# add random noise
new.data <- new.data + matrix(runif(10*4, min=0, max=0.2),
                              nrow=10, ncol=4)

# label new data
pred &lt <- predict(ds, iris2, new.data)

# Results of prediction for 10 plants - 8 are assigned to the correct labels (3,3,2)
table(pred, iris$Species[idx])

# How does this look - where do they go?
plot(iris2[c(1, 4)], col = 1 + ds$cluster)
points(new.data[c(1, 4)], pch = "+", col = 1 + pred, cex = 3)


