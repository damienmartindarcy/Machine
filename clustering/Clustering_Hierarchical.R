# Hierarchical Clustering

data(oliveoil)
head(oliveoil)

acids = oliveoil[,3:10]

# Standardize
StDev = apply(acids, 2, sd)
apply(acids, 2, mean)
stdacids = sweep(acids, 2, StDev, "/")

# Average Linkage
cl.average = hclust(dist(stdacids, method="euclidean"), method="average")
plot(cl.average)

# Single Linkage - best avoided
cl.single = hclust(dist(stdacids), method="single")
plot(cl.single)

# Complete Linkage - final choice
cl.complete = hclust(dist(stdacids), method="complete")
plot(cl.complete)

# Cut the dendrogram at 5 clusters
hcl = cutree(cl.complete, k = 5)
hcl
table(hcl)

# Compare with original cluster allocation
table(hcl, oliveoil[,1])
