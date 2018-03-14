# Cluster Plot allows examination of individual observations 

clusplot(faithful, cl$cluster, color=TRUE, shade=TRUE, labels=3, lines=0)

# Silhouette Plot assesses accuracy of clustering solution

library(cluster) 
library(HSAUR) 
data(faithful) 
km <- kmeans(faithful,3) 
dissE <- daisy(faithful) 
dE2 <- dissE^2
sk2 <- silhouette(km$cl, dE2) 
plot(sk2) 

