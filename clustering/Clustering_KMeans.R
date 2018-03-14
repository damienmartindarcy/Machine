# K Means Clustering

data(faithful)

WGSS <- rep(0,10)

n <- nrow(faithful)

WGSS[1] <- (n-1) * sum(apply(faithful, 2, var))

for(k in 2:10)
{
  WGSS[k] <- sum(kmeans(faithful, centers = k)$withinss)
}

plot(1:10, WGSS, type="b", xlab="k", ylab="Within group sum of squares")

K <- 3
cl <- kmeans(faithful, center=K)
table(cl$cluster)

plot(faithful, col= cl$cluster)
points(cl$centers, col=1:K, pch=8)

