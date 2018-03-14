library(mclust)
data(thyroid)
pairs(thyroid[,-1])

res = Mclust(thyroid[,-1], G=1:5)
summ = summary(res)
summ

# Plot BIC values
res$BIC
plot(res, what="BIC", legendArgs = list(x="topleft", cex=0.5,horiz=T), ylim=c(-7400, -4000))

plot(res, what="classification")

attributes(summ)
summ$pro
summ$mean
summ$variance

# Assess uncertainty associated with the model
plot(res, what="uncertainty")

# Cluster uncertainty at the individual patient level
plot(res$uncertainty, type="h")

# Rand Index comparison with full medical record patients
table(thyroid[,1], summ$classification)
adjustedRandIndex(thyroid[,1], summ$classification)
