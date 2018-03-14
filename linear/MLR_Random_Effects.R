# Random Effects Models
batch=c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5) 
y=c(23.46,23.48,23.56,23.39,23.4,23.59,23.46,23.42,23.49,23.5,23.51,23.64,23.46,23.52,23.49,23.28,23.4,23.37,23.46,23.39,23.29,23.46,23.37,23.32,23.38)
calcium=data.frame(batch,y)

library(lattice)
attach(calcium)
xyplot(y ~ batch)

# test for batch to batch variation
batch = as.factor(batch)
av = aov(y ~ batch)

# using lmer
library(lme4)
lmer_cal = lmer(y ~ 1 + (1 | batch))
summary(lmer_cal)

# Estimate proportion of variance due to batches
install.packages("ICC")
library("ICC")
ICCest(batch, y, CI.type = "S")

# With CI for this
ICCest(batch, y, CI.type = "S",alpha=0.01)