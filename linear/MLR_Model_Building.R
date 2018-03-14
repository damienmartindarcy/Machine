#6 - Model Building

library(leaps)

# Crime Dataset
head(USArrests)

# Using all possible regressions
leaps <-regsubsets( Murder~ Assault + UrbanPop + Rape,nbest=10)

# view results
summary(leaps)

which.max(summary(leaps)$adjr2)
# Solution 37

summary(leaps)$which[37,]
