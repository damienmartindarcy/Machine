# ADV - Lab Week 10 - Fixed and Random Effects

head(Fiber)
summary(Fiber)


library(lattice)

attach(Fiber)
xyplot(Strength ~ Operator)
xyplot(Strength ~ Machine)
boxplot(Strength ~ Operator*Machine,col=c("white","lightgray"),Fiber)

# Hypothesis Testing
batch = as.factor(batch)
av = aov(y ~ batch)
summary(av)

# Test for evidence of batch to batch variation
library(lme4)
lmer_cal = lmer(y ~ 1 + (1 | batch))
summary(lmer_cal)
anova(lmer(y ~ 1 + (1 | batch)))
anova(y ~ 1 + (1 | batch))

# Estimate of the proportion of variation due to batches - 95% CI
install.packages("ICC")
library("ICC")
ICCest(batch, y, CI.type = "S")

# Estimate of the proportion of variation due to batches - 99% CI
ICCest(batch, y, CI.type = "S",alpha=0.01)

# ANOVA Version
attach(Fiber)
Operator=as.factor(Operator)
Machine=as.factor(Machine)
av=aov(Strength ~ Operator+Machine+Machine*Operator)
summary(av)

