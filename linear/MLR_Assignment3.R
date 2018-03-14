# STAT 40770 - Assignment 3 - 15202834

# Question 1

# EDA using XDA
library(devtools)
install_github("ujjwalkarn/xda")

# Import and verify
data<-Q1data
head(data)
summary(data)

# Convert to dataframe
data.frame(data)

# Drop initial column
data$X1 <- NULL

# Convert x3 to factor
data$x3 <- factor(data$x3)

# 1 - EDA - Suggest non linear relationships between variables
# Pairs
pairs(data)
cor(data)

# XDA to look more closely

library(xda)

# Basic on continuous variables
numSummary(data)
# Basic on binary variables
charSummary(data)
# Bivariate analysis between two variables
bivariate(data,'y','x3')
bivariate(data,'x1','x3')
bivariate(data,'x2','x3')
# Plot all against y
Plot(data,'y')

# 2 - Transformations - lots to try - doesn't make much difference though

# a - Log Transformation / Translation - this is the one we try - slightly linearized
data$x1 = log10(data$x1 + 1 - min(data$x1))
data$x2 = log10(data$x2 + 1 - min(data$x2))
pairs(data)

# b - SQRT Transformation
data$x1 = sqrt(data$x1 + 10)
data$x2 = sqrt(data$x2 + 10)
pairs(data)

# c - exp Transformation
data$x1 <- exp(data$x1)
data$x2 <- exp(data$x2)
head(data)
pairs(data)

# d - Inverse Transformation
data$x1 <- (1/data$x1)
data$x2 <- (1/data$x2)
head(data)
pairs(data)


# 3 - Model Fitting

# Preliminary model fitting

# Initial - Can we drop any of the variables?
fitnullinitial <- lm(y~ 1 ,data=data)
fitfullinitial <- lm(y~.,data=data)
step(fitnullinitial, scope=list(lower=fitnullinitial, upper=fitfullinitial), direction="forward")
anova(fitfullinitial)
summary(fitfullinitial)
plot(fitfullinitial)

# Interactions - Do we need all of these? - stepwise Elimination
fitnull<-lm(y ~ 1,data=data)
fitfull<-lm(y ~ x1+x2+x3+x1*x2 + x1*x3 + x2*x3,data=data)
step(fitfull, scope=list(lower=fitnull, upper=fitfull), direction="backward")
final<-lm(formula = y ~ x1 + x2 + x3 + x1*x2 + x2*x3, data = data)
anova(final)
summary(final)

# Diagnostic Plots
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fitfullinitial)
plot(final)
plot(data$y)
lines(fitted(final))

# Anova of model with interactions vs without interactions
anova(fitfullinitial, final)

##############################################################

# Question 2

library(lme4)
politeness=read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")

head(politeness)
summary(politeness)

# Missing value in row 39
which(is.na(politeness$frequency))

# Relationship between politness and pitch
boxplot(frequency ~ attitude*gender,col=c("white","lightgray"),politeness)

# Attempt 1 - it needs a random effect
lmer(frequency ~ attitude, data=politeness)

# Attempt 2 - Add random effects for subject and scenario
politeness.model = lmer(frequency ~ attitude +(1|subject) + (1|scenario), data=politeness)
summary(politeness.model)

# Attempt 3 - Add gender as an additional fixed effect
politeness.model = lmer(frequency ~ attitude +gender + (1|subject) +(1|scenario), data=politeness)
summary(politeness.model)

# Full and reduced models - start with null model
politeness.null = lmer(frequency ~ gender +(1|subject) + (1|scenario), data=politeness,REML=FALSE)

# Then the full model
politeness.model = lmer(frequency ~ attitude +gender + (1|subject) + (1|scenario),data=politeness, REML=FALSE)

# Likelihood ration test - comparing the two models
anova(politeness.null,politeness.model)

coef(politeness.model)

# Random slope model
politeness.model = lmer(frequency ~ attitude +gender + (1+attitude|subject) + (1+attitude|scenario),data=politeness,REML=FALSE)
coef(politeness.model)






