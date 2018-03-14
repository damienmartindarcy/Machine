# ADV - Week 8 - Model Building

# Crime 

summary(crime)

# 1 - All Possible Regressions - Old Fashioned Way

# 1 predictor 
reg1 = lm(VR ~ S, data=crime)
reg2 = lm(VR ~ M, data=crime)
reg3 = lm(VR ~ W, data=crime)
# 2 predictors
reg4 = lm(VR ~ S + M, data=crime)
reg5 = lm(VR ~ S + W, data=crime)
summary(reg5)
reg6 = lm(VR ~ M + W, data=crime)
#3 predictors */
reg7 = lm(VR ~ S + M + W, data=crime)

# 2 - Using leap

library(leaps)
leaps<-regsubsets(VR~MR+M+W+H+P+S,nbest=10, data=crime)

# view results
summary(leaps)
which.max(summary(leaps)$adjr2)
summary(leaps)$which[27,]

# 3 - Forward Stepwise Regression

fitnull <- lm(VR~ 1 ,data=crime)
fitfull <- lm(VR~ MR+M+W+H+P+S ,data=crime)
step(fitnull, scope=list(lower=fitnull, upper=fitfull),
     direction="forward") # Should get same results
final<-lm(VR ~ MR + M + S + P, data = crime)
summary(final)


  