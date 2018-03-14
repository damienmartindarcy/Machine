# Q1 - LSE of beta values


y <- matrix(test$y, ncol = 1)
x <- as.matrix(cbind(1, test$x1, test$x2))

x
y

n <- nrow(x)
p <- ncol(x)

x
beta.hat <- solve(t(x) %*% x) %*% t(x) %*% y
beta.hat
t(y) %*% y

#the variance, residual standard error and df's
y.hat <- x %*% beta.hat
sigma2 <- sum((y - y.hat)^2)/(n - p)


reg<-lm(y~x1+x2, data=test)
summary(reg)
test

beta=coefficients(reg) # Least Squares Estimates
coefficients(reg) # model coefficients
confint(reg, level=0.9) # CIs for model parameters 
hat=hatvalues(reg) # hat values
yhat=fitted(reg) # Predicted Values
epsilon=residuals(reg) # Residuals

t.test(reg$x1,mu=0) # Ho: mu=3

anova(reg)
newdata = data.frame(x1=2.1,x2=0.5)
predict(reg, newdata, interval="predict")
summary(reg)$coef[,2] 
summary(reg)$sigma
vcov(reg)



#2 - PGA Data

# Correlation

head(PGA)
PGA <-PGA[-c(1:3)]
cor(PGA)
pairs(PGA)

# Multiple Regession

reg<-lm(TotalWinnings~Age+AvgDrive+GreensReg+AvPutts, data=PGA)
summary(reg)

beta=coefficients(reg) # Least Squares Estimates
hat=hatvalues(reg) # hat values
yhat=fitted(reg) # Predicted Values
epsilon=residuals(reg) # Residuals

# Regression
# Multiple regression model using all exaplanatory variables
fitfull<-reg<-lm(TotalWinnings~Age+AvgDrive+GreensReg+AvPutts, data=PGA)
summary(fitfull)

# Stepwise regression to fit multiple models simultaneously
fitnull<-lm(TotalWinnings~1, data=PGA)
summary(fitnull)

step(fitnull, scope=list(lower=fitnull, upper=fitfull), direction="forward")

fitfinal<-lm(TotalWinnings~AvPutts+GreensReg+AvgDrive, data=PGA)
summary(fitfinal)
beta=coefficients(fitfinal)


reg<-lm(TotalWinnings~Age+AvgDrive+GreensReg+AvPutts, data=PGA)
summary(reg)

# Wrap the parameters in a dataframe
newdata = data.frame(Age=36, AvgDrive=290, GreensReg=65, AvPutts=1.77)
# Apply to the original model
predict(reg, newdata)
