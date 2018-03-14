# Ridge and LASSO Lab

# Baseball Players
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# We want to predict hitters salary based on the rest of the data 
# Note how this behaves differently
# Model matrix transforms qualitative variables into dummy variables

x=model.matrix(Salary~.,Hitters )[,-1]
y=Hitters$Salary

# Ridge Regression - Alpha=0
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

# When lambda = 11498
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# When lambda = 705
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# Predict ridge regression coefficients for a new value of lambda (50)
predict(ridge.mod,s=50,type="coefficients")[1:20 ,]

# Random subset of numbers to generate test / training observations
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# We fit the model to the training set and evaluate its MSE on the test set
# using lambda = 4

ridge.mod=glmnet(x[train ,],y[train],alpha =0,lambda=grid,thresh =1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

# If we fit a model with just an intercept - we get a much larger MSE - as below
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
114783
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients") [1:20 ,]

# What value of lambda gives us the smallest crossvalidation error 
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha =0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

# What is the MSE of this lambda value (212)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test ,])
mean((ridge.pred-y.test)^2)

# Then we fit the model to the full dataset using the lambda selected above
# As expected none of the coefficiens are zero
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam )[1:20 ,]

# LASSO - Alpha=1

lasso.mod=glmnet(x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)

# Cross validation and error test - most variables from before
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test ,])
mean((lasso.pred-y.test)^2)

# The low MSE of 100743 is better than the null model and least squares 
# It is also broadly similar to the Ridge example above

# Note how many of the coefficients are reduced to zero
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam )[1:20 ,]

lasso.coef

# We can isolate the factors which are not zero
lasso.coef[lasso.coef!=0]
lasso.coef[lasso.coef==0]



