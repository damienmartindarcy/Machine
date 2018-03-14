# ADV Labs and Projects

# 1 - Basic MLR

# Elementary Multiple Regression 

earnings=c(2841,1876,2934,1552,3065,3670,2005,3215,1930,2010,3111,2882,1683,1817,4066)
age=c(29,21,62,12,40,50,65,44,17,70,20,29,15,14,33)
hours=c(12,8,10,10,11,11,5,8,8,6,9,9,5,7,12)
vendors=data.frame(earnings, age, hours) 

reg <- lm(earnings ~ age + hours,data= vendors)
summary(reg)

beta = coefficients(reg) #least squares estimates
hat = hatvalues(reg) #hat values
yhat = fitted(reg) # predicted values
epsilon = residuals(reg) # residuals

# 2 - Correlation and regression

Age=c(13.5,12,10.5,15,14,10.5,14,16.5,17.5,16.5,17,16.5,16,16.5,17.225,17,16,14.625,14.5,14.5)	
Expenses=c(1,14,16,4,11,15,2,1,1,8,12,2,2,13,2,1,1,12,16,3)
VacancyRate=c(5.02,8.19,3,10.7,8.97,9.45,8,6.62,5.2,11.78,4.62,11.55,9.63,12.99,12.01,12.01,7.99,10.33,10.67,9.45)	
Size=c(0.14,0.27,0,0.05,0.07,0.24,0.19,0.6,0,0.03,0.08,0.03,0,0.04,0.03,0,0.14,0.12,0,0.03)	
RentalRate=c(123000,104079,39998,57112,60000,101385,31300,248172,215000,251015,291264,207549,82000,359665,265500,299000,189258,366013,349930,85335)
RentData=data.frame(Age,Expenses, VacancyRate, Size, RentalRate)


# Pairs Plot
pairs(RentData)
# correlation
cor(RentData)

# Use all explanatory variables
fitfull <- lm(RentalRate~ Age + Expenses + VacancyRate+ Size ,data=RentData)
summary(fitfull)

# Stepwise Regression
fitnull <- lm(RentalRate~ 1 ,data=RentData)
fitfull <- lm(RentalRate~ Age + Expenses + VacancyRate + Size ,data=RentData)
step(fitnull, scope=list(lower=fitnull, upper=fitfull), direction="forward")

# 3 - ANOVA / Extra Sum of Squares

Age=c(13.5,12,10.5,15,14,10.5,14,16.5,17.5,16.5,17,16.5,16,16.5,17.225,17,16,14.625,14.5,14.5)	
Expenses=c(1,14,16,4,11,15,2,1,1,8,12,2,2,13,2,1,1,12,16,3)
VacancyRate=c(5.02,8.19,3,10.7,8.97,9.45,8,6.62,5.2,11.78,4.62,11.55,9.63,12.99,12.01,12.01,7.99,10.33,10.67,9.45)	
Size=c(0.14,0.27,0,0.05,0.07,0.24,0.19,0.6,0,0.03,0.08,0.03,0,0.04,0.03,0,0.14,0.12,0,0.03)	
RentalRate=c(123000,104079,39998,57112,60000,101385,31300,248172,215000,251015,291264,207549,82000,359665,265500,299000,189258,366013,349930,85335)
RentData=data.frame(Age,Expenses, VacancyRate, Size, RentalRate)

fitfull <- lm(RentalRate~ Age + Size + Expenses
              + VacancyRate ,data=RentData)
summary(fitfull)

anova(fitfull) # anova table
coefficients(fitfull)
confint(fitfull, level=0.95)
fitted(fitfull) # predicted values

# 4 - Polynomial models, Interactions, Categorical Predictors

Steroid=c(27.1,22.1,21.9,10.7,1.4,18.8,14.7,5.7,18.6,20.4,9.2,23.4,10.5,19.7,11.8,24.6,3.4,22.8,21.1,24,21.8,23.5,19.4,25.6,12.8,20.8,20.6)
Age=c(23,19,25,12,8,12,11,8,17,18,9,21,10,25,9,17,9,23,13,14,16,17,21,24,13,14,18)
SteroidData=data.frame(Steroid,Age)

pairs(SteroidData) # Clearly a non linear relationship between these

mean(SteroidData$Age)
SteroidData$cen_Age = SteroidData$Age - mean(SteroidData$Age) # Centred version of age

reg = lm(Steroid ~ cen_Age + I(cen_Age^2) + I(cen_Age^3),data=SteroidData)
summary(reg) # cubic polynomial model

# Interactions in the rental data

Age=c(13.5,12,10.5,15,14,10.5,14,16.5,17.5,16.5,17,16.5,16,16.5,17.225,17,16,14.625,14.5,14.5)	
Expenses=c(1,14,16,4,11,15,2,1,1,8,12,2,2,13,2,1,1,12,16,3)
VacancyRate=c(5.02,8.19,3,10.7,8.97,9.45,8,6.62,5.2,11.78,4.62,11.55,9.63,12.99,12.01,12.01,7.99,10.33,10.67,9.45)	
Size=c(0.14,0.27,0,0.05,0.07,0.24,0.19,0.6,0,0.03,0.08,0.03,0,0.04,0.03,0,0.14,0.12,0,0.03)	
RentalRate=c(123000,104079,39998,57112,60000,101385,31300,248172,215000,251015,291264,207549,82000,359665,265500,299000,189258,366013,349930,85335)
RentData=data.frame(Age,Expenses, VacancyRate, Size, RentalRate)

head(RentData)
RentData$cen_Age = RentData$Age - mean(RentData$Age)
RentData$cen_Expenses = RentData$Expenses - mean(RentData$Expenses)

# Model with interaction terms
fit <- lm(RentalRate~ Size + cen_Age + cen_Expenses + cen_Age*cen_Expenses,data=RentData)
summary(fit)

# Is a model with no interaction terms sufficient?
fit2 <- lm(RentalRate~ Size + cen_Age + cen_Expenses, data=RentData)
summary(fit2)

anova(fit, fit2)

# Categorical Predictors

library(faraway)
data(teengamb)

pairs(teengamb)

# Basic model
Gamfit = lm(gamble ~ sex + status + income +verbal+ sex*status + sex*verbal + sex*income,data=teengamb)
summary(Gamfit)

# Stepwise version
fitnull= lm(gamble ~ sex,data=TeenGambData )
fitfull = lm(gamble ~ sex + status + income +verbal + sex*status + sex*verbal + sex*income,data=teengamb)
step(fitfull, scope=list(lower=fitnull, upper=fitfull), direction="backward")

# Plot the fitted model
plot(teengamb$gamble)
lines(fitted(fitfull))

# 5 - Model Diagnostics

# Diagnostic plot - rental data

Age=c(13.5,12,10.5,15,14,10.5,14,16.5,17.5,16.5,17,16.5,16,16.5,17.225,17,16,14.625,14.5,14.5)	
Expenses=c(1,14,16,4,11,15,2,1,1,8,12,2,2,13,2,1,1,12,16,3)
VacancyRate=c(5.02,8.19,3,10.7,8.97,9.45,8,6.62,5.2,11.78,4.62,11.55,9.63,12.99,12.01,12.01,7.99,10.33,10.67,9.45)	
Size=c(0.14,0.27,0,0.05,0.07,0.24,0.19,0.6,0,0.03,0.08,0.03,0,0.04,0.03,0,0.14,0.12,0,0.03)	
RentalRate=c(123000,104079,39998,57112,60000,101385,31300,248172,215000,251015,291264,207549,82000,359665,265500,299000,189258,366013,349930,85335)
RentData=data.frame(Age,Expenses, VacancyRate, Size, RentalRate)

fit <- lm(RentalRate~ Size + Age + Expenses,data=RentData)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

# Plotting rogue observations
plot(RentData$RentalRate)
lines(fitted(fit))
points(5, RentData$RentalRate[5], col = "red")
points(16, RentData$RentalRate[16], col = "red")
points(18, RentData$RentalRate[18], col = "red")

# Crime Dataset
head(USArrests)

# Murder as a function of population and other crimes
fit <- lm(Murder~ Assault + UrbanPop + Rape ,data=USArrests)
summary(fit)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)

#6 - Model Building

library(leaps)

# Using all possible regressions
leaps<-regsubsets( Murder~ Assault + UrbanPop + Rape,nbest=10)
# view results
summary(leaps)

which.max(summary(leaps)$adjr2)
# Solution 37
summary(leaps)$which[37,]

#7 - Ridge and LASSO

# Baseball hitters
rm(list = ls())
install.packages("ISLR")
library(ISLR)
attach(Hitters)
names(Hitters)

dim(Hitters)

# Remove rows with missing values
Hitters=na.omit(Hitters)
dim(Hitters)

sum(is.na(Hitters))

# Model matrix needed as glmnet can only take numerical quantitative inputs
x=model.matrix(Salary~.,Hitters )[,-1]
y=Hitters$Salary

# Ridge - alpha=0
library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

# Lambda at 11498
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# Lambda at 705
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

# Alternatively we can use the predict function
# Obtain ridge regression coordinates for a lambda of 50
predict(ridge.mod,s=50,type="coefficients")[1:20 ,]

# Now we want to estimate error
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Fit model to training set and get MSE (ridge)
ridge.mod=glmnet(x[train ,],y[train],alpha =0, lambda =grid ,
                 thresh =1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

# LASSO
lasso.mod=glmnet(x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)

# Perform cross validations and get associated test error
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha =1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test ,])
mean((lasso.pred-y.test)^2)

# Final LASSO version has just 7 variables
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam )[1:20 ,]

lasso.coef
lasso.coef[lasso.coef!=0]

#8 - Random Effects Models

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

#9 - Generalised linear models
library(HSAUR)
library(HSAUR2)
data(plasma)
attach(plasma)

# Logistic Regression
ESR_num = as.integer(ESR)
ESR_log = (ESR_num == 2)
fit = glm(ESR_log~fibrinogen + globulin, family="binomial")
summary(fit)
# CI for this
confint(fit) # 95% CI for the coefficients

exp(coef(fit)) # exponentiated coefficients
exp(confint(fit)) # 95% CI for exponentiated coefficients

predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals

# Poisson Regression
attach(polyps)

treat_num = as.integer(treat)
treat_log = (treat_num == 1)
fit1 = glm(number~age + treat_log, family="poisson")
summary(fit1)




