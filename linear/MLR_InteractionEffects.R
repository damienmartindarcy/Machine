# ADV - W6 Lab
# Polynomials, Interaction Effects, Categorical Predictors

# Scatterplot of Steroids
pairs(Steroids)

# This is not a linear relationship - the predictor variables are highly correlated
# We therefore want to centre the variables by subtracting their means

mean(Steroids$Age)
Steroids$cen_Age = Steroids$Age - mean(Steroids$Age)

# Then we fit a polynomial model
reg = lm(Steroid ~ cen_Age + I(cen_Age^2) + I(cen_Age^3),data=Steroids)
summary(reg)

# Interactions in the rent model
# We centre both age and expenses and fit them to the model
Rent$cen_Age = Rent$Age - mean(Rent$Age)
Rent$cen_Expenses = Rent$Expenses - mean(Rent$Expenses)

# Then we fit the model
fit<-lm(formula = RentalRate ~ Size + cen_Age + cen_Expenses + cen_Age *
     cen_Expenses, data = Rent)
summary(fit)
coefficients(fit)

# Then we try a version with the interaction terms removed
fit2 <- lm(RentalRate~ Size + cen_Age + cen_Expenses, data=Rent)
summary(fit2)

# Then we can compare the two
anova(fit, fit2)

# Categorical Data - Teen Gambling study
pairs(teengamb)

# Then we fit a model taking interaction terms into account
Gamfit = lm(gamble ~ sex + status + income +verbal
            + sex*status + sex*verbal + sex*income,data=teengamb)
summary(Gamfit)

# Then we use backwards eliminations to find the best model
fitnull= lm(gamble ~ sex,data=teengamb )
fitfull = lm(gamble ~ sex + status + income +verbal + sex*status + sex*verbal + sex*income,data=teengamb)
step(fitfull, scope=list(lower=fitnull, upper=fitfull), direction="backward")

fit3<-lm(formula = gamble ~ sex + income + verbal + sex:income, data = teengamb)

# Then we can plot the best one
plot(teengamb$gamble)
lines(fitted(fit3))









