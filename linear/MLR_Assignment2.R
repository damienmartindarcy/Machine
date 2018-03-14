# ADV - Assignment 2 - Census Data

# Initial versions

# Drop the 'State' column
Census <- Census[ -c(1) ]

# Pairs and correlation
is.na(Census)
head(Census)
summary(Census)
pairs(Census)
round(cor(Census), 2)

# Fit all variables to the model
fitfull <- lm(Exp ~ Pop+Inc+Ill+Mur+Hsg+Fro+Are,data=Census)
summary(fitfull)
summary.aov(fitfull)

# Other details
residuals(fitfull)
anova(fitfull)

# Step Model  - final model here is exp ~ Pop + Mur + Hsg + Fro
fitnull <- lm(Exp~ 1 ,data=Census)
fitfull <- lm(Exp ~ Pop+Inc+Ill+Mur+Hsg+Fro+Are ,data=Census)
step(fitfull, scope=list(lower=fitnull, upper=fitfull), direction="backward", test="F")

# AIC only
step <- stepAIC(fitfull, direction="backward")
step$anova

# Final model with Pop Dropped - exp ~ Mur + Hsg + Fro
Final<-lm(Exp ~ Mur+Hsg+Fro ,data=Census)
summary(Final)
anova(Final)

# Compare AIC's for the two models
AIC(step, Final)

# Diagnostics
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(Final)
residuals(Final)

# Outliers
plot(Census$Exp)
lines(fitted(fitfinal))
points(10, Census$Exp[10], col = "red")
points(18, Census$Exp[18], col = "red")
points(39, Census$Exp[39], col = "red")

# Prediction
# Wrap the parameters in a dataframe
newdata = data.frame(Mur=6.8, Hsg=63.9, Fro=166)
# Apply to the original model - 95%
predict(Final, newdata, interval="predict")





