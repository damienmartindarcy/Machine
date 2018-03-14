# Multiple Linear Regression - Stepwise

# Based on a subset of the commercial property dataset. 
# Variables are the age of the property (X1), operating expenses and taxes (X2), vacancy rates(X3), 
# total square footage (X4) and rental rates (Y )
# We want to predict the rental rate as a function of the other variables

# Data Import
Age=c(13.5,12,10.5,15,14,10.5,14,16.5,17.5,16.5,17,16.5,16,16.5,17.225,17,16,14.625,14.5,14.5) 
Expenses=c(1,14,16,4,11,15,2,1,1,8,12,2,2,13,2,1,1,12,16,3)
VacancyRate=c(5.02,8.19,3,10.7,8.97,9.45,8,6.62,5.2,11.78,4.62,11.55,9.63,12.99,12.01,12.01,7.99,10.33,10.67,9.45) 
Size=c(0.14,0.27,0,0.05,0.07,0.24,0.19,0.6,0,0.03,0.08,0.03,0,0.04,0.03,0,0.14,0.12,0,0.03) 
RentalRate=c(123000,104079,39998,57112,60000,101385,31300,248172,215000,251015,291264,207549,82000,359665,265500,299000,189258,366013,349930,85335)

# Convert to data frame
RentData=data.frame(Age,Expenses, VacancyRate, Size, RentalRate)

# Use all explanatory variables
fitfull <- lm(RentalRate~ Age + Expenses + VacancyRate+ Size ,data=RentData)
summary(fitfull)

# Stepwise Regression
fitnull <- lm(RentalRate~ 1 ,data=RentData)
fitfull <- lm(RentalRate~ Age + Expenses + VacancyRate + Size ,data=RentData)
step(fitnull, scope=list(lower=fitnull, upper=fitfull), direction="forward")