# ADV - Lab 5

# Run the model again

fitfull <- lm(RentalRate~ Age + Size + Expenses
              + VacancyRate ,data=Rent)

summary(fitfull)

# Then the anova table
anova(fitfull)

# Compute the following in R for the full model: coefficients, 95% confidence intervals 
# for model parameters, predicted values and residuals

coefficients(fitfull)

confint(fitfull, level=0.95) 

fitted(fitfull) # Predicted Values

residuals(fitfull)


