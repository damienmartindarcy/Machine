# ADV - Week 4 - Correlation and Regression

# Correlation
# We start with a scatterplot
pairs(Rent)
cor(Rent)

# Regression
# Multiple regression model using all exaplanatory variables
fitfull<-lm(RentalRate ~ Age + Expenses + VacancyRate + Size, data=Rent)
summary(fitfull)

# Stepwise regression to fit multiple models simultaneously
fitnull<-lm(RentalRate~1, data=Rent)
            
step(fitnull, scope=list(lower=fitnull, upper=fitfull), direction="forward")

# Polynomial model examples from the lecture notes

# Linear Model
linear<-lm(formula=Geiger$Counts~Geiger$Time)
summary(linear)

# Quadratic Model
quad<-lm(formula=Geiger$Counts~Geiger$Time+I(Geiger$Time^2))
summary(quad)

# Cubic Model
cubic<-lm(formula=Geiger$Counts~Geiger$Time+I(Geiger$Time^2)+I(Geiger$Time^3))
summary(cubic)

# Categorical Data - Cargo Example

# We need to create the factor variable
# creating the factor variable
Cargo$Type.f <- factor(Cargo$Type)

reg1 <- lm(Cost ~ Cargo$Type.f, data=Cargo)
summary(reg1)

# Interactions - Bidding Example

head(Bidding)
reg2 <- lm(Price ~ Size + Bidders +
             Size*Bidders, data=Bidding)
summary(reg2)



