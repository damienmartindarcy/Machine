# Polynomial model

Steroid=c(27.1,22.1,21.9,10.7,1.4,18.8,14.7,5.7,18.6,20.4,9.2,23.4,10.5,19.7,11.8,24.6,3.4,22.8,21.1,24,21.8,23.5,19.4,25.6,12.8,20.8,20.6)
Age=c(23,19,25,12,8,12,11,8,17,18,9,21,10,25,9,17,9,23,13,14,16,17,21,24,13,14,18)
SteroidData=data.frame(Steroid,Age)

pairs(SteroidData) # Clearly a non linear relationship between these

# Data Centring
mean(SteroidData$Age)
SteroidData$cen_Age = SteroidData$Age - mean(SteroidData$Age) # Centred version of age

reg = lm(Steroid ~ cen_Age + I(cen_Age^2) + I(cen_Age^3),data=SteroidData)
summary(reg) # cubic polynomial model