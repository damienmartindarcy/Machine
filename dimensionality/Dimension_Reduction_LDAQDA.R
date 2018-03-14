

##############################################################################

# Question 3 - Pima Data

# Initial Analysis
library(plyr)
library(mvtnorm)
Pima=data.frame(Pima)
head(Pima)
summary(Pima)
count(Pima, "type")

# model 1 - no validation

qda.fit1=qda(type~.,Pima)
qda.fit1$counts

qda.class1 <- predict(qda.fit1, Pima)$class
table(qda.class1)
table(Pima$type)
table(qda.class1, Pima$type) 

#This suggests that QDA predictions are accurate ~ 77% of the time
mean(qda.class1 == Pima$type)

# model 2 - Training and test data

# Training and Test split
require(caTools)
set.seed(101) 
sample = sample.split(Pima$type, SplitRatio = .80)
train = subset(Pima, sample == TRUE)
test = subset(Pima, sample == FALSE)
count(train, "type")
count(test, "type")

# Training
qda.fit2=qda(type~.,train)
qda.fit2$counts

# Test
qda.class2 <- predict(qda.fit2, test)$class
table(qda.class2)
table(qda.class2, test$type) 

#This suggests that QDA predictions are accurate ~ 76% of the time on the test set
mean(qda.class2 == test$type)

# Applied to entire set
qda.class3 <- predict(qda.fit2, Pima)$class
table(qda.class3)
table(qda.class3, Pima$type) 

# The mean is slightly lower than the original (77.2% vs 77.6%)
mean(qda.class3 == Pima$type)

# Model 3 - Model with Cross Validation

qda.fit3<-qda(type~., data=Pima,  CV=TRUE)
qda.fit3$posterior
qda.fit3$class

table(Pima$type, qda.fit3$class)
mean(Pima$type==qda.fit3$class) # Lowest of the 3 - about 75%

# Q3A - New Prediction - Main two models

qda.fit1
qda.fit2

predict(qda.fit1,newdata=data.frame(npreg=7,glu=187,bp=50,skin=33,bmi=33.9,ped=0.826,age=30))
predict(qda.fit2,newdata=data.frame(npreg=7,glu=187,bp=50,skin=33,bmi=33.9,ped=0.826,age=30))

# (Attempted) Derivation of QDA Function for this record

dq<-NULL

for(i in 1:nlevels(Pima$type))
  dq <- cbind(dq, dmvnorm(x= cbind(Pima$npreg, Pima$glu, Pima$bp, Pima$skin, Pima$bmi, Pima$ped, Pima$age),
                          mean=qda.fit1$means[,i], sigma=solve(qda.fit1$scaling[,,i]%*%t(qda.fit1$scaling[,,i])),log=TRUE)+log(p[i]))

# Q3B -  Applied to test set of five observations

# Records assigned to csv file
head(PimaTest)
predict(qda.fit1, PimaTest)# Mislcassifies observation #3 - so 20% misclassified
predict(qda.fit2, PimaTest) # Classifies all correctly


 

