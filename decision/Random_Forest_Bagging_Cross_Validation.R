# 1 - Loading

Physio<-read.table("Physio.txt")

Physio[is.na(Physio)] <- 0
summary(Physio)
is.factor(Physio$assigned.labels)
Physio$assigned.labels <- as.factor(Physio$assigned.labels)
mode(Physio$assigned.labels)

mode(Physio)

# 2 - Classification - Attempt 1

#Load the rpart and partykit libraries
library(rpart)
library(partykit)

#Use a classification tree to classify the data - Classification 1
fit.r <- rpart(assigned.labels~.,data=Physio)
plot(as.party(fit.r))

# Classify the observations
pred <- predict(fit.r,type="class")
pred

# Look at table (rows=truth, cols=prediction)
tab <- table(Physio$assigned.labels,pred)
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)


# 3 - Using training and test data - Classification Attempt 2

#Set seed of random number generator
set.seed(1000)


#Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Split the data
#75% of values are used as training data
# The remaining 25% are used as test data

N <- nrow(Physio)
indtrain <- sample(1:N,size=0.75*N)
indtrain <- sort(indtrain)
indtest <- setdiff(1:N,indtrain)

#Fit a classifier to only the training data
fit.r <- rpart(assigned.labels~.,data=Physio,subset=indtrain)
plot(as.party(fit.r))

# Classify for ALL of the observations
pred <- predict(fit.r,type="class",newdata=Physio)

#Look at the results for the test data only
pred[indtest]

# Look at table for the test data only (rows=truth, cols=prediction)
tab <- table(Physio$assigned.labels[indtest],pred[indtest])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)

#Look at the results for the training data only
tab <- table(Physio$assigned.labels[indtrain],pred[indtrain])
tab

# Work out the accuracy
sum(diag(tab))/sum(tab)

# Let's repeat this process to see if we were just unlucky!

#Set up res to store results

res<-matrix(NA,100,2)

# Start simulation to look at this 
iterlim <- 100
for (iter in 1:iterlim)
{
  
  # Split the data
  # 75% of values are used as training data
  # The remaining 25% are used as test data
  N <- nrow(Physio)
  indtrain <- sample(1:N,size=0.75*N)
  indtrain <- sort(indtrain)
  indtest <- setdiff(1:N,indtrain)
  
  # Fit a classifier to the training data only
  fit.r <- rpart(assigned.labels~.,data=Physio,subset=indtrain)
  
  # Classify for ALL of the observations
  pred <- predict(fit.r,type="class",newdata=Physio)
  
  # Look at table for the test data only (rows=truth, cols=prediction)
  tab <- table(Physio$assigned.labels[indtest],pred[indtest])
  
  # Work out the accuracy
  res[iter,1]<-sum(diag(tab))/sum(tab)
  
  #Look at the results for the training data only
  tab <- table(Physio$assigned.labels[indtrain],pred[indtrain])
  
  # Work out the accuracy
  res[iter,2] <- sum(diag(tab))/sum(tab)
  
} 


#Check out the error rate summary statistics.
colnames(res)<-c("test","train")
apply(res,2,summary)

# 4 - Comparative classifier performance / tvt
# Comparing classifiers using training, validation and test - Classification 3

#Set seed of random number generator
set.seed(1000)


#Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Sample 50% of the data as training data
# Sample 25% of the data as validation 
# Let the remaining 25% data be test data

N <- nrow(Physio)
indtrain <- sample(1:N,size=0.50*N,replace=FALSE)
indtrain <- sort(indtrain)
indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N)
indvalid <- sort(indvalid)
indtest <- setdiff(1:N,union(indtrain,indvalid))

# Fit a classifier to only the training data
fit.r <- rpart(assigned.labels~,data=Physio,subset=indtrain)
plot(as.party(fit.r))

# Fit a logistic regression to the training data only too
# First load the nnet package
library(nnet)
fit.l <- multinom(assigned.labels~., data=Physio,subset=indtrain)

# Classify for ALL of the observations
pred.r <- predict(fit.r,type="class",newdata=Physio)
pred.l <- predict(fit.l,type="class",newdata=Physio)

# Look at table for the validation data only (rows=truth, cols=prediction)
tab.r <- table(Physio$assigned.labels[indvalid],pred.r[indvalid])
tab.r
tab.l <- table(Physio$assigned.labels[indvalid],pred.l[indvalid])
tab.l

# Work out the accuracy
acc.r <- sum(diag(tab.r))/sum(tab.r)
acc.l <- sum(diag(tab.l))/sum(tab.l)

acc.r
acc.l

#Look at the method that did best on the validation data 
#when applied to the test data
if (acc.r>acc.l)
{
  tab <- table(Physio$assigned.labels[indtest],pred.r[indtest])
}else
{
  tab <- table(Physio$assigned.labels[indtest],pred.l[indtest])
}

tab

sum(diag(tab))/sum(tab)

# Let's repeat this process to see if we were just unlucky!

#Set up res to store results

res<-matrix(NA,100,4)

# Start simulation to look at this 
iterlim <- 100
for (iter in 1:iterlim)
{
  # Sample 50% of the data as training data
  # Sample 25% of the data as validation 
  # Let the remaining 25% data be test data
  
  N <- nrow(Physio)
  indtrain <- sample(1:N,size=0.50*N,replace=FALSE)
  indtrain <- sort(indtrain)
  indvalid <- sample(setdiff(1:N,indtrain),size=0.25*N)
  indvalid <- sort(indvalid)
  indtest <- setdiff(1:N,union(indtrain,indvalid))
  
  # Fit a classifier to only the training data
  fit.r <- rpart(assigned.labels~.,data=Physio,subset=indtrain)
  
  # Fit a logistic regression to the training data only too
  fit.l <- multinom(assigned.labels~., data=Physio,subset=indtrain)
  
  # Classify for ALL of the observations
  pred.r <- predict(fit.r,type="class",newdata=Physio)
  pred.l <- predict(fit.l,type="class",newdata=Physio)
  
  # Look at table for the validation data only (rows=truth, cols=prediction)
  tab.r <- table(Physio$assigned.labels[indvalid],pred.r[indvalid])
  tab.l <- table(Physio$assigned.labels[indvalid],pred.l[indvalid])
  
  # Work out the accuracy
  acc.r <- sum(diag(tab.r))/sum(tab.r)
  acc.l <- sum(diag(tab.l))/sum(tab.l)
  
  # Store the results
  res[iter,1] <- acc.r
  res[iter,2] <- acc.l
  
  # Look at the method that did best on the validation data 
  # when applied to the test data - iteration to convergence
  if (acc.r>acc.l)
  {
    tab <- table(Physio$assigned.labels[indtest],pred.r[indtest])
    acc <- sum(diag(tab))/sum(tab)
    res[iter,3] <- 1
    res[iter,4] <- acc
  }else
  {
    tab <- table(Physio$assigned.labels[indtest],pred.l[indtest])
    acc <- sum(diag(tab))/sum(tab)
    res[iter,3] <- 2
    res[iter,4] <- acc
  }
  
} 


# Check out the error rate summary statistics.
colnames(res)<-c("valid.r","valid.l","chosen","test")
apply(res,2,summary)
table(res[,3])

# 5 - Bagging / Random Forest
# Bagging / Random Forest - Classification  

# Set random number seed
set.seed(1)

# Load many required packages
library(rpart)
library(adabag)
library(randomForest)
library(partykit)

# Split data into training and test
N<-nrow(Physio)
indtrain<-sample(1:N,size=0.75*N)
indtrain<-sort(indtrain)
indtest<-setdiff(1:N,indtrain)

# Look at performance of classification trees
fit.r <- rpart(assigned.labels~.,data=Physio,subset=indtrain)
plot(as.party(fit.r))
pred.r <- predict(fit.r,newdata=Physio,type="class")

# Test data
table(Physio$assigned.labels[indtest],pred.r[indtest])
sum(Physio$assigned.labels[indtest]==pred.r[indtest])/length(indtest)

# Training data
table(Physio$assigned.labels[indtrain],pred.r[indtrain])
sum(Physio$assigned.labels[indtrain]==pred.r[indtrain])/length(indtrain)

# Look at bagging classification trees
fit.b <- bagging(assigned.labels~.,data=Physio[indtrain,])
pred.b <- predict(fit.b,newdata=Physio,type="class")$class

# Test data
table(Physio$assigned.labels[indtest],pred.b[indtest])
sum(Physio$assigned.labels[indtest]==pred.b[indtest])/length(indtest)

# Training data

table(Physio$assigned.labels[indtrain],pred.b[indtrain])
sum(Physio$assigned.labels[indtrain]==pred.b[indtrain])/length(indtrain)

# Look at random forest classifier
fit.rf <- randomForest(assigned.labels~.,data=Physio,subset=indtrain)
pred.rf <- predict(fit.rf,newdata=Physio,type="class")

# Test data
table(Physio$assigned.labels[indtest],pred.rf[indtest])
sum(Physio$assigned.labels[indtest]==pred.rf[indtest])/length(indtest)

# Training data
table(Physio$assigned.labels[indtrain],pred.rf[indtrain])
sum(Physio$assigned.labels[indtrain]==pred.rf[indtrain])/length(indtrain)

# 6 - Using k-fold cross validation - Classification  

# Set seed of random number generator
set.seed(1000)


N <- nrow(Physio)

# Load the rpart and partykit libraries
library(rpart)
library(partykit)

# Fit a classifier to all of the training data
fit.r <- rpart(assigned.labels~.,data=Physio)
plot(as.party(fit.r))

# Let's do some k-fold cross validation

# First, let's assign the observations to folds.
K <- 10

folds <- rep(1:K,ceiling(N/K))
folds <- sample(folds) 
folds <- folds[1:N]

# Set up res to store results

res<-matrix(NA,K,1)

# We will need to drop each fold in turn.
iterlim <- K
for (iter in 1:iterlim)
{
  indtrain <- (1:N)[!(folds==iter)]
  indtest <- setdiff(1:N,indtrain)
  
  # Fit a classifier to only the training data
  fit.r <- rpart(assigned.labels~.,data=Physio,subset=indtrain)
  
  # Classify for ALL of the observations
  pred.r <- predict(fit.r,type="class",newdata=Physio)
  
  # Look at table for the validation data only (rows=truth, cols=prediction)
  tab.r <- table(Physio$assigned.labels[indtest],pred.r[indtest])
  
  # Let's see how well we did on the fold that we dropped
  res[iter,1] <- sum(diag(tab.r))/sum(tab.r)
  
} #iter


#Check out the error rate summary statistics.
colnames(res)<-c("test")
apply(res,2,summary)
res
table(folds)


