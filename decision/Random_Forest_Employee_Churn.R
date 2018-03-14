# People Analytics - Predicting employee churn / turnover

# Load an R data frame.
MFG10YearTerminationData <- read.csv("MFG10YearTerminationData.csv")
MYdataset <- MFG10YearTerminationData
str(MYdataset)

library(plyr)
library(dplyr)

# Second Look

summary(MYdataset)

##########################################################################
# EDA
# Question by question - what proportion of the staff are levaing?

StatusCount<- as.data.frame.matrix(MYdataset %>%
                                     group_by(STATUS_YEAR) %>%
                                     select(STATUS) %>%
                                     table())

StatusCount$TOTAL<-StatusCount$ACTIVE + StatusCount$TERMINATED
StatusCount$PercentTerminated <-StatusCount$TERMINATED/(StatusCount$TOTAL)*100
StatusCount

mean(StatusCount$PercentTerminated)

# Churn - by business unit

library(ggplot2)
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(BUSINESS_UNIT),fill = as.factor(STATUS)),data=MYdataset,position = position_stack())

# By termination type and status

TerminatesData<- as.data.frame(MYdataset %>%
                                 filter(STATUS=="TERMINATED"))
ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),fill = as.factor(termtype_desc)),data=TerminatesData,position = position_stack())

# By status year and termination reason

ggplot() + geom_bar(aes(y = ..count..,x =as.factor(STATUS_YEAR),fill = as.factor(termreason_desc)),data=TerminatesData,position = position_stack())

# By termination reason and department

ggplot() + geom_bar(aes(y = ..count..,x =as.factor(department_name),fill = as.factor(termreason_desc)),data=TerminatesData,position = position_stack())+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# Role of length of service and age in terminations

library(caret)
featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="density",auto.key = list(columns = 2))

# Age and length of service distributions by status
featurePlot(x=MYdataset[,6:7],y=MYdataset$STATUS,plot="box",auto.key = list(columns = 2))

#################################################################

# Modeling

library(rattle)   
library(magrittr) # For the %>% and %<>% operators.

# Training and testing datasets
set.seed(crv$seed) 
MYnobs=nrow(MYdataset) # 52692 observations 
MYtrain=subset(MYdataset,STATUS_YEAR<=2014)
MYvalidate=NULL
MYtest=subset(MYdataset,STATUS_YEAR== 2015)

MYinput=c("age", "length_of_service",    "gender_full", "STATUS_YEAR", "BUSINESS_UNIT")

MYnumeric=c("age", "length_of_service", "STATUS_YEAR")

MYcategoric=c("gender_full", "BUSINESS_UNIT")

MYtarget="STATUS"
MYrisk=NULL
MYident="EmployeeID"
MYignore=c("recorddate_key", "birthdate_key", "orighiredate_key", "terminationdate_key", "city_name", "gender_short", "termreason_desc", "termtype_desc","department_name","job_title", "store_name")
MYweights=NULL

MYTrainingData<-MYtrain[c(MYinput, MYtarget)]
MYTestingData<-MYtest[c(MYinput, MYtarget)]

# Decision Tree

library(rattle)
library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model / then plot

MYrpart<-rpart(STATUS ~ .,data=MYtrain[, c(MYinput, MYtarget)],method="class",
              parms=list(split="information"),
              control=rpart.control(usesurrogate=0, 
                                    maxsurrogate=0))

fancyRpartPlot(MYrpart, main="Decision Tree MFG10YearTerminationData $ STATUS")

# Random Forest

library(randomForest, quietly=TRUE)
library(pROC)

set.seed(crv$seed)
MYrf<-randomForest(STATUS ~ .,data=MYtrain[c(MYinput, MYtarget)],
                  ntree=500,
                  mtry=2,
                  importance=TRUE,
                  na.action=randomForest::na.roughfix,
                  replace=FALSE)

# Textual 
MYrf

# Calculate AUC
pROC::roc(MYrf$y, as.numeric(MYrf$predicted))

# Calculate AUC CI
pROC::ci.auc(MYrf$y, as.numeric(MYrf$predicted))

# List the importance of the variables
rn<-round(randomForest::importance(MYrf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Ada Boost Model
library(ada)
set.seed(crv$seed)
MYada<-ada(STATUS ~ .,
          data=MYtrain[c(MYinput, MYtarget)], control=rpart::rpart.control(maxdepth=30,
                                       cp=0.010000,
                                       minsplit=20,
                                       xval=10),iter=50)

# Results
print(MYada)

round(MYada$model$errs[MYada$iter,], 2)

cat('Variables actually used in tree construction:\n')

print(sort(names(listAdaVarsUsed(MYada))))

cat('\nFrequency of variables actually used:\n')

print(listAdaVarsUsed(MYada))

 
# SVM

library(kernlab, quietly=TRUE)

set.seed(crv$seed)
MYksvm<-ksvm(as.factor(STATUS) ~ .,
            data=MYtrain[c(MYinput, MYtarget)],
            kernel="rbfdot",
            prob.model=TRUE)

MYksvm

# Finally, linear regression

MYglm<-glm(STATUS ~ .,
          data=MYtrain[c(MYinput, MYtarget)],
          family=binomial(link="logit"))

print(summary(MYglm))

print(anova(MYglm, test="Chisq"))

#################################################################

# Model Evaluation - Confusion matrix counts to begin with

# DT
MYpr<-predict(MYrpart, newdata=MYtest[c(MYinput, MYtarget)], type="class")

# Confusion Matrix - counts
table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))

# ada boost
MYpr<-predict(MYada, newdata=MYtest[c(MYinput, MYtarget)])

table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
      dnn=c("Actual", "Predicted"))

# rf
MYpr<-predict(MYrf, newdata=na.omit(MYtest[c(MYinput, MYtarget)]))

table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
dnn=c("Actual", "Predicted"))

# SVM
MYpr<-predict(MYksvm, newdata=na.omit(MYtest[c(MYinput, MYtarget)]))

table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
dnn=c("Actual", "Predicted"))

# Linear Regression
MYpr<-as.vector(ifelse(predict(MYglm, type=green: "response", newdata=MYtest[c(MYinput, MYtarget)]) > 0.5, green: "TERMINATED", "ACTIVE"))

table(MYtest[c(MYinput, MYtarget)]$STATUS, MYpr,
dnn=c("Actual", "Predicted"))


# ada boost has highest accuracy - linear regression the lowest

##################################################################
# Then predict 2016 terminations

Employees2016<-MYtest #2015 data

ActiveEmployees2016<-subset(Employees2016,STATUS=='ACTIVE')
ActiveEmployees2016$age<-ActiveEmployees2016$age+1
ActiveEmployees2016$length_of_service<-ActiveEmployees2016$length_of_service+1

#Predict 2016 Terminates using adaboost
ActiveEmployees2016$PredictedSTATUS2016<-predict(MYada,ActiveEmployees2016)
PredictedTerminatedEmployees2016<-subset(ActiveEmployees2016,PredictedSTATUS2016=='TERMINATED')
#show records for first 5 predictions
head(PredictedTerminatedEmployees2016)




