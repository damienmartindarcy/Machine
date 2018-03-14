# STAT 40800 - Student 15202834 - Project / Strand 1

# Import Libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sklearn
from sklearn.cross_validation import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn.metrics import accuracy_score
from sklearn.ensemble import RandomForestClassifier
from imblearn.under_sampling import ClusterCentroids 
from imblearn.under_sampling import RandomUnderSampler #undersampling
from imblearn.over_sampling import SMOTE 
from imblearn.over_sampling import RandomOverSampler  #oversampling
from imblearn.combine import SMOTEENN
from sklearn.metrics import roc_curve, auc

#Data Cleaning / Description

card=pd.read_csv('C:\Users\ubacwa4\Desktop\Data_Fraud.csv')
card.head()
card.describe()

# How imbalanced is the dataset?
card['Class'].value_counts()

# Check for missing values
card.isnull().values.any()

# Simple correlation
columns = ['V1', 'V2', 'V3', 'V4', 'V5', 'V6', 'V7', 'V8', 'V9', 'V10',
       'V11', 'V12', 'V13', 'V14', 'V15', 'V16', 'V17', 'V18', 'V19', 'V20',
       'V21', 'V22', 'V23', 'V24', 'V25', 'V26', 'V27', 'V28',
       'Class', 'Time', 'Amount']
correlation = card[columns].corr(method='pearson')

# Test and Training 

card.drop(['Time','Amount'], axis=1, inplace=True)

Y = card['Class']
X = card.drop("Class",axis=1)

# Usoing test_train_split for test and training sets - 70/30 split. Random number generator
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.3, random_state=123)

# Check the relative number of observations

print(X_train.shape, X_test.shape)
print(Y_train.shape, Y_test.shape)

#Random Forest - Attempt 1

rfc = RandomForestClassifier()
rfc.fit(X_train,Y_train)
Y_pred = rfc.predict(X_test) 

# Show classification report, confusion matrix, accuracy score
print(classification_report(Y_pred,Y_test))
print(confusion_matrix(Y_test, Y_pred))
print(accuracy_score(Y_test,Y_pred))

#Parameter Tuning - Random Forest - Attempt 2

# Weighting in favour of the minority class in an attempt to improve accuracy 
rfc = RandomForestClassifier(class_weight={0:0.001,1:0.999})
rfc.fit(X_train,Y_train)
Y_pred = rfc.predict(X_test) 

print(classification_report(Y_pred,Y_test))
print(confusion_matrix(Y_test, Y_pred))
print(accuracy_score(Y_test,Y_pred))

#Random Re-Sampling (under-sampling)

rus = RandomUnderSampler()
rux, ruy = rus.fit_sample(X_train, Y_train)

# Sample counts following under-sampling 
unique, counts = np.unique(ruy, return_counts=True)
print (np.asarray((unique, counts)).T)

# Apply re-sampled data to RFC
rfc = RandomForestClassifier()
rfc.fit(rux,ruy)
Y_rus_pred = rfc.predict(X_test) 

# Performance not so good on minority class
print(classification_report(Y_rus_pred,Y_test))
print(confusion_matrix(Y_test, Y_rus_pred))
print(accuracy_score(Y_test,Y_rus_pred))

#Under-sampling via ClusterCentroids

CC = ClusterCentroids()
ccx, ccy = CC.fit_sample(X_train, Y_train)

# Again - size / shape of resampled data
unique, counts = np.unique(ccy, return_counts=True)
print (np.asarray((unique, counts)).T)

# It takes a long time to run ....
rfc = RandomForestClassifier()
rfc.fit(ccx,ccy)
Y_cc_pred = rfc.predict(X_test) 

print(classification_report(Y_cc_pred,Y_test))
print(confusion_matrix(Y_test, Y_cc_pred))
print(accuracy_score(Y_test,Y_cc_pred))

#Random over-sampling - with replacement
ros = RandomOverSampler()
rox, roy = ros.fit_sample(X_train, Y_train)

# What is the size of the re-sampled dataset?
unique, counts = np.unique(roy, return_counts=True)
print (np.asarray((unique, counts)).T)

rfc = RandomForestClassifier()
rfc.fit(rox,roy)
Y_ros_pred = rfc.predict(X_test) 

print(classification_report(Y_ros_pred,Y_test))
print(confusion_matrix(Y_test, Y_ros_pred))
print(accuracy_score(Y_test,Y_ros_pred))

#SMOTE / SMOTEENN

#SMOTE
smote = SMOTE(ratio='auto', kind='regular')
smox, smoy = smote.fit_sample(X_train, Y_train)

unique_smote, counts_smote = np.unique(smoy, return_counts=True)
print (np.asarray((unique_smote, counts_smote)).T)

rfc.fit(smox,smoy)
y_smote_pred = rfc.predict(X_test) 
print(classification_report(y_smote_pred,Y_test))
print(confusion_matrix(Y_test, y_smote_pred))
print(accuracy_score(Y_test,y_smote_pred))

#SMOTEENN

# This has an additional data cleaning step to remove outliers
SENN = SMOTEENN(ratio = 'auto')
ennx, enny = SENN.fit_sample(X_train, Y_train)
unique_enny, counts_enny = np.unique(enny, return_counts=True)
print (np.asarray((unique_enny, counts_enny)).T)

rfc.fit(ennx, enny)
y_senn_pred = rfc.predict(X_test) 
print(classification_report(y_senn_pred,Y_test))
print(confusion_matrix(Y_test, y_senn_pred))
print(accuracy_score(Y_test,y_senn_pred))


#Finally - ROC / AUC for the three over-sampling approaches

false_positive_rate, true_positive_rate, thresholds = roc_curve(Y_test,Y_ros_pred)
roc_auc=auc(false_positive_rate, true_positive_rate)
print(roc_auc)

false_positive_rate, true_positive_rate, thresholds = roc_curve(Y_test,y_smote_pred)
roc_auc=auc(false_positive_rate, true_positive_rate)
print(roc_auc)

false_positive_rate, true_positive_rate, thresholds = roc_curve(Y_test,y_senn_pred)
roc_auc=auc(false_positive_rate, true_positive_rate)
print(roc_auc)

# Plot of the SMOTEENN version

plt.title('Receiver Operating Characteristic')
plt.plot(false_positive_rate, true_positive_rate, 'b', label='AUC=%0.2f'% roc_auc)
plt.legend(loc='lower right')
plt.plot([0,1],[0,1],'r--')
plt.xlim([-0.1,1.2])
plt.ylim([-0.1,1.2])
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')
plt.show()



