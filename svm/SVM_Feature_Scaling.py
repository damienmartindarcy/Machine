import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
 
# Importing the dataset
dataset=pd.read_csv('Position_Salaries.csv')
X = dataset.iloc[:, 1:2].values
Y = dataset.iloc[:, 2].values
 
# Feature Scaling - we need scaling for SVR
from sklearn.preprocessing import StandardScaler
sc_X = StandardScaler()
sc_Y = StandardScaler()
X = sc_X.fit_transform(X)
Y = sc_Y.fit_transform(Y)
 
# Fitting SVR to the dataset
from sklearn.svm import SVR
regressor=SVR(kernel = 'rbf')
regressor.fit(X, Y)
 
# Predicting a new result - the employee at level 6.5 has an expected salary of 170370
Y_pred = sc_Y.inverse_transform(regressor.predict(sc_X.transform(np.array([[6.5]]))))
 
# Visualising the SVR results - the red points are the real and the blue line is the predicted value
plt.scatter(X, Y, color='red')
plt.plot (X, regressor.predict(X), color='blue')
plt.title ('Position and Salary (SVR)')
plt.xlabel('Position Level')
plt.ylabel('Salary')
plt.show()