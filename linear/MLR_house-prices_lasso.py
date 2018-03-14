import pandas
import numpy as np
from sklearn.linear_model import Lasso, LinearRegression
from sklearn import cross_validation
from sklearn import grid_search
from sklearn.ensemble import ExtraTreesRegressor
import statsmodels.api as sm
float_formatter = lambda x: "%.2f" % x
np.set_printoptions(formatter={'float_kind':float_formatter})
import sklearn.feature_selection as f

######################################################################################

g = pandas.read_csv("C:/Udemy/Pro Data Science in Python/HousePricesKingCounty/kc_house_data.csv",encoding = "ISO-8859-1")
g["price"]    = g["price"]/1000



X               = g[["sqft_above","sqft_basement","sqft_lot","sqft_living","floors","bedrooms",
                     "yr_built","lat","long","bathrooms"]].values
Y               = g["price"].values
zipcodes        = pandas.get_dummies(g["zipcode"]).values
condition       = pandas.get_dummies(g["condition"]).values
grade           = pandas.get_dummies(g["grade"]).values
X               = np.concatenate((X,zipcodes),axis=1)
X               = np.concatenate((X,condition),axis=1)
X               = np.concatenate((X,grade),axis=1)

#######################################################################

model = sm.OLS(g["price"],X)
results = model.fit()
print(results.summary())

#######################################################################
 
clf            = LinearRegression()
clf.fit(X, g["price"].values)
scores = cross_validation.cross_val_score(clf,X , g["price"].values, cv=3)
print("Linear Regression Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(clf.coef_)
print("LinearRegression # coeffs :" + str(clf.coef_.shape[0]))

#######################################################################

clf            = Lasso(max_iter = 100000000)
clf.fit(X, g["price"].values)
scores = cross_validation.cross_val_score(clf,X , g["price"].values, cv=3)
print("Lasso Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
print(clf.coef_)
print("Lasso # coeffs :" + str(clf.coef_[clf.coef_>0].shape[0]))

######################################################################

clf            = ExtraTreesRegressor()
parameters     = {'max_depth':np.arange(1,15)}
clfgrid        = grid_search.GridSearchCV(clf, parameters)
clfgrid.fit(X, g["price"].values)
scores = cross_validation.cross_val_score(clf,X , g["price"].values, cv=3)
print("Extratrees Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))

#######################################################################################
