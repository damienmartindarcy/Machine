# Model Selection - Grid search

# Load libraries
import numpy as np
from sklearn import datasets
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline

# Set random seed
np.random.seed(0)

# Load data
iris = datasets.load_iris()
X = iris.data
y = iris.target

#Create Pipeline With Model Selection Search Space
#Notice that we include both multiple possible learning algorithms and multiple possible hyperparameter values to search over.

# Create a pipeline
pipe = Pipeline([('classifier', RandomForestClassifier())])

# Create space of candidate learning algorithms and their hyperparameters
search_space = [{'classifier': [LogisticRegression()],
                 'classifier__penalty': ['l1', 'l2'],
                 'classifier__C': np.logspace(0, 4, 10)},
                {'classifier': [RandomForestClassifier()],
                 'classifier__n_estimators': [10, 100, 1000],
                 'classifier__max_features': [1, 2, 3]}]

# Create grid search 
clf = GridSearchCV(pipe, search_space, cv=5, verbose=0)

# Fit grid search
best_model = clf.fit(X, y)

# View best model
best_model.best_estimator_.get_params()['classifier']

# Predict target vector
best_model.predict(X)