# Preprocessing steps in model selection

# Load libraries
import numpy as np
from sklearn import datasets
from sklearn.feature_selection import SelectKBest
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

# Set random seed
np.random.seed(0)
 
# Load data
iris = datasets.load_iris()
X = iris.data
y = iris.target
#Create Proprocessing Object
#We are include two different preprocessing steps: principal component analysis and a k-best feature selection.

# Create a combined preprocessing object
preprocess = FeatureUnion([('pca', PCA()), ("kbest", SelectKBest(k=1))])
 
# Create a pipeline
pipe = Pipeline([('preprocess', preprocess), ('classifier', LogisticRegression())])
 
# Create space of candidate values
search_space = [{'preprocess__pca__n_components': [1, 2, 3],
                 'classifier__penalty': ['l1', 'l2'],
                 'classifier__C': np.logspace(0, 4, 10)}]
 
# Create grid search 
clf = GridSearchCV(pipe, search_space, cv=5, verbose=0, n_jobs=-1)
 
# Fit grid search
best_model = clf.fit(X, y)
 
# View best hyperparameters
print('Best Number Of Princpal Components:', best_model.best_estimator_.get_params()['preprocess__pca__n_components'])
print('Best Penalty:', best_model.best_estimator_.get_params()['classifier__penalty'])
print('Best C:', best_model.best_estimator_.get_params()['classifier__C'])