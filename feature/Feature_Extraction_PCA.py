# Feature Extraction - PCA

# Import packages
import numpy as np
from sklearn import decomposition, datasets
from sklearn.preprocessing import StandardScaler

# Load the breast cancer dataset
dataset = datasets.load_breast_cancer()

# Load the features
X = dataset.data

# View the shape of the dataset
X.shape

# Create a scaler object
sc = StandardScaler()

# Fit the scaler to the features and transform
X_std = sc.fit_transform(X)

# Create a pca object with the 2 components as a parameter
pca = decomposition.PCA(n_components=2)

# Fit the PCA and transform the data
X_std_pca = pca.fit_transform(X_std)

# View the new feature data's shape
X_std_pca.shape

# View the new feature data
X_std_pca