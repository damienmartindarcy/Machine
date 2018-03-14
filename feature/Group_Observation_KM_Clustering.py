# Group observations - KM Clustering

# Load libraries
from sklearn.datasets import make_blobs
from sklearn.cluster import KMeans
import pandas as pd
 
# Make simulated feature matrix
X, _ = make_blobs(n_samples = 50,
                  n_features = 2,
                  centers = 3,
                  random_state = 1)

# Create DataFrame
df = pd.DataFrame(X, columns=['feature_1','feature_2'])
 
# Make k-means clusterer
clusterer = KMeans(3, random_state=1)

# Fit clusterer
clusterer.fit(X)

# Create feature based on predicted cluster

# Predict values
df['group'] = clusterer.predict(X)

# First few observations
df.head(5)