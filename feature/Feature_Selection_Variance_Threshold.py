# Feature selection - variance threshold

from sklearn import datasets
from sklearn.feature_selection import VarianceThreshold
 
# Load iris data
iris = datasets.load_iris()

# Create features and target
X = iris.data
y = iris.target
 
# Create VarianceThreshold object with a variance with a threshold of 0.5
thresholder = VarianceThreshold(threshold=.5)

# Conduct variance thresholding
X_high_variance = thresholder.fit_transform(X)
 
# View first five rows with features with variances above threshold
X_high_variance[0:5]