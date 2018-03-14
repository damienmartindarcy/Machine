# LDA - Dimension Reduction

# Load libraries
from sklearn import datasets
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
 
# Load the Iris flower dataset:
iris = datasets.load_iris()
X = iris.data
y = iris.target
 
# Create an LDA that will reduce the data down to 1 feature
lda = LinearDiscriminantAnalysis(n_components=1)

# run an LDA and use it to transform the features
X_lda = lda.fit(X, y).transform(X)
 
# Print the number of features
print('Original number of features:', X.shape[1])
print('Reduced number of features:', X_lda.shape[1])
 
## View the ratio of explained variance
lda.explained_variance_ratio_