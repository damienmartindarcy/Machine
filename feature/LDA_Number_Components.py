# LDA - best number of components

# Load libraries
from sklearn import datasets
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
 
# Load the Iris flower dataset:
iris = datasets.load_iris()
X = iris.data
y = iris.target
 
# Create and run an LDA
lda = LinearDiscriminantAnalysis(n_components=None)
X_lda = lda.fit(X, y)
 
# Create array of explained variance ratios
lda_var_ratios = lda.explained_variance_ratio_
 
# Create a function
def select_n_components(var_ratio, goal_var: float) -> int:
    # Set initial variance explained so far
    total_variance = 0.0
    
    # Set initial number of features
    n_components = 0
    
    # For the explained variance of each feature:
    for explained_variance in var_ratio:
        
        # Add the explained variance to the total
        total_variance += explained_variance
        
        # Add one to the number of components
        n_components += 1
        
        # If we reach our goal level of explained variance
        if total_variance >= goal_var:
            # End the loop
            break
            
    # Return the number of components
    return n_components
 
# Run function
select_n_components(lda_var_ratios, 0.95)
