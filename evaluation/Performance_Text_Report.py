# Performance Text Reports

# Load libraries
from sklearn import datasets
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report

# Load data
iris = datasets.load_iris()

# Create feature matrix
X = iris.data

# Create target vector
y = iris.target

# Create list of target class names
class_names = iris.target_names

# Create training and test set
X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=1)
# Create logistic regression
classifier = LogisticRegression()

# Train model and make predictions
y_hat = classifier.fit(X_train, y_train).predict(X_test)

# Create a classification report
print(classification_report(y_test, y_hat, target_names=class_names))