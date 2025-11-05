############################### VSN301 example - Hyperparameter Tuning and testing ####################################
# 1. Packages import
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import (
    classification_report, confusion_matrix, accuracy_score,
    cohen_kappa_score)

# 2. Load data
csv_path = "VSN301/VSN301_training_data.csv"  # Load the exported training dataset from GEE
df = pd.read_csv(csv_path)
df = df.drop(columns=["system:index", ".geo", "source_file", "source_species"], errors='ignore') # Clean the dataset

# 3. Split features and labels (class)
X = df.drop(columns=["class"])
y = df["class"]

# 4. Split the dataset in train (80%) and test (20%)
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, stratify=y, random_state=42)

print("Train size:", len(X_train))
print("Test size:", len(X_test))

# 5. Grid Search for best hyperparameters
param_grid = {
    'n_estimators': [100, 200, 300, 400, 500], # Defining all the possible ranges of the hyperparameters
    'max_depth': [None, 10, 20, 30],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 5, 10],
    'max_features': ['sqrt', 'log2']}

rf = RandomForestClassifier(random_state=42) # fixing the random seed, standard value
grid_search = GridSearchCV(rf, param_grid, cv=5, scoring='accuracy', n_jobs=-1, verbose=1) # 5 fold cross-validation for each combination. Accuracy is the criterion to be maximized
grid_search.fit(X_train, y_train) # Start the tuning

best_model = grid_search.best_estimator_  # Save the best model found
print("Best Parameters:", grid_search.best_params_) # print only the list of hyperparameters (to copy for the second run in GEE)

# 6. Evaluate on validation set
def report(model, X, y, title): # Defining the function "report" to predict the classes
    y_pred = model.predict(X)
    print(f"\n{title.upper()} REPORT")
    print(classification_report(y, y_pred))
    print("Confusion Matrix:\n", confusion_matrix(y, y_pred)) # Print the confusion matrix
    print("Accuracy:", accuracy_score(y, y_pred))             # Print the Accuracy
    print("Kappa:", cohen_kappa_score(y, y_pred))             # Print the Kappa value

report(best_model, X_train, y_train, "Train")      # To check the overfitting: if the Kappa and Accuracy are 10-15% times higher
report(best_model, X_test, y_test, "Test")         # than the ones for the test set, probable overfitting

