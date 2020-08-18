# Installing Keras
# conda install keras

# importing the libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Loading the data
# dataset = pd.read_csv('../sonar-data/sonar.csv', header = None)
dataset = pd.read_csv('sonar.csv', header = None)

X = dataset.iloc[:, 0:60].values
y = dataset.iloc[:, 60].values

# Feature Encoding
from sklearn.preprocessing import LabelEncoder, OneHotEncoder
encoder = LabelEncoder()
y = encoder.fit_transform(y)

# Splitting into training and test sets
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state = 0)

# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

# Building an ANN
# Importing Keras Libraries and packages
import keras 
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import Dropout

# Initialize the ANN
classifier = Sequential()

# Build the input and hidden layers with dropout
classifier.add(Dense(units = 32, activation = 'relu', kernel_initializer = 'uniform', input_dim = 60))
classifier.add(Dropout(rate = 0.1))

# Adding the second hidden layer
classifier.add(Dense(units = 32, activation = 'relu', kernel_initializer = 'uniform'))
classifier.add(Dropout(rate = 0.1))

# Adding an output layer
classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))

# Compiling the ANN
classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])

# Fitting ANN to training set
history = classifier.fit(X_train, y_train, validation_split = 0.33, batch_size = 10, epochs = 100)

# Predicting on test set
y_pred = classifier.predict(X_test) > 0.5

# Evaluating using confusion matrix
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)

# Listing all the data in training history
print(history.history.keys())

# Visualizing the loss updates with the number of epochs
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('Training Loss vs Epochs')
plt.ylabel('Training Loss')
plt.xlabel('Number of Epochs')
plt.legend(['train', 'validation'], loc='upper left')
plt.show()

# Visualizing the accuracy updates with the number of epochs
plt.plot(history.history['acc'])
plt.plot(history.history['val_acc'])
plt.title('Training Accuracy vs Epochs')
plt.ylabel('Training Accuracy')
plt.xlabel('Number of Epochs')
plt.legend(['train', 'validation'], loc='upper left')
plt.show()

# Evaluating, Improving and Tuning the ANN

# Evaluating the ANN
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import StratifiedKFold
from keras.models import Sequential
from keras.layers import Dense
def build_classifier():
    classifier = Sequential()
    classifier.add(Dense(units = 32, kernel_initializer = 'uniform', activation = 'relu', input_dim = 60))
    classifier.add(Dense(units = 32, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = 'adam', loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier, batch_size = 10, epochs = 15)
kfold = StratifiedKFold(n_splits=10, shuffle=True, random_state=7)
accuracies = cross_val_score(estimator = classifier, X = X_train, y = y_train, cv = kfold)
print(accuracies)
mean = accuracies.mean()
print(mean)
variance = accuracies.std()
print(variance)

# Improving the ANN
# Dropout Regularization to reduce overfitting if needed

# Tuning the ANN
from keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import GridSearchCV
from keras.models import Sequential
from keras.layers import Dense
def build_classifier(optimizer):
    classifier = Sequential()
    classifier.add(Dense(units = 32, kernel_initializer = 'uniform', activation = 'relu', input_dim = 60))
    classifier.add(Dense(units = 32, kernel_initializer = 'uniform', activation = 'relu'))
    classifier.add(Dense(units = 1, kernel_initializer = 'uniform', activation = 'sigmoid'))
    classifier.compile(optimizer = optimizer, loss = 'binary_crossentropy', metrics = ['accuracy'])
    return classifier
classifier = KerasClassifier(build_fn = build_classifier)
parameters = {'batch_size': [10, 25, 32],
              'epochs': [15, 20],
              'optimizer': ['adam', 'rmsprop']}
grid_search = GridSearchCV(estimator = classifier,
                           param_grid = parameters,
                           scoring = 'accuracy',
                           cv = 10)
grid_search = grid_search.fit(X_train, y_train)    
best_parameters = grid_search.best_params_
best_accuracy = grid_search.best_score_
print(best_parameters)
# {'batch_size': 10, 'epochs': 100, 'optimizer': 'rmsprop'}
print(best_accuracy)
# 0.8653846153846154