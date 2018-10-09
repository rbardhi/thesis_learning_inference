'''
Created on Jun 7, 2018

@author: nitesh
'''

import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets, linear_model
from sklearn.metrics import mean_squared_error, r2_score
from models.UncertainLinearRegression import UncertainLinearRegression
from models.UncertainMulticlassLogisticRegression import UncertainMulticlassLogisticRegression

import time
from sklearn import metrics
from sklearn.linear_model import LogisticRegression
from numpy import linalg as LA


def testLinearRegression():
    # Load the diabetes dataset
    diabetes = datasets.load_diabetes()
    
    
    # Use only one feature
    diabetes_X = diabetes.data[:, np.newaxis, 2]
    
    # Split the data into training/testing sets
    diabetes_X_train = diabetes_X[:-20]
    diabetes_X_test = diabetes_X[-20:]
    
    # Split the targets into training/testing sets
    diabetes_y_train = diabetes.target[:-20]
    diabetes_y_test = diabetes.target[-20:]
    
    # Create linear regression object
    regr = linear_model.LinearRegression()
    prob = np.full((len(diabetes_y_train),), 0.1)
    for i in range(0,len(diabetes_X_train)):
        if diabetes_X_train[i][0] > 0.02:
            prob[i] = 0.1
        else:
            prob[i] = 0.5
    
    #regr = UncertainLinearRegression()
    #regr.fit(diabetes_X_train, diabetes_y_train, prob)
    #prob = np.full((len(diabetes_y_test),), 1.0)
    #diabetes_y_pred = regr.predict(diabetes_X_test, prob)
    
    # Train the model using the training sets
    regr.fit(diabetes_X_train, diabetes_y_train, sample_weight=prob)
    
    # Make predictions using the testing set
    diabetes_y_pred = regr.predict(diabetes_X_test)
    
    # The coefficients
    print('Coefficients: \n', regr.coef_)
    # The mean squared error
    print("Mean squared error: %.2f"
          % mean_squared_error(diabetes_y_test, diabetes_y_pred))
    # Explained variance score: 1 is perfect prediction
    print('Variance score: %.2f' % r2_score(diabetes_y_test, diabetes_y_pred))
    
    # Plot outputs
    plt.scatter(diabetes_X_test, diabetes_y_test,  color='black')
    plt.plot(diabetes_X_test, diabetes_y_pred, color='blue', linewidth=3)
    
    plt.xticks(())
    plt.yticks(())
    
    plt.show()
    
def testMulticlassLogisticRegression():
    # load the iris datasets
    dataset = datasets.load_iris()
    # fit a logistic regression model to the data
    model = LogisticRegression(solver='lbfgs', multi_class='multinomial', C=1.0, penalty='l2', fit_intercept=True, max_iter=100)
    prob = np.full((len(dataset.target),), 1.0)
    for i in range(0,len(dataset.data)/2):
        prob[i] = 0.1
    t0 = time.time()
    #model.fit(dataset.data, dataset.target,sample_weight=prob)
    #print("--- Sklearn: %s secs" % (time.time() - t0))
    

    model = UncertainMulticlassLogisticRegression()
    t0 = time.time()
    model.fit(dataset.data, dataset.target, prob)
    print("--- My : %s secs" % (time.time() - t0))
    
    # make predictions
    expected = dataset.target
    #x = np.array(dataset.data)
    #x = x/LA.norm(x, axis=1)[:,np.newaxis]
    predicted = model.predict(dataset.data)
    # summarize the fit of the model
    print(metrics.classification_report(expected, predicted))
    print(metrics.confusion_matrix(expected, predicted))
    
if __name__ == '__main__':
    testMulticlassLogisticRegression()   
