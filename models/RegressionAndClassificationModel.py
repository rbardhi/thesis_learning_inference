'''
Created on Jun 10, 2018

@author: nitesh
'''
from sklearn import datasets, linear_model

class Models(object):
    
    def regression(self, features, target, probability):
        regr = linear_model.LinearRegression()
        