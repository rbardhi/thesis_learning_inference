'''
Created on Jun 2, 2018

@author: nitesh
'''
import numpy as np
from scipy.optimize import minimize
from models.ModelSettings import LR_SETTINGS
from numpy import linalg as LA

class UncertainLinearRegression(object):
    
    def __init__(self):
        self.coef_ = None
        self.intercept_ = None
        self.numOfDataPoints = -1
        self.numOfAttributes = -1

    def funcAndGrad(self, weight, x, t, p, wMean, wPrec):
        numRows = x.shape[0]
        p1 = 0.5*wPrec*(np.sum((weight-wMean)**2))
        mask = np.full((numRows,1), 1.0)
        x1 = np.concatenate((x,mask), axis=1)
        y = np.dot(x1,weight.T)
        y = t - y
        err = y**2
        err = np.multiply(err, p)
        p2 = 0.5*np.sum(err)
        fval = p2 + p1
        y = np.multiply(y, p)
        fgrad = wPrec*(weight-wMean)
        for d in range(0,numRows):
            fgrad = fgrad - y[d]*x1[d,:]
        return fval, fgrad

    def fit(self, features, label, prob):
        x = np.array(features)
        t = np.array(label)
        p = np.array(prob)
        (self.numOfDataPoints, self.numOfAttributes) = x.shape
        wMean = np.full((self.numOfAttributes+1,),0)
        wPrec = LR_SETTINGS['weight_precision']
        opt = {'maxiter': LR_SETTINGS['maximum_iteration'], 'disp': LR_SETTINGS['message_display']}
        weight0 = np.random.rand(self.numOfAttributes+1)
        optModel = minimize(self.funcAndGrad, weight0, args=(x, t, p, wMean, wPrec), method=LR_SETTINGS['optimization_method'], jac=True, tol=LR_SETTINGS['tolerance'], options=opt)
        optWeight = optModel.x
        self.intercept_= optWeight[-1]
        self.coef_ = optWeight[0:self.numOfAttributes]
        return self
    
    def predict(self, testData, prob):
        x = np.array(testData)
        y = np.dot(x,self.coef_.T) + self.intercept_
        #y = np.power(y, prob)
        return y
    
    