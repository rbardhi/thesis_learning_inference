'''
Created on Jun 1, 2018

@author: nitesh
'''
import numpy as np
from scipy.optimize import minimize
from models.ModelSettings import MLR_SETTINGS
from numpy import linalg as LA

class UncertainMulticlassLogisticRegression(object):
    
    def __init__(self):
        self.classes_ = None
        self.classDic = None
        self.numOfDataPoints = -1
        self.numOfClasses = -1
        self.numOfAttributes = -1
        self.coef_ = None
        self.intercept_ = None
        self.classDictRevs = None
   
   
    # x is N*D array, weight is K*(D+1) array, w is K*D array, b is K*1 array
    def funcAndGrad1(self, weight, x, t, wMean, wPrec):
        size = weight.shape[0]
        (numRows, numCols) = x.shape
        numClasses = len(weight)/(numCols+1)
        weight = np.reshape(weight,(numClasses,numCols+1))
        mask = np.full((numRows,1), 1.0)
        x1 = np.concatenate((x,mask), axis=1)
        y = np.dot(x1,weight.T)
        for i in range(0,numRows):
            y[i] = y[i] - np.amax(y[i])
        y = np.exp(y)
        y = y/(np.sum(y, axis=1)[:,np.newaxis])
        logy = np.log(y)
        t1 = np.multiply(logy,t)
        p1 = 0.5*wPrec*(np.sum((weight-wMean)**2))
        p2 = np.sum(t1)
        fval = p1 - p2
        wfgrad = wPrec*(weight-wMean)
        for j in range(0,numClasses):
            q1 = np.full((numCols+1,), 0.0)
            for d in range(0,numRows):
                q1 = q1 + (y[d,j] - t[d,j])*x1[d,:]
            wfgrad[j,:] = wfgrad[j,:] + q1
        fgrad = np.reshape(wfgrad,(size,))
        print fval
        return fval
    
       
    # x is N*D array, weight is K*(D+1) array, w is K*D array, b is K*1 array
    def funcAndGrad(self, weight, x, t, wMean, wPrec):
        size = weight.shape[0]
        (numRows, numCols) = x.shape
        numClasses = len(weight)/(numCols+1)
        weight = np.reshape(weight,(numClasses,numCols+1))
        mask = np.full((numRows,1), 1.0)
        x1 = np.concatenate((x,mask), axis=1)
        y = np.dot(x1,weight.T)
        for i in range(0,numRows):
            y[i] = y[i] - np.amax(y[i])
        y = np.exp(y)
        y = y/(np.sum(y, axis=1)[:,np.newaxis])
        logy = np.log(y)
        t1 = np.multiply(logy,t)
        p1 = 0.5*wPrec*(np.sum((weight-wMean)**2))
        p2 = np.sum(t1)
        fval = p1 - p2
        wfgrad = wPrec*(weight-wMean)
        for j in range(0,numClasses):
            q1 = np.full((numCols+1,), 0.0)
            for d in range(0,numRows):
                q1 = q1 + (y[d,j] - t[d,j])*x1[d,:]
            wfgrad[j,:] = wfgrad[j,:] + q1
        fgrad = np.reshape(wfgrad,(size,))
        print fval
        return fval,fgrad
    
    def fit(self, features, label, prob):
        x = np.array(features)
        #x = x/LA.norm(x, axis=1)[:,np.newaxis]
        (self.numOfDataPoints, self.numOfAttributes) = x.shape
        self.classes_ = np.unique(label)
        self.numOfClasses = len(self.classes_)
        self.classDict = dict()
        self.classDictRevs = dict()
        for i in range(0,self.numOfClasses):
            self.classDict[self.classes_[i]] = i
            self.classDictRevs[i] = self.classes_[i]
        t = np.full((self.numOfDataPoints,self.numOfClasses), 0)
        for i in range(0,self.numOfDataPoints):
            t[i][self.classDict[label[i]]] = prob[i]
        wPrec = MLR_SETTINGS['weight_precision']
        weight0 = np.random.rand(self.numOfClasses,self.numOfAttributes+1)
        wMean = np.full((self.numOfClasses,self.numOfAttributes+1),0)
        opt = {'maxiter': MLR_SETTINGS['maximum_iteration'], 'disp': MLR_SETTINGS['message_display']}
        optModel = minimize(self.funcAndGrad1, weight0, args=(x, t, wMean, wPrec), method=MLR_SETTINGS['optimization_method'], jac=False, tol=MLR_SETTINGS['tolerance'], options=opt)
        optWeight = optModel.x
        optWeight = np.reshape(optWeight, (self.numOfClasses, self.numOfAttributes+1))
        self.intercept_= optWeight[:, -1]
        self.coef_ = optWeight[:, 0:self.numOfAttributes]
        return self
        
    def predict_log_proba(self, features):
        x = np.array(features)
        #x = x/LA.norm(x, axis=1)[:,np.newaxis]
        y = np.add(np.dot(x,self.coef_.T),self.intercept_)
        y = np.exp(y)
        y = y/(np.sum(y, axis=1)[:,np.newaxis])
        logy = np.log(y)
        return logy
    
    def predict(self, features):
        logy = self.predict_log_proba(features)
        maxIdx = np.argmax(logy,axis=1)
        pLabel = []
        for i in range(0, len(maxIdx)):
            pLabel.append(self.classDictRevs[maxIdx[i]])
        return np.array(pLabel)
    
