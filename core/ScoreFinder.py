'''
Created on Aug 28, 2017

@author: niteshkumar
'''
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sympy.logic.boolalg import false

class ScoreFinder(object):
    def __init__(self):
        numIteration = 100
        # For softmax regression
        regularization = 1
        self.softmax = LogisticRegression(solver='lbfgs', multi_class='multinomial', C=regularization, penalty='l2', fit_intercept=True, max_iter=numIteration)
        
        # For Bayesian linear regression
        self.gaussian = LinearRegression(fit_intercept=True)
    
    def fitNoDistribution(self):
        result = {'score' : np.nan, 'coefficient' : np.nan, 'variance' : np.nan, 'distribution': 'NoDistribution'}
        return result
    
    def fitGaussainDistribution(self, bigY, bigYCross):
        mean = np.mean(bigY)
        score = 0
        #variance = np.var(bigY)
        numRows = len(bigYCross)
        vr = []
        for i in range(0, numRows):
            vr.append(bigYCross[i] - mean)
        variance = np.var(vr)
        if numRows == 1:
            pass
        else:
            cons1 = -0.91893853320467267 - 0.5*np.log(variance)
            cons2 = -0.5*(1/variance)
            for i in range(0,numRows):
                a = bigYCross[i] - mean
                a *= a
                a *= cons2
                a += cons1
                score += a
        distribution = 'Gaussian(' + str(mean) + ', ' + str(variance) + ')'
        result = {'score' : score, 'coefficient' : mean, 'variance' : variance, 'distribution': distribution}
        return result
    
    def fitMultinomialDistribution(self, bigY, bigYCross):
        numRows = len(bigY)
        classes = {}
        classLogProbs = {}
        classProbs = []
        for i in bigY:
            if classes.has_key(i):
                cnt = classes[i]
                classes[i] = cnt + 1
            else:
                classes[i] = 1
        leni = len(classes)-1
        i = 0
        classKeys = '['
        for key in classes.keys():
            classLogProbs[key] = np.log(classes[key]/float(numRows))
            classProbs.append(classes[key]/float(numRows))
            if i == leni:
                classKeys += key + ']'
            else:
                classKeys += key + ', '
            i += 1
        score = 0
        for i in bigYCross:
            score += classLogProbs[i]
        distribution = 'Finite(' + classKeys + ', ' + str(classProbs) + ')' 
        result = {'score': score, 'coefficient': classProbs, 'distribution': distribution}
        return result
        
    def fitLinearRegressionModel(self, bigX, bigY, bigXCross, bigYCross, xVar):
        colSize = len(bigX)
        rowSize = len(bigX[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigX[j][i])
            data.append(aRow)
        model = self.gaussian.fit(data, bigY)
        coef = model.coef_
        intercept = model.intercept_
        param = list(coef)
        param.append(intercept)
        probs = []
        colSize = len(bigXCross)
        rowSize = len(bigXCross[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigXCross[j][i])
            data.append(aRow)
        numCols = len(data[0])
        numRows = len(data)
        for rec in data:
            p = 0
            for i in range(0,numCols):
                p += coef[i]*rec[i]
            p += intercept
            probs.append(p)
        score = 0
        #variance = np.var(probs)
        vr = []
        for i in range(0,numRows):
            vr.append(bigYCross[i] - probs[i])
        variance = np.var(vr)
        if numRows == 1:
            pass
        else:
            cons1 = -0.91893853320467267 - 0.5*np.log(variance)
            cons2 = -0.5*(1/variance)
            for i in range(0,numRows):
                a = bigYCross[i] - probs[i]
                a *= a
                a *= cons2
                a += cons1
                score += a
        distribution = 'Gaussian(['
        leni = len(xVar)-1
        i = 0
        for var in xVar:
            if i == leni:
                distribution += var + '], '
            else:
                distribution += var + ', '
            i += 1
        distribution += str(param) + ', ' + str(variance) + ')'
        result = {'score' : score, 'coefficient' : param, 'variance' : variance, 'distribution': distribution}
        return result
    
    def fitMultinomialClassificationModel(self, bigX, bigY, bigXCross, bigYCross, xVar):
        colSize = len(bigX)
        rowSize = len(bigX[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigX[j][i])
            data.append(aRow)
        model = self.softmax.fit(data, bigY)
        colSize = len(bigXCross)
        rowSize = len(bigXCross[0])
        data = []
        for i in range(0, rowSize):
            aRow = [] 
            for j in range(0, colSize):
                aRow.append(bigXCross[j][i])
            data.append(aRow)
        logEstimate = model.predict_log_proba(data)
        classes = model.classes_.tolist()
        classString = '['
        leni = len(classes)-1
        for i in range(0,len(classes)):
            if i == leni:
                classString += classes[i] + ']'
            else:
                classString += classes[i] + ', '
        coef = model.coef_
        intercept = model.intercept_
        score = 0
        for i in range(0,len(logEstimate)):
            idx = classes.index(bigYCross[i])
            score += logEstimate[i][idx]
        coef = np.concatenate((coef, np.array([list(intercept)]).T), axis=1)
        param = {}
        parameter = []
        idx = 0
        for arr in coef:
            param[classes[idx]] = list(arr)
            parameter.append(list(arr))
            idx += 1
        if(len(parameter) == 1):
            distribution = 'LogisticRegression(['
            leni = len(xVar)-1
            i = 0
            for var in xVar:
                if i == leni:
                    distribution += var + '], '
                else:
                    distribution += var + ', '
                i += 1
            distribution += classString + ', ' + str(parameter[0]) + ')'
        else:
            distribution = 'SoftmaxRegression(['
            leni = len(xVar)-1
            i = 0
            for var in xVar:
                if i == leni:
                    distribution += var + '], '
                else:
                    distribution += var + ', '
                i += 1
            distribution += classString + ', ' + str(parameter) + ')'
        result = {'score' : score, 'coefficient': param, 'distribution': distribution}
        return result

    def getScore(self, bigX, bigY, bigXCross, bigYCross, targetType, xVar):
        scoreAndDistribution = None
        bigXEmpty = False
        if(len(bigX) == 0):
            bigXEmpty = True
        elif(len(bigX[0]) == 0):
            bigXEmpty = True
        else:
            bigXEmpty = False
        if(bigXEmpty):
            if(bigY == []):
                scoreAndDistribution = self.fitNoDistribution()
            elif(targetType == 'discrete'):
                scoreAndDistribution = self.fitMultinomialDistribution(bigY, bigYCross)
            elif(targetType == 'continuous'):
                scoreAndDistribution = self.fitGaussainDistribution(bigY, bigYCross)
            else:
                pass
        elif(targetType == 'continuous'):
            scoreAndDistribution = self.fitLinearRegressionModel(bigX, bigY, bigXCross, bigYCross, xVar)
        elif(targetType == 'discrete'):
            if(len(set(bigY)) == 1):
                scoreAndDistribution = self.fitMultinomialDistribution(bigY, bigYCross)
            else:
                scoreAndDistribution = self.fitMultinomialClassificationModel(bigX, bigY, bigXCross, bigYCross, xVar)
        else:
            print "Target variable type undefined"
        return scoreAndDistribution
            
if __name__ == '__main__':
    X = [[3.2, 4.1, 4.9, 10], [2.3, 1.3, 7.8, 9.7]]
    Y = [1, 1, 3, 2]
    X = [[3.2, 4.1], [2.3, 1.3]]
    Y = [2.1, 1.4]
    sf = ScoreFinder()
    a = sf.getScore(X, Y, X, Y, 'continuous', ['A', 'B'])
    print a 
    b = sf.getScore([], Y, [], Y, 'continuous', ['A', 'B'])
    print b
