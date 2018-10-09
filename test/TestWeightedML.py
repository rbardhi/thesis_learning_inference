'''
Created on Aug 5, 2018

@author: nitesh
'''
from sklearn.linear_model import LogisticRegression

softmax = LogisticRegression(solver='lbfgs', multi_class='multinomial', C=1.0, penalty='l2', fit_intercept=True, max_iter=100)

data = [[1.0, 0.5], [1, 3], [2, 1], [2, 3], [3, 2]]
bigY = [1, 1, 0, 1, 0]
bigP = [0.5, 1.0, 1.0, 1.0, 1.0]

test = [[1.1, 0.9]]
model = softmax.fit(data, bigY, sample_weight=bigP)
classes = model.classes_.tolist()
logEstimate = model.predict_proba(test)

print classes
print logEstimate

