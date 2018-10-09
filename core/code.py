'''
Created on Aug 16, 2017

@author: niteshkumar
'''
from pyswip import Prolog
from TypesModesAggregationLanguage import TypesModesAggregationLanguage
from FeatureSpace import Feature, FeatureCreator
from PrologInterface import PrologInterface

if __name__ == '__main__':
    interface = PrologInterface('../data/university.pl', 'SWI-prolog')
    language = TypesModesAggregationLanguage(interface)
    obj2 = FeatureCreator(language)
    obj2.fillFeatureList()
    for feat in obj2.featureList:
        print feat.featureAsListOfString()
    
#     prolog = Prolog()
#     prolog.consult('../data/university.pl')
#     a = prolog.query('intelligence(S, X1), health(S, X2), satScore(S, X3), X3 > 50.')
#     for ele in a:
#         #print ele
#         print ele['S'], ele['X1'], ele['X2'], ele['X3']
    #print list(prolog.query('mode(X,Y).'))
    #print list(prolog.query('ground(student(ann)).'))
    #print list(prolog.query('intelligence(X, Y).'))
#     for ele in prolog.query('learn(X).'):
#         print ele['X']
#     print type(ele['X'])