'''
Created on Aug 29, 2017

@author: niteshkumar
'''
import ast
from subprocess import Popen, PIPE
from pyswip import Prolog
from TranslateToDC import TranslateToDC
import logging
from yapInterface.YapInterface import YapInterface

class PrologInterface(object):
    
    def __init__(self, prologFile, whichProlog):
        logging.basicConfig(level=logging.DEBUG, filename="dcProbLogs", filemode="a+", format="%(asctime)-15s %(levelname)-8s %(message)s")
        self.fileName = prologFile
        self.whichProlog = whichProlog
        self.prolog = None
        self.dcFileName = None
        #self.dc = None
        self.translator = TranslateToDC()
        self.numOfSamples = 0
        if(self.whichProlog == 'SWI-prolog'):
            self.prolog = Prolog()
            self._consult()
        elif(self.whichProlog == 'YAP-prolog'):
            self.prolog = YapInterface()
            self._consult()
        else:
            raise Exception('Only SWI-prolog supported')
    
    def _consult(self):
        if self.whichProlog == 'SWI-prolog':
            self.prolog.consult(self.fileName)
        elif self.whichProlog == 'YAP-prolog':
            self.prolog.consult(self.fileName)
        else:
            pass
    
    def simpleQuery(self, query):
        if self.whichProlog == 'SWI-prolog':
            return self.prolog.query(query)
        else:
            raise Exception('Only SWI-prolog supported for simpleQuery')
        
    def listQuery(self, query, listOfVars):
        result = {}
        for var in listOfVars:
            result[var] = []
        if self.whichProlog == 'SWI-prolog':
            response = self.prolog.query(query)
            for ele in response:
                for var in listOfVars:
                    a = result[var]
                    a.append(ele[var])
        elif self.whichProlog == 'YAP-prolog':
            stringOfVars = '['
            lenOfListOfVars = len(listOfVars)
            for i in range(0, lenOfListOfVars):
                if i == lenOfListOfVars-1:
                    stringOfVars += listOfVars[i] + ']'
                else:
                    stringOfVars += listOfVars[i] + ','
            query = query.replace('.','')
            query = 'findall(' + stringOfVars + ',' + '(' + query + '),FINDALL).'
            response = self.prolog.prologQuery(query)
            response = self.convertStringReprOfListToList(response)
            for ele in response:
                for i in range(0, len(listOfVars)):
                    a = result[listOfVars[i]]
                    a.append(ele[i])
        else:
            pass
        return result
    
    def convertStringReprOfListToList(self, inp):
        out = []
        inp = inp.split('],[')
        for i in range(0, len(inp)):
            inp[i] = inp[i].replace('[','')
            inp[i] = inp[i].replace(']','')
            inp[i] = inp[i].split(',')
        for i in range(0, len(inp)):
            tempOutput = []
            for j in range(0, len(inp[i])):
                try:
                    tempLiteral = ast.literal_eval(inp[i][j])
                except ValueError:
                    tempLiteral = inp[i][j]
                except SyntaxError:
                    tempLiteral = inp[i][j]
                tempOutput.append(tempLiteral)
            out.append(tempOutput)
        return out
    
    def initiateDC(self, dcFileName, numOfSamples):
        self.dcFileName = dcFileName
        #self.dc = YapInterface(dcFileName)
        self.numOfSamples = numOfSamples
        
    def getProbabilityOfQuery(self, query, body, listOfSubstitutionVars):
        response = self.prolog.query(query)
        prob = []
        for ele in response:
            subs = {}
            for var in listOfSubstitutionVars:
                if var in ele.keys():
                    subs[var] = ele[var]
            dcQuery = self.generateDCQuery(body, subs)
            query = 'python ../yapInterface/YapInterface.py ' + '"' + self.dcFileName + '" ' + str(self.numOfSamples)+ ' "(' + dcQuery + ')"'
            outputString = Popen(query, stdout=PIPE, shell=True).communicate()
            p = float(outputString[0][0:-1])
            #p = self.dc.query(self.numOfSamples, "(" + dcQuery + ")", "")
            s = str(p) + ' ' + query
            logging.info(s)
            prob.append(p)   
        return prob
    
    def generateDCQuery(self, query, subs):
        query = query.replace('\\\\+', '\\+')
        query = query.replace(' ', '')
        for key in subs.keys():
            keyStr = '(' + key + ')'
            valStr = '(' + str(subs[key]) + ')'
            query = query.replace(keyStr, valStr)
            keyStr = ',' + key + ')'
            valStr = ',' + str(subs[key]) + ')'
            query = query.replace(keyStr, valStr)
            keyStr = ',' + key + ','
            valStr = ',' + str(subs[key]) + ','
            query = query.replace(keyStr, valStr)
            keyStr = '(' + key + ','
            valStr = '(' + str(subs[key]) + ','
            query = query.replace(keyStr, valStr)
        query = self.translator.convertDCPredicates(query, 'body')
        return query
    
    def getProlog(self):
        return self.prolog
    
if __name__ == '__main__':
    obj = PrologInterface('../data/hepatitis.pl', 'SWI-prolog')
    #obj1 = PrologInterface('../data/interp1.pl', 'SWI-prolog')
    res = obj.simpleQuery('mode(X, Y).')
    for ele in res:
        print ele['X'], ele['Y']
    #res = obj.listQuery('teaches(P, C), ability(P, X3).', ['C', 'P', 'X3'])
    #print res
    #res = obj.listQuery('intelligence(S, X_1), health(S, X2), satScore(S, X3), X3 > 50.', ['S', 'X_1', 'X2', 'X3'])
    #print res
    #res = obj.listQuery('satScore(S, X), intelligence(S, X1), health(S, X2), maxMod(V, grade(S, C, V), I1), I1 == low.', ['X', 'X1', 'X2'])
    #res = obj.listQuery('satScore(S, X).', ['S', 'X'])
    #print res
    
#     obj = PrologInterface('../data/prologUniv.pl', 'SWI-prolog')
#     obj.initiateDC('../data/dcUniv.pl', 100)
# #     query = 'intelligence(S,I), maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high'
# #     body = 'maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high'
# #     listOfSubstitutionVars = ['S']
# #     probs = obj.getProbabilityOfQuery(query, body, listOfSubstitutionVars)
# #     print probs
#     
#     ########## Generate DC clauses tests ##############
#     body = 'intelligence(S,I), maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high, \+maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), \+avg(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), avg(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3>10, minMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high, maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == low'
#     subs = {'S': 4}
#     dcQuery = obj.generateDCQuery(body, subs)
#     print dcQuery
#     ###################################################

    #for i in range(0, 10):
    #    outputString = Popen('python ../yapInterface/YapInterface.py "../data/dcUniv.pl" 100 "(maxMod(R_M, (registration(C_M,14), rating(C_M)~=R_M), X_T_3), X_T_3 == high)"', stdout=PIPE, shell=True).communicate()
    #    print float(outputString[0][0:-1])
    #obj = YapInterface("../data/dcUniv.pl")
    #obj.test()
    
    
    

    
        
    
