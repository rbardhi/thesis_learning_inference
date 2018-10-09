'''
Created on Aug 29, 2017

@author: niteshkumar
'''
import ast, re
from subprocess import Popen, PIPE
from TranslateToDC import TranslateToDC
import logging
import sys
from yapWrapper import PyDC
## After executing from command line : $ python setup.py build_ext --inplace

BIGNUM = 1000000
VARIABLE_NUMBER = 3
FLAG = 0

class YapPrologInterface(object):
    
    def __init__(self, randomVariablePredicates):
        logging.basicConfig(level=logging.INFO, filename="dcProbLogs", filemode="w", format="%(asctime)-15s %(levelname)-8s %(message)s")
        self.yapPrologInterface_logger = logging.getLogger('YapPrologInterface Logger :')
        yapPrologInterface_logger_handler = logging.FileHandler("dcProbLogs", mode="w")
        yapPrologInterface_logger_handler.setFormatter(logging.Formatter("%(asctime)-15s %(levelname)-8s %(message)s"))
        yapPrologInterface_logger_handler.setLevel(logging.INFO)
        self.yapPrologInterface_logger.addHandler(yapPrologInterface_logger_handler)
        self.translator = TranslateToDC()
        self.translator.setRandomVariablePredicates(randomVariablePredicates)
        self.numOfSamples = 0
        self.yapObject = None
        self.yapFileName = []
    
    def consultWithOneFile(self, yapFileName):
        self.yapObject = PyDC(yapFileName)
        self.yapFileName.append(yapFileName)
        
    def consultWithTwoFiles(self, yapFileName1, yapFileName2, numOfSamples):
        self.yapObject = PyDC(yapFileName1, yapFileName2)
        self.yapFileName.append(yapFileName1)
        self.yapFileName.append(yapFileName2)
        self.numOfSamples = numOfSamples
    
    def setNumOfSamples(self, num):
        self.numOfSamples = num
    
    def query(self, numOfSamples, query, evidence):
        probability = self.yapObject.query(numOfSamples, query, evidence)
        return probability
    
    def executeQuery(self, queryStr):
        self.yapObject.execute(queryStr)
    
    def sample(self, numOfSamples, query, evidence, variable):
        return self.yapObject.sample(numOfSamples, query, evidence, variable, BIGNUM)
    
    def prologQuery(self, query):
        st = self.yapObject.prologQuery(VARIABLE_NUMBER, query, BIGNUM)
        return st
    
    def queryWithSamples(self, numOfSamples, query, evidence, variable):
        self.yapObject.queryWithSamples(numOfSamples, query, evidence, variable, FLAG, BIGNUM)
        result = dict()
        result['probability'] = self.yapObject.prob
        result['samples'] = self.yapObject.samples
        return result
    
    def simpleQuery(self, query):
        query = query.replace(' ', '')
        listOfVars = re.findall('\((.*)\)', query)[0]
        listOfVars = listOfVars.split(',')
        result = self.listQuery(query, listOfVars)
        keys = result.keys()
        akey = keys[0]
        avalue = result[akey]
        res = []
        for i in range(0, len(avalue)):
            tempres = {}
            for key in keys:
                tempres[key] = result[key][i]
            res.append(tempres)
        return res
        
    def listQuery(self, query, listOfVars):
        result = {}
        for var in listOfVars:
            result[var] = []
        stringOfVars = '['
        lenOfListOfVars = len(listOfVars)
        for i in range(0, lenOfListOfVars):
            if i == lenOfListOfVars-1:
                stringOfVars += listOfVars[i] + ']'
            else:
                stringOfVars += listOfVars[i] + ','
        if query[-1] == '.':
            query = query[:-1]
        query = 'findall(' + stringOfVars + ',' + '(' + query + '),FINDALL).'
        response = self.prologQuery(query)
        response = self.convertStringReprOfListToList(response)
        for ele in response:
            for i in range(0, len(listOfVars)):
                a = result[listOfVars[i]]
                a.append(ele[i])
        s = 'Query: ', query, '   Response ' + str(result)
        logging.info(s)
        return result
    
    def convertStringReprOfListToList(self, inp):
        openBrackets = ['[', '(', '{']
        closeBrackets = [']', ')', '}']
        insideBracketCounter = 0
        inp = inp.replace(' ', '')
        inp = list(inp)
        for i in range(0, len(inp)):
            if inp[i] in openBrackets:
                insideBracketCounter += 1
            elif inp[i] in closeBrackets:
                insideBracketCounter -= 1
            else:
                pass
            if inp[i] == ',' and insideBracketCounter == 1:
                inp[i] = ';'
            elif inp[i] == ',' and insideBracketCounter == 2:
                inp[i] = ':'
            else:
                pass
        inp = ''.join(inp)
        inp = inp[1:]
        inp = inp[:-1]
        out = []
        if inp == '':
            return out
        inp = inp.split(';')
        for i in range(0, len(inp)):
            inp[i] = inp[i][1:]
            inp[i] = inp[i][:-1]
            inp[i] = inp[i].split(':')
        for i in range(0, len(inp)):
            tempOutput = []
            for j in range(0, len(inp[i])):
                if ('[' in inp[i][j]):
                    tempLiteralList = []
                    tempLiteral = inp[i][j]
                    tempLiteral = tempLiteral[1:]
                    tempLiteral = tempLiteral[:-1]
                    tempLiteral = tempLiteral.split(',')
                    for k in range(0, len(tempLiteral)):
                        st = self.returnLiteral(tempLiteral[k])
                        if st == '':
                            pass
                        else:
                            tempLiteralList.append(st)
                    tempLiteral = tempLiteralList
                    tempOutput.append(tempLiteral)
                else:
                    tempLiteral = self.returnLiteral(inp[i][j])
                    tempOutput.append(tempLiteral)
            out.append(tempOutput)
        return out
    
    def returnLiteral(self, inp):
        out = inp
        try:
            out = ast.literal_eval(inp)
        except Exception:
            pass
        return out
        
    def getProbabilityOfQuery(self, query, body, listOfSubstitutionVars):
        result = self.listQuery(query, listOfSubstitutionVars)
        keys = result.keys()
        akey = keys[0]
        avalue = result[akey]
        response = []
        for i in range(0, len(avalue)):
            tempres = {}
            for key in keys:
                tempres[key] = result[key][i]
            response.append(tempres)
        prob = []
        for ele in response:
            subs = {}
            for var in listOfSubstitutionVars:
                if var in ele.keys():
                    subs[var] = ele[var]
            dcQuery = self.generateDCQuery(body, subs)
            #query = 'python ../yapInterface/YapInterface.py ' + '"' + self.dcFileName + '" ' + str(self.numOfSamples)+ ' "(' + dcQuery + ')"'
            #outputString = Popen(query, stdout=PIPE, shell=True).communicate()
            #p = float(outputString[0][0:-1])
            p = self.query(self.numOfSamples, "(" + dcQuery + ")", "")
            s = str(p) + ' ' + dcQuery
            self.yapPrologInterface_logger.info(s)
            prob.append(p)   
        return prob
    
    def getSamplesAndProbabilityOfQuery(self, query, body, listOfSubstitutionVars, listOfContinuousVars):
        stringOfContVars = '['
        lenOfListOfContVars = len(listOfContinuousVars)
        for i in range(0, lenOfListOfContVars):
            if i == lenOfListOfContVars-1:
                stringOfContVars += listOfContinuousVars[i] + ']'
            else:
                stringOfContVars += listOfContinuousVars[i] + ','
        result = self.listQuery(query, listOfSubstitutionVars)
        keys = result.keys()
        akey = keys[0]
        avalue = result[akey]
        response = []
        for i in range(0, len(avalue)):
            tempres = {}
            for key in keys:
                tempres[key] = result[key][i]
            response.append(tempres)
        prob = []
        for ele in response:
            subs = {}
            for var in listOfSubstitutionVars:
                if var in ele.keys():
                    subs[var] = ele[var]
            dcQuery = self.generateDCQuery(body, subs)
            #query = 'python ../yapInterface/YapInterface.py ' + '"' + self.dcFileName + '" ' + str(self.numOfSamples)+ ' "(' + dcQuery + ')"'
            #outputString = Popen(query, stdout=PIPE, shell=True).communicate()
            #p = float(outputString[0][0:-1])
            probAndSamples = self.queryWithSamples(self.numOfSamples, "(" + dcQuery + ")", "", stringOfContVars)
            samples = probAndSamples['samples']
            probability = probAndSamples['probability']
            listSamples = self.convertStringReprOfListToList(samples)
            acceptedSamples = []
            sumW1 = 0
            for i in range(0, len(listSamples)):
                if listSamples[i][2] == 0:
                    pass
                else:
                    acceptedSamples.append(listSamples[i])
                sumW1 += listSamples[i][1]
            for i in range(0, len(acceptedSamples)):
                p = (acceptedSamples[i][1] * acceptedSamples[i][2])/sumW1
                acceptedSamples[i].append(p)
            s = str(probability) + ' ' + dcQuery + ' ' + str(acceptedSamples)
            self.yapPrologInterface_logger.info(s)
            prob.append(acceptedSamples)   
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
        return self.yapObject
    
if __name__ == '__main__':
#     obj = YapPrologInterface('../data/interp1_train.pl')
#     res = obj.simpleQuery('intelligence(S, X1).')
#     for ele in res:
#         print ele['S'], ele['X1']
#     
#     obj = YapPrologInterface('../data/interp1_train.pl')
#     res = obj.listQuery('teaches(P, C), ability(P, X3).', ['C', 'P', 'X3'])
#     print res
#     
#     obj1 = YapPrologInterface('../yapInterface/data/cancer.pl')
#     numOfSamples = 1000
#     query = '(grade(carl)~=X, X>68)'
#     print obj1.prolog.query(numOfSamples, query, "")
    
    
#     obj = YapPrologInterface('../data/interp1_train.pl')
#     #out = obj.convertStringReprOfListToList('[[minMod, (rel12(-,+),ztt(+,-)), 3.4554, 2, discrete, [zero, one, two, three, four]], [minMod, (rel11(-,+),activity(+,-)), 2023.8765, 2, discrete, [0.1, 1.3, 2.5, 3, 4.3]], [none, sex(+,-), 33.94, 21, continuous, []]]')
#     out = obj.convertStringReprOfListToList('[]')
#     print out
    #res = obj.listQuery('intelligence(S, X_1), health(S, X2), satScore(S, X3), X3 > 50.', ['S', 'X_1', 'X2', 'X3'])
    #print res
    #res = obj.listQuery('satScore(S, X), intelligence(S, X1), health(S, X2), maxMod(V, grade(S, C, V), I1), I1 == low.', ['X', 'X1', 'X2'])
    #res = obj.listQuery('satScore(S, X).', ['S', 'X'])
    #print res
    
    obj = YapPrologInterface()
    obj.consultWithOneFile('../program/testSampling.pl')
    obj.setNumOfSamples(10)
    body = 'stress(S, Y), rating(S, Z)'
    query = 'smokes(S, X),stress(S, Y), rating(S, Z)'
    res = obj.getSamplesAndProbabilityOfQuery(query, body, ['S','X'], ['Z','Y'])
    print res
    
    
    #obj = YapPrologInterface()
    #obj.consultWithTwoFiles('../data/hepatitis.pl', '../data/dcHepatitis.pl', 100)
    #print obj.query(100, '(findall_forward(Tc_M,(rel12(In_M,1),tcho(In_M)~=Tc_M),X_T_1),minMod(X_T_1)~=tcho0)', '')

    #obj = YapPrologInterface('../data/prologUniv.pl')
#     obj.initiateDC('../data/dcUniv.pl', 100)
# #     query = 'intelligence(S,I), maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high'
# #     body = 'maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high'
# #     listOfSubstitutionVars = ['S']
# #     probs = obj.getProbabilityOfQuery(query, body, listOfSubstitutionVars)
# #     print probs
#     
    ########## Generate DC clauses tests ##############
    #body = 'intelligence(S,I), maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high, \+maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), \+avg(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), avg(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3>10, minMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == high, maxMod(R_M, (registration(C_M,S), rating(C_M, R_M)), X_T_3), X_T_3 == low'
    #body = '\+minMod(Du_M,(rel13(A_M,1),dur(A_M,Du_M)),X_T_30),maxMod(Zt_M,(rel12(In_M,1),ztt(In_M,Zt_M)),X_T_276),X_T_276 == a,minMod(Zt_M_1,(rel12(In_M_1,1),ztt(In_M_1,Zt_M_1)),X_T_311), X_T_311 == a'
    #subs = {'S': 4}
    #dcQuery = obj.generateDCQuery(body, subs)
    #print dcQuery
    ###################################################

    #for i in range(0, 10):
    #    outputString = Popen('python ../yapInterface/YapInterface.py "../data/dcUniv.pl" 100 "(maxMod(R_M, (registration(C_M,14), rating(C_M)~=R_M), X_T_3), X_T_3 == high)"', stdout=PIPE, shell=True).communicate()
    #    print float(outputString[0][0:-1])
    #obj = YapInterface("../data/dcUniv.pl")
    #obj.test()