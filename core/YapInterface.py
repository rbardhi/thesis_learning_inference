import sys
from yapWrapper import PyDC
## After executing from command line : $ python setup.py build_ext --inplace

BIGNUM = 1000000
VARIABLE_NUMBER = 3

class YapInterface(object):
    
    def __init__(self):
        self.yapObject = None
        self.yapFileName = []
    
    def consultWithOneFile(self, yapFileName):
        self.yapObject = PyDC(yapFileName)
        self.yapFileName.append(yapFileName)
    
    def consultWithTwoFiles(self, yapFileName1, yapFileName2):
        self.yapObject = PyDC(yapFileName1, yapFileName2)
        self.yapFileName.append(yapFileName1)
        self.yapFileName.append(yapFileName2)
        
    def query(self, numOfSamples, query, evidence):
        probability = self.yapObject.query(numOfSamples, query, evidence)
        return probability
    
    def sample(self, numOfSamples, query, evidence, variable):
        return self.yapObject.sample(numOfSamples, query, evidence, variable, BIGNUM)
    
    def prologQuery(self, query):
        st = self.yapObject.prologQuery(VARIABLE_NUMBER, query, BIGNUM)
        return st
        
if __name__ == '__main__':
    #dcFileName = sys.argv[1]
    #obj = YapInterface(dcFileName)
    #variableNumber = int(sys.argv[2])
    #query = sys.argv[3]
    obj = YapInterface()
    obj.consultWithTwoFiles('../data/hepatitis.pl', '../data/dcHepatitis.pl')
    print obj.query(100, '(findall_forward(Tc_M,(rel12(In_M,1),tcho(In_M)~=Tc_M),X_T_1),minMod(X_T_1)~=tcho0)', '')
    print obj.query(1000, '(findall_forward(Tc_M,(rel12(In_M,2),tcho(In_M)~=Tc_M),X_T_1),minMod(X_T_1)~=tcho0)', '')
    #print obj.query(1000, '(findall_forward(Tc_M,(rel12(In_M,1),tcho(In_M)~=Tc_M),X_T_2),minMod(X_T_2)~=tcho1)', '')
    print obj.query(1000, '(findall_forward(Tc_M,(rel12(In_M,2),tcho(In_M)~=Tc_M),X_T_2),minMod(X_T_2)~=tcho1)', '')
    #print obj.query(1000, '(findall_forward(Tc_M,(rel12(In_M,1),tcho(In_M)~=Tc_M),X_T_3),minMod(X_T_3)~=tcho2)', '')
    print obj.query(1000, '(findall_forward(Tc_M,(rel12(In_M,2),tcho(In_M)~=Tc_M),X_T_3),minMod(X_T_3)~=tcho2)', '')
    #print obj.query(1000, '(findall_forward(Tc_M,(rel12(In_M,1),tcho(In_M)~=Tc_M),X_T_4),minMod(X_T_4)~=tcho3)', '')
    print obj.query(1000, '(findall_forward(Tc_M,(rel12(In_M,2),tcho(In_M)~=Tc_M),X_T_4),minMod(X_T_4)~=tcho3)', '')
    print obj.sample(10, '(findall_forward(Tc_M,(rel12(In_M,2),tcho(In_M)~=Tc_M),X_T_1),minMod(X_T_1)~=XXX)', "[]", "[X_T_1,XXX]")
    #obj2 = YapInterface()
    #obj2.consult('data/friends.pl')
    #query = 'findall([S,I,X], (intelligence(S,I), maxMod(G, grade(S,C,G), X)), FINDALL).'
    #print obj2.prologQuery(query)
     
#     obj1 = YapInterface()
#     obj1.consult('data/cancer.pl')
#     numOfSamples = 1000
#     query = '(grade(carl)~=X, X>68)'
#     print obj1.query(numOfSamples, query, "")
#     
#     obj3 = YapInterface()
#     obj3.consult('../data/interp1_train.pl')
#     query = 'findall([C,P,X3],(teaches(P, C), ability(P, X3)), FINDALL).'
#     print obj3.prologQuery(query)
#     query = 'findall([S,I,X], (intelligence(S,I), maxMod(G, grade(S,C,G), X)), FINDALL).'
#     print obj2.prologQuery(query)
    #core.cleanupProlog()
    #obj = YapInterface("../data/dcUniv.pl")
    #obj = PyDC("../data/dcUniv.pl") 
    #print obj.query(100, "(maxMod(R_M, (registration(C_M,14), rating(C_M)~=R_M), X_T_3), X_T_3 == high)", "")
    #a = obj.query(100, "(maxMod(R_M, (registration(C_M,14), rating(C_M)~=R_M), X_T_3), X_T_3 == high)", "")
    #print a
    #b = obj.sample(10, "(intelligence(41) ~= X)", "[]", "[X]")
    #print b
