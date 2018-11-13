from yapWrapper import PyDC

## After executing from command line : $ python setup.py build_ext --inplace

BIGNUM = 1000000
#obj = PyDC("data/cancer.pl")
#a = obj.query(1000, "(smokes(carl)~=true)", "")
#print a
#b = obj.sample(10, "(grade(carl)~=X, height(carl)~=Y)", "[stress(carl)~=true]", "[X,Y]", BIGNUM)
#print b

#obj = PyDC("../DCRuleLearningTestBed/program/testSampling.pl")

obj = PyDC('../data/Financial.pl', '../data/FinancialDC.pl')
#query = "findall([X,I,C,M],(intelligence(X,I),maxMod(X1, grade(X,C,X1), M)),FINDALL)."
#a = obj.prologQuery(3, query, BIGNUM)

#query = "findall([X],(person(X)),FINDALL)."
#a = obj.prologQuery(3, query, BIGNUM)
#print a 

query = '(loanAmount(l_7091)~=La_M)'
obj.queryWithSamples(1000, query, '', '[La_M]', 0, BIGNUM)


print obj.prob
print obj.samples
obj.terminate()

#obj.execute('halt.')
print 'hello'
