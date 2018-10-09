from libcpp.string cimport string
from libc.stdlib cimport free
from libcpp cimport bool
    
cdef extern from "dc.h":
    ctypedef struct probAndSamples:
        double prob
        char* samples
    cdef cppclass dc:
        dc() except +
        dc(string file) except +
        dc(string file1, string file2) except +
        bool execute(string q)
        double query(int n, string query,string evidence)
        char* sample(int n, string query, string evidence, string variable, unsigned int size)
        probAndSamples* queryWithSamples(int n, string query, string evidence, string vars, int flag, unsigned int size)
        char* prologQuery(int n, string query, unsigned int size)

cdef class PyDC:
    cdef dc c_dc      
    probSamplesDict = dict()
    def __cinit__(self, *file):
        if len(file) == 1:
            self.c_dc = dc(file[0])
        else:
            self.c_dc = dc(file[0], file[1])    
    def query(self, n, query, evidence):
        return self.c_dc.query(n, query, evidence)
#     def sample(self, n, query, evidence, variable, size):
#         return self.c_dc.sample(n, query, evidence, variable, size)
#     def prologQuery(self, n, query, size):
#         return self.c_dc.prologQuery(n, query, size)

    def execute(self, execQuery):
        self.c_dc.execute(execQuery)
        
    def sample(self, n, query, evidence, variable, size):
        samplePython = None
        cdef char* samples
        try:
            samples = self.c_dc.sample(n, query, evidence, variable, size)
            samplePython = samples
        finally:
            free(samples)
        return samplePython
    def prologQuery(self, n, query, size):
        prologQuery = None
        cdef char* results
        try:
            results = self.c_dc.prologQuery(n, query, size)
            prologQuery = results
        finally:
            free(results)
        return prologQuery
    def queryWithSamples(self, n, query, evidence, vars, flag, size):
        cdef probAndSamples* probSamples
        try:
            probSamples = self.c_dc.queryWithSamples(n, query, evidence, vars, flag, size)
            self.probSamplesDict['prob'] = probSamples.prob
            self.probSamplesDict['samples'] = probSamples.samples   
        finally:
            free(probSamples.samples)
            free(probSamples)
    @property
    def prob(self):
       return self.probSamplesDict['prob']
    @property
    def samples(self):
        return self.probSamplesDict['samples']
