import ast
import pickle
import sys
import math
import numpy as np
from sklearn.covariance import LedoitWolf
from utils import draw_example_new
sys.path.insert(1,'../../core/')
from yapWrapper import PyDC

NUM_SAMPLES = 100
FLAG = 0
BIGNUM = 10000000
IN_NAME = 'test.pl'

'''def reshape(obj):
  if len(obj.shape) == 1:
    return np.reshape(obj, (1, obj.shape[0]))
  elif len(obj.shape) == 2:
    return np.reshape(obj, (1, obj.shape[0], obj.shape[1]))

def weighted_avg_and_std(values, weights):
  avg = np.average(values, axis=0, weights=weights)
  var = np.average((values-avg)**2, axis=0, weights=weights)
  return (avg, np.array(map(lambda v:math.sqrt(v),var)))
def weighted_avg_and_cov(values, weights):
  avg = np.average(values, axis=0, weights=weights)
  cov = np.cov(values, rowvar=False, aweights=weights, ddof=0)
  return (avg, cov) #cov.shape = (3,3)
  
def query_samples_and_probabilities(pydc,query,evidence,var,std=False):
  pydc.queryWithSamples(NUM_SAMPLES,query,evidence,var,FLAG,BIGNUM)
  parsed_samples = ast.literal_eval(pydc.samples)
  values = []
  weights = []
  for sample in parsed_samples:
    x, w = sample[0], sample[1]
    values  += [x]
    weights += [w]
  values, weights = np.array(values), np.array(weights)
  
  if std:
    avg, std = weighted_avg_and_std(values, weights)
    return avg, std
  else:
    #values = values + 1e-5*np.random.rand(*values.shape)
    avg, cov = weighted_avg_and_cov(values, weights)
    print avg
    print '\n'
    #X = np.random.multivariate_normal(mean=avg,cov=cov,size=100)
    #shcov = LedoitWolf().fit(X)
    #assert cov is positive-semidefinite
    #try:
    #  assert(np.all(np.linalg.eigvals(cov) >= 0))
    #except AssertionError:
    #  X = np.random.multivariate_normal(mean=avg,cov=cov,size=100)
    #  shcov = LedoitWolf().fit(X)
    #  avg, cov = shcov.location_, shcov.covariance_
      #assert(np.all(np.linalg.eigvals(cov) >= 0))
    return (avg, cov)

def query_posX_t1(pydc,evidence,rng):
  query = '(findall_forward(X, (between({},{},W), n(W)~=N, N1 is N - 1, between(0,N1,O), posX_t1(W,O)~=X), L))'.format(rng[0],rng[1])
  var = 'L'
  return query_samples_and_probabilities(pydc,query,evidence,var,std=False)
def query_posY_t1(pydc,evidence,rng):
  query = '(findall_forward(X, (between({},{},W), n(W)~=N, N1 is N - 1, between(0,N1,O), posY_t1(W,O)~=X), L))'.format(rng[0],rng[1])
  var = 'L'
  return query_samples_and_probabilities(pydc,query,evidence,var,std=False)
def query_displX(pydc,evidence,rng):
  query = '(findall_forward(X, (between({},{},W), n(W)~=N, N1 is N - 1, between(0,N1,O), displX(W,O)~=X), L))'.format(rng[0],rng[1])
  var = 'L'
  return query_samples_and_probabilities(pydc,query,evidence,var,std=False) 
def query_displY(pydc,evidence,rng):
  query = '(findall_forward(X, (between({},{},W), n(W)~=N, N1 is N - 1, between(0,N1,O), displY(W,O)~=X), L))'.format(rng[0],rng[1])
  var = 'L'
  return query_samples_and_probabilities(pydc,query,evidence,var,std=False)  
def query_move_left_of(pydc,evidence):
  query = '(findall_forward([I1,I2,B],move_left_of(W,I1,I2)~=B,L))'
  var = 'L'    
  return None #TODO'''
    
'''def query_pos_t1_given_pos_t0(pydc,evd,rng):
  totX, totstdX = query_posX_t1(pydc,evd,rng)
  return (totX, totstdX)
def new_query(pydc,evd,rng):
  totD, totstdD = query_displX(pydc,evd,rng)
  return (totD, totstdD) '''  
def weighted_avg_and_std(values, weights):
  avg = np.average(values, axis=0, weights=weights)
  var = np.average((values-avg)**2, axis=0, weights=weights)
  return (avg, math.sqrt(var))
  
def query_samples_and_probabilities(pydc,query,evidence,var):
  pydc.queryWithSamples(NUM_SAMPLES,query,evidence,var,FLAG,BIGNUM)
  parsed_samples = ast.literal_eval(pydc.samples)
  values = []
  weights = []
  for sample in parsed_samples:
    x, w = sample[0], sample[1]
    values  += [x]
    weights += [w]
  values, weights = np.array(values), np.array(weights)
  avg, std = weighted_avg_and_std(values, weights)
  return avg, std
      
def query_displX(pydc,evidence,W,I):
  avgD,stdD = [], []
  var = 'X'
  for i in range(I):
    query = '(displX({},{})~=X)'.format(W,i)
    avg, std = query_samples_and_probabilities(pydc, query, evidence, var)
    avgD += [avg]
    stdD += [std] 
  return avgD, stdD
  
def query_displY(pydc,evidence,W,I):
  avgD,stdD = [], []
  var = 'Y'
  for i in range(I):
    query = '(displY({},{})~=Y)'.format(W,i)
    avg, std = query_samples_and_probabilities(pydc, query, evidence, var)
    avgD += [avg]
    stdD += [std] 
  return avgD, stdD     
 
def main(evidence,examples,dir):
  pydc = PyDC(dir+IN_NAME)
  EVDs = ast.literal_eval(evidence)
  ids = [7940,3489]
  for EVD, example in zip(EVDs,examples):
    #if not example.init_state.id in ids:
    #  continue
    evd, param = EVD
    W,I = param
    DX,_ = query_displX(pydc,evd,W,I)
    DY,_ = query_displY(pydc,evd,W,I)
    DX[:] = [d if abs(d)>0.1 else 0 for d in DX]
    DY[:] = [d if abs(d)>0.1 else 0 for d in DY]
    X = [obj.x for obj in example.init_state.objects]
    Y = [obj.y for obj in example.init_state.objects]
    X1 = [x+d for x,d in zip(X,DX)]
    Y1 = [y+d for y,d in zip(Y,DY)]
    print(DX)
    print(DY)
    draw_example_new(example,X1,Y1)
  
  pydc.terminate()
if __name__ == '__main__':
  dir = '2D_hier/constrained/'#'2D_hier/simple/'
  with open(dir+'evidence_file','r') as f:
    evidence = f.read()
  with open(dir+'examples_file','rb') as f:
    examples = pickle.load(f)
  twoD = True
  main(evidence,examples,dir)

