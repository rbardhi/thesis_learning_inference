from yapWrapper import PyDC
import ast
import pickle
import sys
import math
import numpy as np
from sklearn.covariance import LedoitWolf

NUM_SAMPLES = 100
FLAG = 0
BIGNUM = 10000000
IN_NAME = 'test.pl'
OUT_NAME = 'results_%s_%s.pkl'
DIR_PATH = '../data/cross_validation/'
OUT_PATH = '../../new_experiments_test/results/'

#TODO error here is that diferent number of objects are present and numpy complaints when appending results

#To fix: with the evidence we will get the id of the object that moves so that we allwas query one value, no more covarianse, just standart deviation

def reshape(obj):
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
    #X = np.random.multivariate_normal(mean=avg,cov=cov,size=100)
    #shcov = LedoitWolf().fit(X)
    #assert cov is positive-semidefinite
    try:
      assert(np.all(np.linalg.eigvals(cov) >= 0))
    except AssertionError:
      X = np.random.multivariate_normal(mean=avg,cov=cov,size=100)
      shcov = LedoitWolf().fit(X)
      avg, cov = shcov.location_, shcov.covariance_
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
  return None #TODO
    
def query_pos_t1_given_pos_t0(j, index, evidence,twoD):
  EVDs = ast.literal_eval(evidence)
  #init PyDC
  pydc = PyDC(DIR_PATH + IN_NAME)
  for i, EVD in enumerate(EVDs):
    evd, params = EVD
    if i == 0:
      totX, totstdX = query_posX_t1(pydc,evd,rng)
      totDX, totstdDX = query_displX(pydc,evd,rng)
      totX, totstdX = reshape(totX), reshape(totstdX)
      totDX, totstdDX = reshape(totDX), reshape(totstdDX)
      if twoD:
        totY, totstdY = query_posY_t1(pydc,evd,rng)
        totDY, totstdDY = query_displY(pydc,evd,rng)
        totY, totstdY = reshape(totY), reshape(totstdY)
        totDY, totstdDY = reshape(totDY), reshape(totstdDY)
    else:
      avgX, stdX = query_posX_t1(pydc,evd,rng)
      avgDX, stdDX = query_displX(pydc,evd,rng)
      avgX, stdX = reshape(avgX), reshape(stdX)
      avgDX, stdDX = reshape(avgDX), reshape(stdDX)
      totDX = np.append(totDX, avgDX, axis=0)
      totX = np.append(totX, avgX, axis=0)
      totstdX = np.append(totstdX, stdX, axis=0)
      totstdDX = np.append(totstdDX, stdDX, axis=0)
      if twoD:
        avgY, stdY = query_posY_t1(pydc,evd,rng)
        avgDY, stdDY = query_displY(pydc,evd,rng)
        avgY, stdY = reshape(avgY), reshape(stdY)
        avgDY, stdDY = reshape(avgDY), reshape(stdDY)
        totDY = np.append(totDY, avgDY, axis=0)
        totY = np.append(totY, avgY, axis=0)
        totstdY = np.append(totstdY, stdY, axis=0)
        totstdDY = np.append(totstdDY, stdDY, axis=0)
  #terminate PyDC
  pydc.terminate() 
  if twoD:
    with open(OUT_PATH + OUT_NAME % (str(j),index),'wb') as f:
      pickle.dump([(totDX,totstdDX)] + [(totX,totstdX)] + [(totDY,totstdDY)] + [(totY,totstdY)], f) 
  else:
    with open(OUT_PATH + OUT_NAME % (str(j),index),'wb') as f:
      pickle.dump([(totDX,totstdDX)] + [(totX,totstdX)], f) 

def query_pos_t1_given_pos_t0_and_action(j,index, evidence,twoD):
  EVDs = ast.literal_eval(evidence)
  #init PyDC
  pydc = PyDC(DIR_PATH + IN_NAME)
  for i, EVD in enumerate(EVDs):
    evd, rng = EVD
    if i == 0:
      totX, totstdX = query_posX_t1(pydc,evd,rng)
      totX, totstdX = reshape(totX), reshape(totstdX)
      if twoD:
        totY, totstdY = query_posY_t1(pydc,evd,rng)
        totY, totstdY = reshape(totY), reshape(totstdY)
    else:
      avgX, stdX = query_posX_t1(pydc,evd,rng)
      avgX, stdX = reshape(avgX), reshape(stdX)
      totX = np.append(totX,avgX, axis=0)
      totstdX = np.append(totstdX,stdX, axis=0)
      if twoD:
        avgY, stdY = query_posY_t1(pydc,evd,rng)
        avgY, stdY = reshape(avgY), reshape(stdY)
        totY = np.append(totY, avgY, axis=0)
        totstdY = np.append(totstdY, stdY, axis=0)
  #terminate PyDC
  pydc.terminate()  
  if twoD:
    with open(OUT_PATH + OUT_NAME % (str(j),index),'wb') as f:
      pickle.dump([(totX,totstdX)] + [(totY,totstdY)], f)
  else:
    with open(OUT_PATH + OUT_NAME % (str(j),index),'wb') as f:
      pickle.dump((totX,totstdX), f)

def query_action_given_pos_t0_and_pos_t1(j,index, evidence,twoD):
  EVDs = ast.literal_eval(evidence)
  #init PyDC
  pydc = PyDC(DIR_PATH + IN_NAME)
  for i, EVD in enumerate(EVDs):
    evd, rng = EVD
    if i == 0:
      totDX, totstdDX = query_displX(pydc,evd,rng)
      totDX, totstdDX = reshape(totDX), reshape(totstdDX)
      if twoD:
        totDY, totstdDY = query_displY(pydc,evd,rng)
        totDY, totstdDY = reshape(totDY), reshape(totstdDY)
    else:
      avgDX, stdDX = query_displX(pydc,evd,rng)
      avgDX, stdDX = reshape(avgDX), reshape(stdDX)
      totDX = np.append(totDX,avgDX, axis=0)
      totstdDX = np.append(totstdDX,stdDX, axis=0)
      if twoD:
        avgDY, stdDY = query_displY(pydc,evd,rng)
        avgDY, stdDY = reshape(avgDY), reshape(stdDY)
        totDY = np.append(totDY, avgDY, axis=0)
        totstdDY = np.append(totstdDY, stdDY, axis=0)
  #terminate PyDC
  pydc.terminate()  
  if twoD:
    with open(OUT_PATH + OUT_NAME % (str(j),index),'wb') as f:
      pickle.dump([(totDX,totstdDX)] + [(totDY,totstdDY)], f)
  else:
    with open(OUT_PATH + OUT_NAME % (str(j),index),'wb') as f:
      pickle.dump((totDX,totstdDX), f) 

if __name__ == '__main__':
  with open(sys.argv[3],'r') as f:
    evidence = f.read()
  twoD = ast.literal_eval(sys.argv[4])
  if sys.argv[1] == str(1):
    query_pos_t1_given_pos_t0(1,sys.argv[2],evidence,twoD)
  elif sys.argv[1] == str(0):
    query_pos_t1_given_pos_t0_and_action(0,sys.argv[2],evidence,twoD)
  elif sys.argv[1] == str(2):
    query_action_given_pos_t0_and_pos_t1(2,sys.argv[2],evidence,twoD)

