'''
Created on Aug 28, 2018

@author: nitesh
'''
import scipy.stats
import logging, time
import statistics
import copy, math
from os import listdir
from os.path import isfile, join
from YapPrologInterface import YapPrologInterface
from TreeLearnerProbabilistic import TreeLearnerProbabilistic
from TranslateToDC import TranslateToDC
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score
from collections import Counter
import numpy as np
from sklearn.metrics import roc_auc_score

FOLD = 1
#DATA_DIRECTORY = '/home/nitesh/eclipse-workspace/DCRuleLearningTestBed/data/PKDD/pkdd'
#PROCESSED_TRAIN_DATA_FILE = '../data/Financial.pl'
#PROCESSED_TRAIN_DATA_FILE_DC = '../data/FinancialDC.pl'

DATA_DIRECTORY = '/home/nitesh/eclipse-workspace/DCRuleLearningTestBed/data/University/data800x125x125'
PROCESSED_TRAIN_DATA_FILE = '../data/University.pl'
PROCESSED_TRAIN_DATA_FILE_DC = '../data/UniversityDC.pl'

OUTPUT_ENSEMBLE_OF_DLTS = '../data/EnsembleOfDLTs.pl'
DATABASE_NAME = 'financial'

DC_RULES = []
TRAIN_FACTS = []
TEST_FACTS = []

foldString = "Fold" + str(FOLD)

RELATIONAL_PREDICATES = dict(
    financial = ['hasLoan', 'clientDistrict', 'hasAccount'],
    university = ['takes', 'friend', 'teaches']
)
RANDOM_VARIABLE_PREDICATE = dict(
    financial = ['hasLoan', 'clientDistrict', 'hasAccount', 'clientLoan', 'loanAmount', 'loanStatus','monthlyPayments', 'gender', 'age', 'freq', 'avgSalary', 'ratUrbInhab', 'avgNrWith', 'avgSumOfInc', 'avgSumOfW', 'stdMonthInc', 'stdMonthW'],
    university = ['nrhours', 'difficulty', 'ability', 'intelligence', 'grade', 'satisfaction', 'takes', 'friend', 'teaches']
)

RANDOM_VARIABLE_PREDICATE_TYPE = dict(
    financial = dict(
        hasLoan = 'discrete',
        clientDistrict = 'discrete',
        hasAccount = 'discrete',
        clientLoan = 'discrete',
        loanAmount = 'continuous',
        loanStatus = 'discrete',
        monthlyPayments = 'continuous',
        gender = 'discrete',
        age = 'continuous',
        freq = 'discrete',
        avgSalary = 'continuous',
        ratUrbInhab = 'continuous',
        avgNrWith = 'continuous',
        avgSumOfInc = 'continuous',
        avgSumOfW = 'continuous',
        stdMonthInc = 'continuous',
        stdMonthW = 'continuous'
    ),
    university = dict(
        nrhours = 'continuous',
        difficulty = 'discrete', 
        ability = 'continuous',
        intelligence = 'continuous', 
        grade = 'discrete',
        satisfaction = 'discrete',
        takes = 'discrete',
        friend = 'discrete',
        teaches = 'discrete'
    )
)

RANDOM_VARIABLE_PREDICATE_RANGE = dict(
    financial = dict(
        loanAmount = 585840,
        monthlyPayments = 9606,
        age = 55,
        avgSalary = 4431,
        ratUrbInhab = 66.1,
        avgNrWith = 42.1667,
        avgSumOfInc = 319796.45,
        avgSumOfW = 299106.333,
        stdMonthInc = 92689.04822,
        stdMonthW = 77248.6264725725,
        loanStatus = ['a','b','c','d'],
        gender = ['m', 'f'],
        freq = ['i', 'w', 'm'],
        hasLoan = ['true', 'false'],
        clientDistrict = ['true', 'false'],
        hasAccount = ['true', 'false'],
        clientLoan = ['true', 'false']
    ), 
    university = dict(
        nrhours = 160.0,
        difficulty = ['medium','hard','easy'], 
        ability = 80.0,
        intelligence = 130.0, 
        grade = ['high','low','mid'],
        satisfaction = ['mid','low','high'],
        takes = ['true', 'false'],
        friend = ['true', 'false'],
        teaches = ['true', 'false']
    )
)


DECLARATIVE_BIAS_ORDERED = dict(
    financial = '''
:- discontiguous client/1.
:- discontiguous loan/1.
:- discontiguous hasLoan/3.
:- discontiguous clientDistrict/3.
:- discontiguous hasAccount/3.
:- discontiguous district/1.
:- discontiguous account/1.
:- discontiguous loanAmount/2.
:- discontiguous loanStatus/2.
:- discontiguous monthlyPayments/2.
:- discontiguous gender/2.
:- discontiguous clientLoan/3.
:- discontiguous age/2.
:- discontiguous freq/2.
:- discontiguous avgSalary/2.
:- discontiguous ratUrbInhab/2.
:- discontiguous avgNrWith/2.
:- discontiguous avgSumOfInc/2.
:- discontiguous avgSumOfW/2.
:- discontiguous stdMonthInc/2.
:- discontiguous stdMonthW/2.

%Relational Lookahead
districtClientAccount(D,C,A) :- clientDistrict(C,D,X1), X1==true, hasAccount(C,A,X2), X2==true.
districtClientLoan(D,C,L) :- clientDistrict(C,D,X1), X1==true, clientLoan(C,L,X2), X2==true.

%Types
base(client(c)).
base(loan(l)).
base(hasLoan(a,l,x1)).
base(districtClientAccount(d,c,a)).
base(districtClientLoan(d,c,l)).
base(clientDistrict(c,d,x2)).
base(hasAccount(c,a,x3)).
base(district(d)).
base(account(a)).
base(loanAmount(l,la)).
base(loanStatus(l,ls)).
base(monthlyPayments(l,mp)).
base(gender(c,ge)).
base(age(c,ca)).
base(clientLoan(c,l,x4)).
base(freq(a,afr)).
base(avgSalary(d,dav)).
base(ratUrbInhab(d,drat)).
base(avgNrWith(a,aavn)).
base(avgSumOfInc(a,aavsi)).
base(avgSumOfW(a,aavw)).
base(stdMonthInc(a,astdmi)).
base(stdMonthW(a,astdmw)).
  
%Modes
mode(avgSalary, cnt, districtClientAccount(+,-,-)).
%For order% mode(avgSalary, none, ratUrbInhab(+,-)).
mode(avgSalary, maxMod, (clientDistrict(-,+,true),gender(+,-))).
mode(avgSalary, avg, (clientDistrict(-,+,true),age(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
%For order% mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
%For order% mode(avgSalary, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
mode(avgSalary, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
mode(avgSalary, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
mode(avgSalary, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(ratUrbInhab, cnt, districtClientAccount(+,-,-)).
mode(ratUrbInhab, none, avgSalary(+,-)).
mode(ratUrbInhab, maxMod, (clientDistrict(-,+,true),gender(+,-))).
mode(ratUrbInhab, avg, (clientDistrict(-,+,true),age(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
%For order% mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
%For order% mode(ratUrbInhab, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
mode(ratUrbInhab, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(gender, cnt, clientDistrict(+,-,true)).
mode(gender, cnt, hasAccount(+,-,true)).
mode(gender, none, age(+,-)).
%For order% mode(gender, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%For order% mode(gender, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(gender, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(gender, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(gender, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(gender, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%mode(gender, avg, (clientLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(age, cnt, clientDistrict(+,-,true)).
mode(age, cnt, hasAccount(+,-,true)).
mode(age, cnt, clientLoan(+,-,true)).
%For order% mode(age, none, gender(+,-)).
%For order% mode(age, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%For order% mode(age, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(age, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(age, maxMod, (hasAccount(+,-,true),freq(+,-))).
%For order% mode(age, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%For order% mode(age, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%For order% mode(age, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(loanAmount, cnt, hasLoan(-,+,true)).
mode(loanAmount, cnt, districtClientLoan(-,-,+)).
%For order% mode(loanAmount, none, loanStatus(+,-)).
%For order% mode(loanAmount, none, monthlyPayments(+,-)).
%For order% mode(loanAmount, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanAmount, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfW(+,-))). %Commented
%For order% mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
%For order% mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgNrWith(+,-))). %Commented
%For order% mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
%For order% mode(loanAmount, maxMod, (hasLoan(-,+,true),freq(+,-))).
%For order% mode(loanAmount, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
%For order% mode(loanAmount, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(loanStatus, cnt, hasLoan(-,+,true)).
mode(loanStatus, cnt, districtClientLoan(-,-,+)).
mode(loanStatus, none, loanAmount(+,-)).
%For order% mode(loanStatus, none, monthlyPayments(+,-)).
%For order% mode(loanStatus, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanStatus, avg, (clientLoan(-,+,true),age(+,-))).
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgNrWith(+,-))). %Commented
%For order% mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
%For order% mode(loanStatus, maxMod, (hasLoan(-,+,true),freq(+,-))).
%For order% mode(loanStatus, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
%For order% mode(loanStatus, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(monthlyPayments, cnt, hasLoan(-,+,true)).
mode(monthlyPayments, cnt, districtClientLoan(-,-,+)).
mode(monthlyPayments, none, loanAmount(+,-)).
mode(monthlyPayments, none, loanStatus(+,-)).
mode(monthlyPayments, maxMod, (clientLoan(-,+,true),gender(+,-))). %commented
mode(monthlyPayments, avg, (clientLoan(-,+,true),age(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfW(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthInc(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthW(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgNrWith(+,-))). %commented
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))). %commented
mode(monthlyPayments, maxMod, (hasLoan(-,+,true),freq(+,-))). %commented
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),avgSalary(+,-))). %commented
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))). %commented

%For order% mode(avgSumOfW, none, stdMonthInc(+,-)).
%For order% mode(avgSumOfW, none, stdMonthW(+,-)).
%For order% mode(avgSumOfW, none, avgNrWith(+,-)).
%For order% mode(avgSumOfW, none, avgSumOfInc(+,-)).
%For order% mode(avgSumOfW, none, freq(+,-)).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfW, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgSumOfW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgSumOfW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(avgSumOfW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthInc, none, avgSumOfW(+,-)).
%For order% mode(stdMonthInc, none, stdMonthW(+,-)).
mode(stdMonthInc, none, avgNrWith(+,-)). %Commented
%For order% mode(stdMonthInc, none, avgSumOfInc(+,-)).
%For order% mode(stdMonthInc, none, freq(+,-)).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%mode(stdMonthInc, maxMod, (hasAccount(-,+,true),gender(+,-))). %uncommented
mode(stdMonthInc, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(stdMonthInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(stdMonthW, none, avgSumOfW(+,-)).
mode(stdMonthW, none, stdMonthInc(+,-)).
mode(stdMonthW, none, avgNrWith(+,-)).
%mode(stdMonthW, none, avgSumOfInc(+,-)). %Uncommented
mode(stdMonthW, none, freq(+,-)).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%mode(stdMonthW, maxMod, (hasAccount(-,+,true),gender(+,-))). %uncommented
mode(stdMonthW, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(stdMonthW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(avgNrWith, none, avgSumOfW(+,-)).
mode(avgNrWith, none, stdMonthInc(+,-)).
mode(avgNrWith, none, stdMonthW(+,-)).
%For order% mode(avgNrWith, none, avgSumOfInc(+,-)).
mode(avgNrWith, none, freq(+,-)).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgNrWith, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgNrWith, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgNrWith, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgNrWith, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(avgNrWith, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(avgSumOfInc, none, avgSumOfW(+,-)).
mode(avgSumOfInc, none, stdMonthInc(+,-)).
mode(avgSumOfInc, none, stdMonthW(+,-)).
mode(avgSumOfInc, none, avgNrWith(+,-)).
mode(avgSumOfInc, none, freq(+,-)).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfInc, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfInc, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgSumOfInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgSumOfInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(avgSumOfInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

mode(freq, none, avgSumOfW(+,-)).
%mode(freq, none, stdMonthInc(+,-)). %uncommented
%mode(freq, none, stdMonthW(+,-)). %uncommented
%For order% mode(freq, none, avgNrWith(+,-)).
%For order% mode(freq, none, avgSumOfInc(+,-)).
mode(freq, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(freq, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%mode(freq, maxMod, (hasAccount(-,+,true),gender(+,-))). %uncommented
mode(freq, avg, (hasAccount(-,+,true),age(+,-))).
mode(freq, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(freq, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
%mode(freq, avg, (hasLoan(+,-,true),monthlyPayments(+,-))). %Uncommented

%For order% mode(clientDistrict, none, age(+,-)).
%For order% mode(clientDistrict, none, gender(+,-)).
%For order% mode(clientDistrict, none, avgSalary(+,-)).
%For order% mode(clientDistrict, none, ratUrbInhab(+,-)).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(clientDistrict, maxMod, (hasAccount(+,-,true),freq(+,-))).
%For order% mode(clientDistrict, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%For order% mode(clientDistrict, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%For order% mode(clientDistrict, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

%For order% mode(hasAccount, none, age(+,-)).
%For order% mode(hasAccount, none, gender(+,-)).
mode(hasAccount, cnt, clientDistrict(+,-,true)).
mode(hasAccount, cnt, clientLoan(+,-,true)).
%For order% mode(hasAccount, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%For order% mode(hasAccount, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(hasAccount, none, avgSumOfW(+,-)).
%For order% mode(hasAccount, none, stdMonthInc(+,-)).
%For order% mode(hasAccount, none, stdMonthW(+,-)).
%For order% mode(hasAccount, none, avgNrWith(+,-)).
%For order% mode(hasAccount, none, avgSumOfInc(+,-)).
%For order% mode(hasAccount, none, freq(+,-)).
%For order% mode(hasAccount, avg, (clientLoan(+,-,true),loanAmount(+,-))).
%For order% mode(hasAccount, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
%For order% mode(hasAccount, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(clientLoan, cnt, clientDistrict(+,-,true)).
%For order% mode(clientLoan, none, age(+,-)).
%For order% mode(clientLoan, none, gender(+,-)).
%For order% mode(clientLoan, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
%For order% mode(clientLoan, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
%For order% mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
%For order% mode(clientLoan, maxMod, (hasAccount(+,-,true),freq(+,-))).
%For order% mode(clientLoan, none, loanAmount(+,-)).
%For order% mode(clientLoan, none, loanStatus(+,-)).
%For order% mode(clientLoan, none, monthlyPayments(+,-)).

mode(hasLoan, cnt, districtClientAccount(-,-,+)).
mode(hasLoan, cnt, hasAccount(-,+,true)).
%For order% mode(hasLoan, none, avgSumOfInc(+,-)).
%For order% mode(hasLoan, none, avgSumOfW(+,-)).
%For order% mode(hasLoan, none, stdMonthInc(+,-)).
%For order% mode(hasLoan, none, stdMonthW(+,-)).
%For order% mode(hasLoan, none, avgNrWith(+,-)).
%For order% mode(hasLoan, none, freq(+,-)).
%For order% mode(hasLoan, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
%For order% mode(hasLoan, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
%For order% mode(hasLoan, maxMod, (hasAccount(-,+,true),gender(+,-))).
%For order% mode(hasLoan, avg, (hasAccount(-,+,true),age(+,-))).
%For order% mode(hasLoan, none, loanAmount(+,-)).
%For order% mode(hasLoan, none, loanStatus(+,-)).
%For order% mode(hasLoan, none, monthlyPayments(+,-)).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(hasLoan, 3, continuous, []).
thres(clientDistrict, 3, continuous, []).
thres(districtClientAccount, 3, continuous, []).
thres(districtClientLoan, 3, continuous, []).
thres(hasAccount, 3, continuous, []).
thres(clientLoan, 3, continuous, []).
thres(loanAmount, 2, continuous, []).
thres(loanStatus, 2, discrete, [a,b,c,d]).
thres(monthlyPayments, 2, continuous, []).
thres(gender, 2, discrete, [m,f]).
thres(age, 2, continuous, []).
thres(freq, 2, discrete, [i,w,m]).
thres(avgSalary, 2, continuous, []).
thres(ratUrbInhab, 2, continuous, []).
thres(avgNrWith, 2, continuous, []).
thres(avgSumOfInc, 2, continuous, []).
thres(avgSumOfW, 2, continuous, []).
thres(stdMonthInc, 2, continuous, []).
thres(stdMonthW, 2, continuous, []).

%Target
learn(hasLoan, 3, 3, discrete).
learn(clientDistrict, 3, 3, discrete).
learn(hasAccount, 3, 3, discrete).
learn(clientLoan, 3, 3, discrete).
learn(loanAmount, 2, 2, continuous).
learn(loanStatus, 2, 2, discrete).
learn(monthlyPayments, 2, 2, continuous).
learn(gender, 2, 2, discrete).
learn(age, 2, 2, continuous).
learn(freq, 2, 2, discrete).
learn(avgSalary, 2, 2, continuous).
learn(ratUrbInhab, 2, 2, continuous).
learn(avgNrWith, 2, 2, continuous).
learn(avgSumOfInc, 2, 2, continuous).
learn(avgSumOfW, 2, 2, continuous).
learn(stdMonthInc, 2, 2, continuous).
learn(stdMonthW, 2, 2, continuous).
  
%Facts
''')

DECLARATIVE_BIAS = dict(
    financial = '''
:- discontiguous client/1.
:- discontiguous loan/1.
:- discontiguous hasLoan/3.
:- discontiguous clientDistrict/3.
:- discontiguous hasAccount/3.
:- discontiguous district/1.
:- discontiguous account/1.
:- discontiguous loanAmount/2.
:- discontiguous loanStatus/2.
:- discontiguous monthlyPayments/2.
:- discontiguous gender/2.
:- discontiguous clientLoan/3.
:- discontiguous age/2.
:- discontiguous freq/2.
:- discontiguous avgSalary/2.
:- discontiguous ratUrbInhab/2.
:- discontiguous avgNrWith/2.
:- discontiguous avgSumOfInc/2.
:- discontiguous avgSumOfW/2.
:- discontiguous stdMonthInc/2.
:- discontiguous stdMonthW/2.

%Relational Lookahead
districtClientAccount(D,C,A) :- clientDistrict(C,D,true), hasAccount(C,A,true).
districtClientLoan(D,C,L) :- clientDistrict(C,D,true), clientLoan(C,L,true).

%Types
base(client(c)).
base(loan(l)).
base(hasLoan(a,l,x1)).
base(districtClientAccount(d,c,a)).
base(districtClientLoan(d,c,l)).
base(clientDistrict(c,d,x2)).
base(hasAccount(c,a,x3)).
base(district(d)).
base(account(a)).
base(loanAmount(l,la)).
base(loanStatus(l,ls)).
base(monthlyPayments(l,mp)).
base(gender(c,ge)).
base(age(c,ca)).
base(clientLoan(c,l,x4)).
base(freq(a,afr)).
base(avgSalary(d,dav)).
base(ratUrbInhab(d,drat)).
base(avgNrWith(a,aavn)).
base(avgSumOfInc(a,aavsi)).
base(avgSumOfW(a,aavw)).
base(stdMonthInc(a,astdmi)).
base(stdMonthW(a,astdmw)).
  
%Modes
mode(avgSalary, cnt, districtClientAccount(+,-,-)).
mode(avgSalary, none, ratUrbInhab(+,-)).
mode(avgSalary, maxMod, (clientDistrict(-,+,true),gender(+,-))).
mode(avgSalary, avg, (clientDistrict(-,+,true),age(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
mode(avgSalary, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
mode(avgSalary, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
mode(avgSalary, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
mode(avgSalary, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
mode(avgSalary, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(ratUrbInhab, cnt, districtClientAccount(+,-,-)).
mode(ratUrbInhab, none, avgSalary(+,-)).
mode(ratUrbInhab, maxMod, (clientDistrict(-,+,true),gender(+,-))).
mode(ratUrbInhab, avg, (clientDistrict(-,+,true),age(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfW(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthInc(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),stdMonthW(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgNrWith(+,-))).
mode(ratUrbInhab, avg, (districtClientAccount(+,-,-),avgSumOfInc(+,-))).
mode(ratUrbInhab, maxMod, (districtClientAccount(+,-,-),freq(+,-))).
mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),loanAmount(+,-))).
mode(ratUrbInhab, maxMod, (districtClientLoan(+,-,-),loanStatus(+,-))).
mode(ratUrbInhab, avg, (districtClientLoan(+,-,-),monthlyPayments(+,-))).

mode(gender, cnt, clientDistrict(+,-,true)).
mode(gender, cnt, hasAccount(+,-,true)).
mode(gender, none, age(+,-)).
mode(gender, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(gender, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(gender, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(gender, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(gender, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(gender, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(gender, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(gender, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(gender, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(gender, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(gender, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(age, cnt, clientDistrict(+,-,true)).
mode(age, cnt, hasAccount(+,-,true)).
mode(age, cnt, clientLoan(+,-,true)).
mode(age, none, gender(+,-)).
mode(age, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(age, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(age, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(age, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(age, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(age, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(age, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(age, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(age, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(age, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(age, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(loanAmount, cnt, hasLoan(-,+,true)).
mode(loanAmount, cnt, districtClientLoan(-,-,+)).
mode(loanAmount, none, loanStatus(+,-)).
mode(loanAmount, none, monthlyPayments(+,-)).
mode(loanAmount, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanAmount, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(loanAmount, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(loanAmount, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanAmount, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(loanAmount, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(loanStatus, cnt, hasLoan(-,+,true)).
mode(loanStatus, cnt, districtClientLoan(-,-,+)).
mode(loanStatus, none, loanAmount(+,-)).
mode(loanStatus, none, monthlyPayments(+,-)).
mode(loanStatus, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(loanStatus, avg, (clientLoan(-,+,true),age(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(loanStatus, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(loanStatus, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(loanStatus, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(loanStatus, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(monthlyPayments, cnt, hasLoan(-,+,true)).
mode(monthlyPayments, cnt, districtClientLoan(-,-,+)).
mode(monthlyPayments, none, loanAmount(+,-)).
mode(monthlyPayments, none, loanStatus(+,-)).
mode(monthlyPayments, maxMod, (clientLoan(-,+,true),gender(+,-))).
mode(monthlyPayments, avg, (clientLoan(-,+,true),age(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfW(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthInc(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),stdMonthW(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgNrWith(+,-))).
mode(monthlyPayments, avg, (hasLoan(-,+,true),avgSumOfInc(+,-))).
mode(monthlyPayments, maxMod, (hasLoan(-,+,true),freq(+,-))).
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),avgSalary(+,-))).
mode(monthlyPayments, avg, (districtClientLoan(-,-,+),ratUrbInhab(+,-))).

mode(avgSumOfW, none, stdMonthInc(+,-)).
mode(avgSumOfW, none, stdMonthW(+,-)).
mode(avgSumOfW, none, avgNrWith(+,-)).
mode(avgSumOfW, none, avgSumOfInc(+,-)).
mode(avgSumOfW, none, freq(+,-)).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfW, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgSumOfW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgSumOfW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(avgSumOfW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthInc, none, avgSumOfW(+,-)).
mode(stdMonthInc, none, stdMonthW(+,-)).
mode(stdMonthInc, none, avgNrWith(+,-)).
mode(stdMonthInc, none, avgSumOfInc(+,-)).
mode(stdMonthInc, none, freq(+,-)).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(stdMonthInc, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(stdMonthInc, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(stdMonthInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(stdMonthW, none, avgSumOfW(+,-)).
mode(stdMonthW, none, stdMonthInc(+,-)).
mode(stdMonthW, none, avgNrWith(+,-)).
mode(stdMonthW, none, avgSumOfInc(+,-)).
mode(stdMonthW, none, freq(+,-)).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(stdMonthW, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(stdMonthW, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(stdMonthW, avg, (hasAccount(-,+,true),age(+,-))).
mode(stdMonthW, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(stdMonthW, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(stdMonthW, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(avgNrWith, none, avgSumOfW(+,-)).
mode(avgNrWith, none, stdMonthInc(+,-)).
mode(avgNrWith, none, stdMonthW(+,-)).
mode(avgNrWith, none, avgSumOfInc(+,-)).
mode(avgNrWith, none, freq(+,-)).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgNrWith, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgNrWith, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgNrWith, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgNrWith, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgNrWith, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(avgNrWith, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(avgSumOfInc, none, avgSumOfW(+,-)).
mode(avgSumOfInc, none, stdMonthInc(+,-)).
mode(avgSumOfInc, none, stdMonthW(+,-)).
mode(avgSumOfInc, none, avgNrWith(+,-)).
mode(avgSumOfInc, none, freq(+,-)).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(avgSumOfInc, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(avgSumOfInc, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(avgSumOfInc, avg, (hasAccount(-,+,true),age(+,-))).
mode(avgSumOfInc, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(avgSumOfInc, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(avgSumOfInc, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(freq, none, avgSumOfW(+,-)).
mode(freq, none, stdMonthInc(+,-)).
mode(freq, none, stdMonthW(+,-)).
mode(freq, none, avgNrWith(+,-)).
mode(freq, none, avgSumOfInc(+,-)).
mode(freq, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(freq, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(freq, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(freq, avg, (hasAccount(-,+,true),age(+,-))).
mode(freq, avg, (hasLoan(+,-,true),loanAmount(+,-))).
mode(freq, maxMod, (hasLoan(+,-,true),loanStatus(+,-))).
mode(freq, avg, (hasLoan(+,-,true),monthlyPayments(+,-))).

mode(clientDistrict, none, age(+,-)).
mode(clientDistrict, none, gender(+,-)).
mode(clientDistrict, none, avgSalary(+,-)).
mode(clientDistrict, none, ratUrbInhab(+,-)).
mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(clientDistrict, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(clientDistrict, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(clientDistrict, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(clientDistrict, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(clientDistrict, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(hasAccount, none, age(+,-)).
mode(hasAccount, none, gender(+,-)).
mode(hasAccount, cnt, clientDistrict(+,-,true)).
mode(hasAccount, cnt, clientLoan(+,-,true)).
mode(hasAccount, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(hasAccount, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(hasAccount, none, avgSumOfW(+,-)).
mode(hasAccount, none, stdMonthInc(+,-)).
mode(hasAccount, none, stdMonthW(+,-)).
mode(hasAccount, none, avgNrWith(+,-)).
mode(hasAccount, none, avgSumOfInc(+,-)).
mode(hasAccount, none, freq(+,-)).
mode(hasAccount, avg, (clientLoan(+,-,true),loanAmount(+,-))).
mode(hasAccount, maxMod, (clientLoan(+,-,true),loanStatus(+,-))).
mode(hasAccount, avg, (clientLoan(+,-,true),monthlyPayments(+,-))).

mode(clientLoan, cnt, clientDistrict(+,-,true)).
mode(clientLoan, none, age(+,-)).
mode(clientLoan, none, gender(+,-)).
mode(clientLoan, avg, (clientDistrict(+,-,true),avgSalary(+,-))).
mode(clientLoan, avg, (clientDistrict(+,-,true),ratUrbInhab(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfW(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthInc(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),stdMonthW(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),avgNrWith(+,-))).
mode(clientLoan, avg, (hasAccount(+,-,true),avgSumOfInc(+,-))).
mode(clientLoan, maxMod, (hasAccount(+,-,true),freq(+,-))).
mode(clientLoan, none, loanAmount(+,-)).
mode(clientLoan, none, loanStatus(+,-)).
mode(clientLoan, none, monthlyPayments(+,-)).

mode(hasLoan, cnt, districtClientAccount(-,-,+)).
mode(hasLoan, cnt, hasAccount(-,+,true)).
mode(hasLoan, none, avgSumOfInc(+,-)).
mode(hasLoan, none, avgSumOfW(+,-)).
mode(hasLoan, none, stdMonthInc(+,-)).
mode(hasLoan, none, stdMonthW(+,-)).
mode(hasLoan, none, avgNrWith(+,-)).
mode(hasLoan, none, freq(+,-)).
mode(hasLoan, avg, (districtClientAccount(-,-,+),avgSalary(+,-))).
mode(hasLoan, avg, (districtClientAccount(-,-,+),ratUrbInhab(+,-))).
mode(hasLoan, maxMod, (hasAccount(-,+,true),gender(+,-))).
mode(hasLoan, avg, (hasAccount(-,+,true),age(+,-))).
mode(hasLoan, none, loanAmount(+,-)).
mode(hasLoan, none, loanStatus(+,-)).
mode(hasLoan, none, monthlyPayments(+,-)).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(hasLoan, 3, continuous, []).
thres(clientDistrict, 3, continuous, []).
thres(districtClientAccount, 3, continuous, []).
thres(districtClientLoan, 3, continuous, []).
thres(hasAccount, 3, continuous, []).
thres(clientLoan, 3, continuous, []).
thres(loanAmount, 2, continuous, []).
thres(loanStatus, 2, discrete, [a,b,c,d]).
thres(monthlyPayments, 2, continuous, []).
thres(gender, 2, discrete, [m,f]).
thres(age, 2, continuous, []).
thres(freq, 2, discrete, [i,w,m]).
thres(avgSalary, 2, continuous, []).
thres(ratUrbInhab, 2, continuous, []).
thres(avgNrWith, 2, continuous, []).
thres(avgSumOfInc, 2, continuous, []).
thres(avgSumOfW, 2, continuous, []).
thres(stdMonthInc, 2, continuous, []).
thres(stdMonthW, 2, continuous, []).

%Target
learn(hasLoan, 3, 3, discrete).
learn(clientDistrict, 3, 3, discrete).
learn(hasAccount, 3, 3, discrete).
learn(clientLoan, 3, 3, discrete).
learn(loanAmount, 2, 2, continuous).
learn(loanStatus, 2, 2, discrete).
learn(monthlyPayments, 2, 2, continuous).
learn(gender, 2, 2, discrete).
learn(age, 2, 2, continuous).
learn(freq, 2, 2, discrete).
learn(avgSalary, 2, 2, continuous).
learn(ratUrbInhab, 2, 2, continuous).
learn(avgNrWith, 2, 2, continuous).
learn(avgSumOfInc, 2, 2, continuous).
learn(avgSumOfW, 2, 2, continuous).
learn(stdMonthInc, 2, 2, continuous).
learn(stdMonthW, 2, 2, continuous).
  
%Facts
''',

university = '''
:- discontiguous course/1.
:- discontiguous professor/1.
:- discontiguous student/1.
:- discontiguous nrhours/2.
:- discontiguous difficulty/2.
:- discontiguous ability/2.
:- discontiguous intelligence/2.
:- discontiguous grade/3.
:- discontiguous satisfaction/3.
:- discontiguous takes/3.
:- discontiguous friend/3.
:- discontiguous teaches/3.


%Relational Lookahead
studentCourseProfessor(S,C,P) :- takes(S,C,true), teaches(P,C,true).

%Types
base(course(c)).
base(professor(p)).
base(student(s)).
base(studentCourseProfessor(s,c,p)).
base(nrhours(c,nr)).
base(difficulty(c,di)).
base(ability(p,ab)).
base(intelligence(s,in)).
base(grade(s,c,gr)).
base(satisfaction(s,c,sa)).
base(takes(s,c,x1)).
base(friend(s,s,x2)).
base(teaches(p,c,x3)).

%Modes
mode(nrhours, none, difficulty(+,-)).
mode(nrhours, avg, (takes(-,+,true), intelligence(+,-))).
mode(nrhours, maxMod, (takes(-,+,true), grade(+,+,-))).
mode(nrhours, maxMod, (takes(-,+,true), satisfaction(+,+,-))).
mode(nrhours, avg, (teaches(-,+,true), ability(+,-))).

mode(difficulty, none, nrhours(+,-)).
mode(difficulty, avg, (takes(-,+,true), intelligence(+,-))).
mode(difficulty, maxMod, (takes(-,+,true), grade(+,+,-))).
mode(difficulty, maxMod, (takes(-,+,true), satisfaction(+,+,-))).
mode(difficulty, avg, (teaches(-,+,true), ability(+,-))).

mode(ability, avg, (teaches(+,-,true), nrhours(+,-))).
mode(ability, maxMod, (teaches(+,-,true), difficulty(+,-))).
%mode(ability, avg, (studentCourseProfessor(-,-,+), intelligence(+,-))).
%mode(ability, maxMod, (studentCourseProfessor(-,-,+), grade(+,+,-))).
%mode(ability, maxMod, (studentCourseProfessor(-,-,+), satisfaction(+,+,-))).

mode(intelligence, maxMod, grade(+,-,-)).
mode(intelligence, maxMod, satisfaction(+,-,-)).
mode(intelligence, avg, (takes(+,-,true), nrhours(+,-))).
mode(intelligence, maxMod, (takes(+,-,true), difficulty(+,-))).
%mode(intelligence, avg, (studentCourseProfessor(+,-,-), ability(+,-))).
%mode(intelligence, maxMod, (friend(+,-,true), grade(+,-,-))).
%mode(intelligence, maxMod, (friend(+,-,true), satisfaction(+,-,-))).
%mode(intelligence, maxMod, (friend(+,-,true), intelligence(+,-))).

mode(grade, none, intelligence(+,-)).
mode(grade, maxMod, satisfaction(+,+,-)).
mode(grade, avg, (takes(+,+,true), nrhours(+,-))).
mode(grade, maxMod, (takes(+,+,true), difficulty(+,-))).
%mode(grade, avg, (studentCourseProfessor(+,+,-), ability(+,-))).
%mode(grade, maxMod, (friend(+,-,true), grade(+,+,-))).
%mode(grade, maxMod, (friend(+,-,true), satisfaction(+,+,-))).
%mode(grade, maxMod, (friend(+,-,true), intelligence(+,-))).

mode(takes, none, intelligence(+,-)).
mode(takes, none, satisfaction(+,+,-)).
mode(takes, none, difficulty(-,+)).
mode(takes, none, nrhours(-,+)).

mode(satisfaction, none, intelligence(+,-)).
mode(satisfaction, maxMod, grade(+,+,-)).
mode(satisfaction, avg, (takes(+,+,true), nrhours(+,-))).
mode(satisfaction, maxMod, (takes(+,+,true), difficulty(+,-))).
%mode(satisfaction, avg, (studentCourseProfessor(+,+,-), ability(+,-))).
%mode(satisfaction, maxMod, (friend(+,-,true), grade(+,+,-))).
%mode(satisfaction, maxMod, (friend(+,-,true), satisfaction(+,+,-))).
%mode(satisfaction, maxMod, (friend(+,-,true), intelligence(+,-))).

mode(friend, cnt, takes(+,-,true)).
mode(friend, maxMod, grade(+,-,-)).
mode(friend, maxMod, satisfaction(+,-,-)).
mode(friend, none, intelligence(+,-)).

%Aggregations
agg(none).
agg(avg).
agg(maxMod).
agg(cnt).
  
%Threshold
thres(nrhours, 2, continuous, []).
thres(difficulty, 2, discrete, [medium,hard,easy]).
thres(ability, 2, continuous, []).
thres(intelligence, 2, continuous, []).
thres(grade, 3, discrete, [high,low,mid]).
thres(satisfaction, 3, discrete, [mid,low,high]).
thres(friend, 3, discrete, [true,false]).
thres(takes, 3, continuous, []).
thres(teaches, 3, continuous, []).

%Target
learn(nrhours, 2, 2, continuous).
learn(difficulty, 2, 2, discrete).
learn(ability, 2, 2, continuous).
learn(intelligence, 2, 2, continuous).
learn(grade, 3, 3, discrete).
learn(satisfaction, 3, 3, discrete).
learn(friend, 3, 3, discrete).
learn(takes, 3, 3, discrete).
learn(teaches, 3, 3, discrete).

%Facts
'''
)

DC_PROGRAM_DATA_DEPENDENT_RELATION = dict(
    university = '''%Relational Lookahead
builtin(studentCourseProfessor(_,_,_)).

studentCourseProfessor(S,C,P) :- takes(S,C,true), teaches(P,C,true).

%Facts

''',
    financial = '''%Relational Lookahead
builtin(districtClientAccount(_,_,_)).
builtin(districtClientLoan(_,_,_)).

districtClientAccount(D,C,A) :- clientDistrict(C,D,true), hasAccount(C,A,true).
districtClientLoan(D,C,L) :- clientDistrict(C,D,true), clientLoan(C,L,true).


%Facts

'''
)

DC_PROGRAM_DATA_DEPENDENT_RELATION_PROBABILISTIC = dict(
    university = '''%Relational Lookahead
builtin(studentCourseProfessor(_,_,_)).

%Facts

''',
    financial = '''%Relational Lookahead
builtin(districtClientAccount(_,_,_)).
builtin(districtClientLoan(_,_,_)).

%Facts

'''
)

PROLOG_PROGRAM_HEADER = '''%%% -*- Mode: Prolog; -*-
:- use_module(library(lists)).

cnt(X, P,Count) :- findall(X,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, [], Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, TMin, Min) :- TMin=[] -> lmin(MMin, [], [], Min); Min=TMin, !.
leftover([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, TMin, Min); leftover(Seen, MMin, [H|TMin], Min).
minMod(X, P, Min) :- findall(X,P,L), lmin(L, Min1), oneElementOfList(Min1, Min).

list2set([], []).
list2set([H|T], [H|T1]) :- subtract(T, [H], T2), list2set(T2, T1).

%maxMod(Template,Goal,G) :-
%    findall(X,bagof(Template,Goal,X),Lists),
%    flatten(Lists,G3),
%    list2set(G3,Gset),
%    member(G,Gset).

%minMod(Template,Goal,G) :-
%    findall(X,bagof(Template,Goal,X),Lists),
%    flatten(Lists,G3),
%    list2set(G3,Gset),
%    member(G,Gset).

max(X, P, Max) :- findall(X,P,L), max_list(L, Max).

min(X, P, Min) :- findall(X,P,L), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- findall(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).

'''

PROLOG_PROGRAM_HEADER_PROBABILISTIC = '''%%% -*- Mode: Prolog; -*-
:- use_module(library(lists)).

cnt(X, P,Count) :- findall(X,P,L), length(L,Count).

oneElementOfList([H|_], X) :- X = H.

lmax(L, M) :- lmax(L, [], [], M).
lmax([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmax(MMax, [], [], Max).
lmax([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmax(T, Seen, [H|MMax], Max); lmax(T, [H|Seen], MMax, Max)).
%maxMod(X, P, Max) :- findall(X,P,L), lmax(L, Max1), oneElementOfList(Max1, Max).

lmin(L, M) :- lmin(L, [], [], M).
lmin([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftover(Seen, MMin, [], Min).
lmin([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmin(T, Seen, [H|Left], Min); lmin(T, [H|Seen], Left, Min)).
leftover([], MMin, TMin, Min) :- TMin=[] -> lmin(MMin, [], [], Min); Min=TMin, !.
leftover([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftover(Seen, MMin, TMin, Min); leftover(Seen, MMin, [H|TMin], Min).
%minMod(X, P, Min) :- findall(X,P,L), lmin(L, Min1), oneElementOfList(Min1, Min).

list2set([], []).
list2set([H|T], [H|T1]) :- subtract(T, [H], T2), list2set(T2, T1).

maxMod(Template,Goal,G) :-
    findall(X,bagof(Template,Goal,X),Lists),
    flatten(Lists,G3),
    list2set(G3,Gset),
    member(G,Gset).

minMod(Template,Goal,G) :-
    findall(X,bagof(Template,Goal,X),Lists),
    flatten(Lists,G3),
    list2set(G3,Gset),
    member(G,Gset).

max(X, P, Max) :- findall(X,P,L), max_list(L, Max).

min(X, P, Min) :- findall(X,P,L), min_list(L, Min).

listavg(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(X, P, Avg) :- findall(X,P,L), length(L,Cnt), listavg(L, Cnt, Avg).

'''


DC_PROGRAM_HEADER = '''%%% -*- Mode: Prolog; -*-

:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).
:- set_options(default).
:- initialization(init).

builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(lmindc(_,_)).
builtin(lmaxdc(_,_)).
builtin(length(_,_)).
builtin(listavgdc(_,_,_)).
builtin(getMean(_,_,_)).
builtin(logistic(_,_,_)).
builtin(softmax(_,_,_)).
builtin(oneElementOfListDC(_,_)).

getMean(X,P,Mean) :- dotProd(X,P,Mean).
dotProd([H1|T1], [H2|T2], Prod) :- dotProd(T1, T2, PartProd), Prod is H1*H2 + PartProd.
dotProd([], [H2], Prod) :- Prod is H2.

logistic(P,X,Result) :- dotProd(X,P,Mean), Result is 1/(1 + exp(-1*Mean)).

softmax(P,X,Result) :- calExponent(P,X,Sum,List), normalization(Sum, List, Result).

normalization(Sum, [X|Xs], [Y|Ys]) :- Y is X/Sum, normalization(Sum, Xs, Ys).
normalization(_, [], []).

calExponent([H|T], X, Sum, [Exp|Exps]) :-
    calExponent([H|T], X, 0, Sum, [Exp|Exps]).
calExponent([H|T], X, Acc0, Sum, [Exp|Exps]) :-
    dotProd(X,H,Prod),
    Exp is exp(Prod),
    Acc is Acc0 + Exp,
    calExponent(T, X, Acc, Sum, Exps).
calExponent([], _, Sum, Sum, []).

cnt(P) ~ val(Count) := length(P,Count).

pickOneElement(List) ~ uniform(List).
%oneElementOfListDC([H|_], X) :- X = H.
%oneElementOfListDC([H|T], X) :- T=[] -> X = H; oneElementOfListDC(T, X).
%pickOneElement(List) ~ val(X) := oneElementOfListDC(List, X).

lmaxdc(L, M) :- lmaxdc(L, [], [], M).
lmaxdc([], Seen, MMax, Max) :- MMax=[] -> Max=Seen; lmaxdc(MMax, [], [], Max).
lmaxdc([H|T], Seen, MMax, Max) :- (member(H, Seen) -> lmaxdc(T, Seen, [H|MMax], Max); lmaxdc(T, [H|Seen], MMax, Max)).
maxMod(L) ~ val(Max) := lmaxdc(L, Max1), pickOneElement(Max1) ~= Max.

lmindc(L, M) :- lmindc(L, [], [], M).
lmindc([], Seen, MMin, Min) :- MMin=[] -> Min=Seen, !; leftoverdc(Seen, MMin, [], Min).
lmindc([H|T], Seen, Left, Min) :- (member(H, Seen) -> lmindc(T, Seen, [H|Left], Min); lmindc(T, [H|Seen], Left, Min)).
leftoverdc([], MMin, TMin, Min) :- TMin=[] -> lmindc(MMin, [], [], Min); Min=TMin, !.
leftoverdc([H|Seen], MMin, TMin, Min) :- (member(H, MMin)) -> leftoverdc(Seen, MMin, TMin, Min); leftoverdc(Seen, MMin, [H|TMin], Min).
minMod(L) ~ val(Min) := lmindc(L, Min1), pickOneElement(Min1) ~= Min.

max(L) ~ val(Max) := max_list(L, Max).

min(L) ~ val(Min) := min_list(L, Min).

listavgdc(L, C, A) :- C =:= 0 -> false; sum_list(L, Sum), A is Sum / C.
avg(L) ~ val(Avg) := length(L,Cnt), listavgdc(L, Cnt, Avg).

samplesGenerator(Num, Evidence, Query, Var, SampleList) :- findall(Var, genSample(Num, Evidence, Query, World), SampleList).
genSample(Num, Evidence, Query, World) :- between(1, Num, SID), generate_backward(Evidence, Query, World).


'''

ERROR = dict()
YPRED = dict()
YTRUE = dict()
YPREDROC = dict()
YTRUEROC = dict()
WPLL = dict()
NUM_OF_ELEMENTS_IN_TEST = dict()
WRMSE= dict()
AUCROC = dict()
WPLL_TOTAL = dict()

logging.basicConfig(level=logging.INFO, filename="Experiment1", filemode="w", format="%(asctime)-15s %(levelname)-8s %(message)s")


class Experiment1():  

    def __init__(self):
        pass

    def getPredicateName(self, feat):
        feat = feat.replace(' ', '')
        featName = ''
        for i in feat:
            if i == '(':
                break
            else:
                featName+=i
        return featName

    def getListOfPredicatesInFolder(self, folderString):
        listOfFacts = []
        onlyfiles = [f for f in listdir(folderString) if isfile(join(folderString, f))]
        for f in onlyfiles:
            fSplit = f.split('.')
            filePath = join(folderString, f)
            filePointer = open(filePath, 'r')
            data = filePointer.read()
            filePointer.close()
            data = data.split('\n')
            for dat in data:
                if dat == '':
                    continue
                else:
                    dat = dat.replace('\r', '')
                    dat = dat.replace(' ', '')
                    datList = []
                    predicateName = self.getPredicateName(dat)
                    if predicateName in RELATIONAL_PREDICATES[DATABASE_NAME]:
                        if fSplit[1] == 'neg':
                            dat = dat.replace(').', ',false).')
                        else:
                            dat = dat.replace(').', ',true).')
                        datList.append(dat)
                    elif predicateName == 'clientAge':
                        tempDat = dat
                        tempDat = tempDat.split(',')
                        tempDat[0] = tempDat[0].replace('clientAge', 'clientLoan')
                        tempDat1 = tempDat[0] + ',' + tempDat[1] + ',true).'
                        tempDat[0] = tempDat[0].replace('clientLoan', 'age')
                        tempDat2 = tempDat[0] + ',' + tempDat[2]
                        datList = []
                        datList.append(tempDat1)
                        datList.append(tempDat2)
                    else:
                        datList.append(dat)
                    listOfFacts = listOfFacts + datList
        return listOfFacts

    def generatePrologFile(self, filename, trainingData, targetVariable=''):
        declarativeBias = DECLARATIVE_BIAS[DATABASE_NAME]
        if targetVariable == '':
            pass
        else:
            declarativeBias = ''
            targetVariableString = 'learn(' + targetVariable
            declarativeBiasTemp = DECLARATIVE_BIAS[DATABASE_NAME].split('\n')
            learnDefinitionStarts = False
            for line in declarativeBiasTemp:
                if '%Target' in line and not learnDefinitionStarts:
                    declarativeBias = declarativeBias + line + '\n'
                    learnDefinitionStarts = True
                elif learnDefinitionStarts:
                    if targetVariableString in line:
                        declarativeBias = declarativeBias + line + '\n'
                    else:
                        pass
                else:
                    declarativeBias = declarativeBias + line + '\n'
        f = open(filename, 'w')
        f.write(PROLOG_PROGRAM_HEADER)
        f.write(declarativeBias)
        for i in trainingData:
            f.write(i)
            f.write('\n')
        f.close()

    def findListOfDCFacts(self, factList):
        factListDC = []
        for feat in factList:
            feat = feat.replace(' ', '')
            featName = ''
            for i in feat:
                if i == '(':
                    break
                else:
                    featName+=i
            if featName in RANDOM_VARIABLE_PREDICATE[DATABASE_NAME]:
                feat = feat.split(',')
                value = feat[-1][0:-2]
                feat = ','.join(feat[0:-1])
                feat += ')'
                feat = feat + ' ~ ' + 'val(' + value + ').'
                factListDC.append(feat)
            else:
                feat = feat[0:-1]
                feat = feat + " := true."
                factListDC.append(feat)
        return factListDC

    def findListOfPrologFactsFromDCFacts(self, factList):
        factListProlog = []
        for feat in factList:
            feat = feat.replace(' ', '')
            feat = feat.split('~')
            feat[1] = feat[1].replace('val(', '')
            feat = ','.join(feat)
            factListProlog.append(feat)
        return factListProlog
            
    def generateDCFile(self, filename, trainingData, testData, dcRules):
        f = open(filename, 'w')
        f.write(DC_PROGRAM_HEADER)
        f.write(DC_PROGRAM_DATA_DEPENDENT_RELATION[DATABASE_NAME])
        trainingDataDC = self.findListOfDCFacts(trainingData)
        testDataDC = self.findListOfDCFacts(testData)
        f.write("\n\n%Train data prolog\n")
        for item in trainingData:
            f.write(item + '\n')
        f.write("\n\n%Test data prolog\n")
        for item in testData:
            f.write(item + '\n')
        f.write("\n\n%Train data\n")
        for item in trainingDataDC:
            f.write(item + '\n')
        f.write("\n\n%Test data\n")
        for item in testDataDC:
            f.write(item + '\n')
        f.write("\n\n%Background Theory\n")
        for item in dcRules:
            f.write(item + '\n')
        f.close()

    def flushPredictionDataStructures(self):
        global ERROR 
        global YPRED 
        global YTRUE
        global YPREDROC
        global YTRUEROC
        global NUM_OF_ELEMENTS_IN_TEST
        ERROR = dict()
        YPRED = dict()
        YTRUE = dict()
        YPREDROC = dict()
        YTRUEROC = dict()
        NUM_OF_ELEMENTS_IN_TEST = dict()

    def updateWPLLList(self, samples, featureName, originalValue, ty):
        if ty == 'continuous':
            empty = False
            for i in range(0, len(samples)):
                if samples[i] == '':
                    empty =True
                    break
                else:
                    samples[i] = float(samples[i])
            if empty:
                pass
            else:
                mn = statistics.mean(samples)
                std = statistics.stdev(samples)
                WPLL[featureName].append(np.log(scipy.stats.norm(mn, std).pdf(float(originalValue))))
        else:
            if samples[0] == '':
                pass
            else:
                lenSamples = len(samples)
                counts = Counter(samples)
                predList = []
                predProb = []
                for ele in RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName]:
                    if ele == originalValue:
                        predList.append(1)
                    else:
                        predList.append(0)
                    if counts.has_key(ele):
                        predProb.append(counts[ele]/float(lenSamples))
                    else:
                        predProb.append(0.000001)
                probability = predProb[RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName].index(originalValue)]
                WPLL[featureName].append(np.log(probability))
    
    def updateAccuracyList(self, samples, featureName, originalValue, ty):
        if ty == 'continuous':
            empty = False
            for i in range(0, len(samples)):
                if samples[i] == '':
                    empty =True
                    break
                else:
                    samples[i] = float(samples[i])
            if empty:
                pass
            else:
                average = sum(samples)/float(len(samples))
                ERROR[featureName].append((average - float(originalValue))*(average - float(originalValue)))
                NUM_OF_ELEMENTS_IN_TEST[featureName] = NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
        else:
            if samples[0] == '':
                pass
            else:
                lenSamples = len(samples)
                counts = Counter(samples)
                predList = []
                predProb = []
                for ele in RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][featureName]:
                    if ele == originalValue:
                        predList.append(1)
                    else:
                        predList.append(0)
                    if counts.has_key(ele):
                        predProb.append(counts[ele]/float(lenSamples))
                    else:
                        predProb.append(0.0)
                YTRUEROC[featureName].append(predList)
                YPREDROC[featureName].append(predProb)
                yPred = max(set(samples), key=samples.count)
                YPRED[featureName].append(yPred)
                YTRUE[featureName].append(originalValue)
                NUM_OF_ELEMENTS_IN_TEST[featureName] = NUM_OF_ELEMENTS_IN_TEST[featureName] + 1
                
    def findWPLL(self, numOfSamples, tempDcFileName, testFacts, listOfTargetPredicates, trainFacts, dcRules):
        for predicate in listOfTargetPredicates:
            WPLL[predicate] = []
        for featName in listOfTargetPredicates:
            restTestFacts = []
            tempTestFacts = []
            for tFacts in testFacts:
                if featName not in tFacts:
                    restTestFacts.append(tFacts)
                else:
                    tempTestFacts.append(tFacts)
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][featName]
            self.generateDCFile(tempDcFileName, trainFacts, restTestFacts, dcRules)
            yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
            yapObj.consultWithOneFile(tempDcFileName)
            for i in range(0, len(tempTestFacts)): 
                feat = tempTestFacts[i]
                feat = self.findListOfDCFacts([feat])[0]
                feat = feat.split('~')
                feat[1] = feat[1].replace('val(', '')
                feat[1] = feat[1].replace(').', '')
                feat[1] = feat[1].replace(' ', '')
                evidence = '[]'
                variable = '[X]'
                query = '(' + feat[0] + ' ~= X)'
                samples = yapObj.sample(numOfSamples, query, evidence, variable)
                samples = samples.split(',')
                for i in range(0, len(samples)):
                    samples[i] = samples[i].replace('[', '').replace(']', '')
                self.updateWPLLList(samples, featName, feat[1], ty)
                print feat, samples[0:10]
            del yapObj
        for predicate in listOfTargetPredicates:
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][predicate] 
            logging.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            wpll = WPLL[predicate]
            numElements = len(wpll)
            if numElements == 0:
                wpllScore = float("-inf")
            else:
                wpllScore = sum(wpll)/float(numElements)
            logging.info('\nWPLL = %f, Number of tests consider = %d' % (wpllScore, numElements))
            if WPLL_TOTAL.has_key(predicate):
                tempList = WPLL_TOTAL[predicate]
                tempList.append(wpllScore)
            else:
                WPLL_TOTAL[predicate] = [wpllScore]
        

    def findTestAccuracy(self, numOfSamples, tempDcFileName, testFacts, listOfTargetPredicates, trainFacts, dcRules):
        for predicate in listOfTargetPredicates:
            ERROR[predicate] = []
            YPRED[predicate] = []
            YTRUE[predicate] = []
            YTRUEROC[predicate] = []
            YPREDROC[predicate] = []
            NUM_OF_ELEMENTS_IN_TEST[predicate] = 0
        for featName in listOfTargetPredicates:
            restTestFacts = []
            tempTestFacts = []
            for tFacts in testFacts:
                if featName not in tFacts:
                    restTestFacts.append(tFacts)
                else:
                    tempTestFacts.append(tFacts)
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][featName]
            self.generateDCFile(tempDcFileName, trainFacts, restTestFacts, dcRules)
            yapObj = YapPrologInterface(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
            yapObj.consultWithOneFile(tempDcFileName)
            for i in range(0, len(tempTestFacts)): 
                feat = tempTestFacts[i]
                feat = self.findListOfDCFacts([feat])[0]
                feat = feat.split('~')
                feat[1] = feat[1].replace('val(', '')
                feat[1] = feat[1].replace(').', '')
                feat[1] = feat[1].replace(' ', '')
                evidence = '[]'
                variable = '[X]'
                query = '(' + feat[0] + ' ~= X)'
                samples = yapObj.sample(numOfSamples, query, evidence, variable)
                samples = samples.split(',')
                for i in range(0, len(samples)):
                    samples[i] = samples[i].replace('[', '').replace(']', '')
                self.updateAccuracyList(samples, featName, feat[1], ty)
                print feat, samples[0:10]
            del yapObj
        for predicate in listOfTargetPredicates:
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[DATABASE_NAME][predicate] 
            logging.info('\n \n \n' + '$$$$$$$$$$ Results for %s $$$$$$$$$$' % (predicate))
            if ty == 'continuous':
                acc = float('nan')
                if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    logging.info('WRMSE = %f, Number of tests consider = %d' % (acc, NUM_OF_ELEMENTS_IN_TEST[predicate]))
                else:
                    wrmse = math.sqrt(sum(ERROR[predicate])/float(NUM_OF_ELEMENTS_IN_TEST[predicate]))/float(RANDOM_VARIABLE_PREDICATE_RANGE[DATABASE_NAME][predicate])
                    if WRMSE.has_key(predicate):
                        tempList = WRMSE[predicate]
                        tempList.append(wrmse)
                    else:
                        WRMSE[predicate] = [wrmse]
                    logging.info('WRMSE = %f, Number of tests consider = %d' % (wrmse, NUM_OF_ELEMENTS_IN_TEST[predicate]))
            else:
                uniqueLabel = list(set(YPRED[predicate] + YTRUE[predicate]))
                logging.info ('Confusion Matrix:')
                logging.info(confusion_matrix(YTRUE[predicate], YPRED[predicate], labels=uniqueLabel))
                logging.info('Labels: '  + str(uniqueLabel))
                if NUM_OF_ELEMENTS_IN_TEST[predicate] == 0:
                    logging.info('Accuracy: nan')
                    logging.info('Area Under ROC: prediction list is empty')
                else:
                    logging.info('Accuracy: %f'  % accuracy_score(YTRUE[predicate], YPRED[predicate]))
                    try:
                        auROC = roc_auc_score(np.array(YTRUEROC[predicate]), np.array(YPREDROC[predicate]), average='weighted')
                        if AUCROC.has_key(predicate):
                            tempList = AUCROC[predicate]
                            tempList.append(auROC)
                        else:
                            AUCROC[predicate] = [auROC]
                        logging.info('Area Under ROC: %f' % auROC)
                    except ValueError:
                        logging.info('Only one class present in y_true, Area Under ROC: %f' % 1)

    def generateEnsembleOfDLTs(self, fileName, trainFacts, outputFile):
        self.generatePrologFile(fileName, trainFacts)
        f = open(outputFile, 'w')
        obj = TreeLearnerProbabilistic(fileName, '', 'SWI-prolog', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
        obj.learnRules()
        obj1 = TranslateToDC()
        dcRules = []
        obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
        for rule in obj.rules:
            rule = obj1.translate(rule)
            dcRules.append(rule)
            f.write(rule + '\n')
        f.close()
        return dcRules

    def experiment1_financial(self, prologFileName, dcFileName, treeOutputFile):
        for randVariable in ['hasAccount']: ## Put all target random variable here.
            for fold in range(1,11):
                foldString = "Fold" + str(fold)
                trainFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
                validateFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
                testFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
                trainFacts = trainFacts + validateFacts
                self.flushPredictionDataStructures()
                
                self.generatePrologFile(prologFileName, trainFacts, targetVariable=randVariable)
                f = open(treeOutputFile, 'w')
                obj = TreeLearnerProbabilistic(prologFileName, '', 'SWI-prolog', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                self.findTestAccuracy(1000, dcFileName, testFacts, [randVariable], trainFacts, dcRules)
            for key in WRMSE:
                a = WRMSE[key]
                logging.info('\nPredicate = %s, WRMSE = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            for key in AUCROC:
                a = AUCROC[key]
                logging.info('\nPredicate = %s, AUCROC = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            print "Too much work. Going to take nap for 10 secs."
            time.sleep(10)
            print "I am feeling fresh now :)"
        
    def experiment1_university(self, prologFileName, dcFileName, treeOutputFile):
        startFold = 1
        endFold = 2
        for randVariable in ['friend']: ## Put all target random variable here.
            for fold in range(startFold,endFold):
                foldString = "data800x125x125_" + str(fold) + '/data800x125x125'
                trainFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
                #validateFacts = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
                testFacts = self.getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
                trainFacts = trainFacts
                self.flushPredictionDataStructures()
                
                self.generatePrologFile(prologFileName, trainFacts, targetVariable=randVariable)
                f = open(treeOutputFile, 'w')
                obj = TreeLearnerProbabilistic(prologFileName, '', 'SWI-prolog', '', RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                obj.learnRules()
                obj1 = TranslateToDC()
                dcRules = []
                obj1.setRandomVariablePredicates(RANDOM_VARIABLE_PREDICATE[DATABASE_NAME])
                for rule in obj.rules:
                    rule = obj1.translate(rule)
                    dcRules.append(rule)
                    f.write(rule + '\n')
                f.close()
                self.findWPLL(1000, dcFileName, testFacts, [randVariable], [], dcRules)
            if (endFold - startFold) > 1:
                for key in WPLL_TOTAL:
                    a = WPLL_TOTAL[key]
                    logging.info('\nPredicate = %s, WPLL = %f +- %f' % (key, statistics.mean(a), statistics.stdev(a)))
            print "Too much work. Going to take nap for 10 secs."
            time.sleep(10)
            print "I am feeling fresh now :)"
        
if __name__ == '__main__':        
    obj = Experiment1()
    #experiment1_financial(PROCESSED_TRAIN_DATA_FILE, PROCESSED_TRAIN_DATA_FILE_DC, OUTPUT_ENSEMBLE_OF_DLTS)
    
    obj.experiment1_university(PROCESSED_TRAIN_DATA_FILE, PROCESSED_TRAIN_DATA_FILE_DC, OUTPUT_ENSEMBLE_OF_DLTS)
    
    #TRAIN_FACTS = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'train')
    #validateFacts = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'validate')
    #TEST_FACTS = getListOfPredicatesInFolder(DATA_DIRECTORY + '/' + foldString + '/' + 'test')
    #TRAIN_FACTS = TRAIN_FACTS + validateFacts
    #DC_RULES = generateEnsembleOfDLTs(PROCESSED_TRAIN_DATA_FILE, TRAIN_FACTS, OUTPUT_ENSEMBLE_OF_DLTS)
    #findTestAccuracy(1000, PROCESSED_TRAIN_DATA_FILE_DC, TEST_FACTS, ['loanStatus'], TRAIN_FACTS, DC_RULES)
    #generatePrologFile(PROCESSED_TRAIN_DATA_FILE+'x', TEST_FACTS)


