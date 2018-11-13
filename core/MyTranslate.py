import subprocess
from cmath import sqrt

dc1 = '''%%% -*- Mode: Prolog; -*-
:- use_module('../program/utils.pl').
:- use_module('../DC/dcpf.pl').
:- use_module('../DC/random/sampling.pl').
:- use_module('../DC/distributionalclause.pl').
:- use_module(library(lists)).

:- set_options(default).
:- set_inference(backward(lazy)).
:- initialization(q).

builtin(getMean(_,_,_)).
builtin(nAnchors(_)).

%number of anchors
nAnchors(4).

'''

dcX = '''
posX(4):t+1 ~ val(41.6599209055584) := \+posX(4):t ~=_.
posY(4):t+1 ~ val(37.3911915427964) := \+posY(4):t ~=_.
posZ(4):t+1 ~ val(49.4477456200159) := \+posZ(4):t ~=_.
posX(3):t+1 ~ val(92.8823749801172) := \+posX(3):t ~=_.
posY(3):t+1 ~ val(1.09857956182942) := \+posY(3):t ~=_.
posZ(3):t+1 ~ val(2.97807683375575) := \+posZ(3):t ~=_.
posX(2):t+1 ~ val(103.062816440623) := \+posX(2):t ~=_.
posY(2):t+1 ~ val(1.34667310926443) := \+posY(2):t ~=_.
posZ(2):t+1 ~ val(38.8407384459119) := \+posZ(2):t ~=_.
posX(1):t+1 ~ val(118.134204167004) := \+posX(1):t ~=_.
posY(1):t+1 ~ val(46.429539446919) := \+posY(1):t ~=_.
posZ(1):t+1 ~ val(35.3091237765314) := \+posZ(1):t ~=_.
move_behind_of(2, 1) := true.
behind_of(2, 3) ~ finite([0.95:true,0.05:false]) := true.
'''

dc2 = '''
%Initializing predicates
move_infront_of(_,_) := false.
move_behind_of(_,_) := false.
move_left_of(_,_) := false.
move_right_of(_,_) := false.
infront_of(ID1,ID2) ~ finite([0.0:true,1.0:false]) := nAnchors(N), between(1,N,ID1), between(1,N,ID2).
behind_of(ID1,ID2) ~ finite([0.0:true,1.0:false]) := nAnchors(N), between(1,N,ID1), between(1,N,ID2).
left_of(ID1,ID2) ~ finite([0.0:true,1.0:false]) := nAnchors(N), between(1,N,ID1), between(1,N,ID2).
right_of(ID1,ID2) ~ finite([0.0:true,1.0:false]) := nAnchors(N), between(1,N,ID1), between(1,N,ID2).


%DC rules
'''

dc3 = '''

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%data fetching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
q   :-
    nSamp(NSamp),
    init_particle(NSamp),
    step_particle([],[], NSamp, 1.0),
    getData(NSamp).

search_query(I,Q) :-
    eraseall(tempparticle),
    abolish_all_tables,
    distributionalclause:proof_query_backward_lazy(I,tempparticle,Q).


getData(NSamp) :-
    dcpf:bb_get(offset,Offset),
    (
    between(1,NSamp,SampID),
    I is Offset+SampID,
    search_query(I,(
        findall_forward(
            [ID, X, Y, Z],
            (nAnchors(N), between(1,N,ID), current(posX(ID)) ~= X, current(posY(ID)) ~= Y, current(posZ(ID)) ~= Z),
            State1)
        )
    ),
    %writeln(State1),
    search_query(I,(
        findall_forward(
            [ID, X, Y, Z],
            (nAnchors(N), between(1,N,ID), next(posX(ID)) ~= X, next(posY(ID)) ~= Y, next(posZ(ID)) ~= Z),
            State2)
        )
    ),
    writeln(State2),
    fail;
    true
    ).
'''
numOfSamples = 200
inputRuleFile = '../results/MyDCRulesOnTrain.pl'
outputDCFile = '../results/tempDCRules.pl'
inputTestFile = '../data/testDataWithNeuralPredProbability.pl'
relationalFeatureList = ['infront_of', 'behind_of', 'left_of', 'right_of']

def getNextTestData(data, lastIndx):
    currentStateStr = '%number of samples\n'
    currentStateStr += 'nSamp(' + str(numOfSamples) + ').\n\n'
    currentStateStr += '%current state\n'
    result = dict()
    processCurrent = True
    for i in range(lastIndx, len(data)):
        if data[i] == '':
            continue
        elif ('current_' in data[i]) and processCurrent:
            predicate = ''
            pos = data[i].find('current_')
            valid = False
            for j in range(pos, len(data[i])):
                c = data[i][j]
                if c == '_':
                    valid = True
                elif c == '(':
                    break
                elif valid:
                    predicate += c
                else:
                    pass
            obj = ''
            val = ''
            pos = data[i].find('obj')
            value = False
            for j in range(pos, len(data[i])):
                c = data[i][j]
                if c.isdigit() and not value:
                    obj += c
                elif (c.isdigit() or (c == '.')) and value:
                    val += c
                elif c == ',':
                    value = True
                elif c == ')':
                    break
                else:
                    pass
            st = predicate + '(' + obj + '):t+1 ~ val(' + val + ') := \+' + predicate + '(' + obj + '):t ~=_.\n'
            currentStateStr += st
        elif 'next_pos' in data[i]:
            processCurrent = False
            predicate = ''
            pos = data[i].find('next_pos')
            valid = False
            for j in range(pos, len(data[i])):
                c = data[i][j]
                if c == 's':
                    valid = True
                elif c == '(':
                    break
                elif valid:
                    predicate += c
                else:
                    pass
            obj = ''
            val = ''
            pos = data[i].find('obj')
            value = False
            for j in range(pos, len(data[i])):
                c = data[i][j]
                if c.isdigit() and not value:
                    obj += c
                elif (c.isdigit() or (c == '.')) and value:
                    val += c
                elif c == ',':
                    value = True
                elif c == ')':
                    break
                else:
                    pass
            if result.has_key(obj):
                entry = result[obj]
                entry[predicate] = val
            else:
                entry = dict()
                entry[predicate] = val
                result[obj] = entry
        elif processCurrent:
            predicate = ''
            for j in range(0, len(data[i])):
                c = data[i][j]
                if c == '(':
                    break
                else:
                    predicate += c
            obj1 = ''
            obj2 = ''
            prob = ''
            pos = data[i].find('obj')
            pickObj2 = False
            pickProb = False
            for j in range(pos, len(data[i])):
                c = data[i][j]
                if (c.isdigit()) and (not pickObj2) and (not pickProb):
                    obj1 += c
                elif (c.isdigit()) and (pickObj2) and (not pickProb):
                    obj2 += c
                elif (c.isdigit() or (c == 'e') or (c == '-') or (c == '+') or (c == '.')) and pickProb:
                    prob += c
                elif c == ',' and not pickObj2 and not pickProb:
                    pickObj2 = True
                elif c == ',' and pickObj2 and not pickProb:
                    pickObj2 = False
                    pickProb = True
                elif c == ')':
                    break
                else:
                    pass
            if prob == '':
                st = predicate + '(' + obj1 + ', ' + obj2 + ') := true.\n'
            else:
                st = predicate + '(' + obj1 + ', ' + obj2 + ') ~ finite([' + str(prob) + ':true,' + str(1-float(prob)) + ':false]) := true.\n'
            currentStateStr += st
        elif ('current_' in data[i]) and not processCurrent:
            break
    return [currentStateStr, result, i]
            
def findTestAccuracy(dynamicDC):
    cmd = ['yap', '-L', outputDCFile]
    f = open(inputTestFile, 'r')
    data = f.read()
    f.close()
    data = data.split('\n')
    lastIdx = 0
    xError = []
    yError = []
    processedSamples = 0
    while lastIdx < len(data)-1:
        [dcX, next, lastIdx] = getNextTestData(data, lastIdx)
        f = open(outputDCFile, 'w')
        f.write(dc1)
        f.write(dcX)
        f.write(dc2)
        for dc in dynamicDC:
            f.write(dc)
            f.write('\n')
        f.write(dc3)
        f.close()
        
        p = subprocess.Popen(cmd, stdout=subprocess.PIPE,stderr=subprocess.PIPE,stdin=subprocess.PIPE)
        out, err = p.communicate()
        out = out.split('\n')
        predicted = dict()
        for result in out:
            if result == '':
                continue
            l = []
            if result[1] == ']':
                pass
            else:
                l1 = []
                st = ''
                op = False
                for i in range(1, len(result)):
                    if result[i] == '[':
                        l1 = []
                        op = True
                    elif result[i] == ']':
                        op = False
                        if l1 == []:
                            break
                        else:
                            l1.append(st)
                            l.append(l1)
                            l1 = []
                    elif result[i] == ',' and op:
                        l1.append(st)
                        st = ''
                    elif result[i] == ',' and not op:
                        pass
                    else:
                        st += result[i]
            for rec in l:
                #xEr.append(rec[1])
                #yEr.append(rec[2])
                if predicted.has_key(rec[0]):
                    v = predicted[rec[0]]
                    v['X'].append(float(rec[1]))
                    v['Y'].append(float(rec[2]))
                    v['Z'].append(float(rec[3]))
                else:
                    d = dict()
                    d['X'] = [float(rec[1])]
                    d['Y'] = [float(rec[2])]
                    d['Z'] = [float(rec[3])]
                    predicted[rec[0]] = d
        for k in predicted.keys():
            v = predicted[k]
            le = len(v['X'])
            su = sum(v['X'])
            v['X'] = su/float(le)
            le = len(v['Y'])
            su = sum(v['Y'])
            v['Y'] = su/float(le)
            xError.append((float(next[k]['X']) - v['X'])*(float(next[k]['X']) - v['X']))
            yError.append((float(next[k]['Y']) - v['Y'])*(float(next[k]['Y']) - v['Y']))
        processedSamples += 1
        if (processedSamples % 10 == 0):
            print 'processedSamples: ' + str(processedSamples)
    su = 0
    count = 0
    for ele in xError:
        su += ele
        count += 1
    su = su/float(count)
    su = sqrt(su)
    xError = su
    su = 0
    count = 0
    for ele in yError:
        su += ele
        count += 1
    su = su/float(count)
    su = sqrt(su)
    yError = su
    return [xError, yError]
                    

def replaceRelationalFeatWithTrueFalseAssignment(data):
    newData = data
    for feat in relationalFeatureList:
        pos = -1
        while True:
            pos = data.find(feat, pos+1)
            if pos == -1:
                break
            elif(data[pos-1] != '_'):
                bracketOpen = True
                st = ''
                for i in range(pos, len(data)):
                    c = data[i]
                    if bracketOpen:
                        st += c
                    if c == ')':
                        bracketOpen = False
                tempSt = st
                tempSt = tempSt.split(',')
                tempSt = tempSt[0] + ',' + tempSt[1] + ')'
                if data[pos-2:pos] == '\\+':
                    #rst = '\\+(' + st + '~=true)' 
                    rst = tempSt + '~=false' 
                    newData = newData.replace(('\\+' + st), rst)
                else:
                    rst = tempSt + '~=true'
                    newData = newData.replace(st, rst)
    return newData


f = open(inputRuleFile, 'r')
data = f.read()
f.close()

data = data.split('\n')

dynamicDC = []
for i in range(0, len(data)):
    if data[i] == '':
        continue
    data[i] = data[i].replace('\\\\+', '\\+')
    data[i] = data[i].replace(' ', '')
    data[i] = data[i].replace('(S,', '(')
    data[i] = replaceRelationalFeatWithTrueFalseAssignment(data[i])
    data[i] = data[i].split(':-')
    data[i][0] = data[i][0].split('~')
    data[i][0][0] = data[i][0][0].replace('next_', '')
    data[i][0][0] = data[i][0][0].replace(',X)', '):t+1')
    data[i][0][0] = data[i][0][0].replace(',Y)', '):t+1')
    data[i][0][0] = data[i][0][0].replace(',Z)', '):t+1')
    distribution = data[i][0][1]
    mean = ''
    if 'Gaussian' in distribution:
        distribution = distribution.replace('Gaussian', 'gaussian')
        strip = False
        bracOpen = False
        for j in range(0, len(distribution)):
            c = distribution[j]
            if c == '(':
                strip = True
                continue
            if (c == ',') and (distribution[j-1] == ']') and (distribution[j+1] != '['):
                strip = False
            if strip:
                mean += c
        distribution = distribution.replace(mean, 'Mean')
    data[i][0][1] = distribution

    if 'current_' in data[i][1]:
        pos = data[i][1].find('current_')
        literal = ''
        for j in range(pos,len(data[i][1])):
            c = data[i][1][j]
            if ((c == ',') and (data[i][1][j-1] == ')' and (data[i][1][j+1].islower()))) or (c == '.'):
                break
            else:
                literal += c
    st = literal
    tempSt = literal.split(',')[1].replace(')', '')
    st = st.replace('current_', '')
    st = st.replace(',' + tempSt + ')', '):t~=' + tempSt)
    mean = ',getMean(' + mean + ',Mean).'
    data[i][1] = data[i][1].replace('.', mean)
    data[i][1] = data[i][1].replace(literal, st)
    dynamicDC.append(data[i][0][0] + ' ~ ' + data[i][0][1] + ' := ' + data[i][1])
[xError, yError] = findTestAccuracy(dynamicDC)
