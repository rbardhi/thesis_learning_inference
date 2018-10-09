'''
Created on May 25, 2018

@author: nitesh
'''
from dataProcessor.DataFetcher import fetchDataFromDB
from dataProcessor.Utils import formInertString
from settings.DatabaseSettings import *
from settings.DCFileSettings import *
from core.YapPrologInterface import YapPrologInterface
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score

class DCFeatureDecomposition(object):
    
    def __init__(self):
        self.omissionList = None
        self.testList = None
        self.error = []
        self.omissionListWithMode = []
        self.numOfElementsInTest = 0
        self.yTrue = []
        self.yPred = []
    
    def featureDecomposition(self, db):
        decomposedFeatures = []
        tables = db.keys()
        for table in tables:
            pKeys = db[table]['primaryKeys']
            for key in db[table].keys():
                if (key == 'primaryKeys') or (key in pKeys):
                    continue
                itemNo = 0
                for item in db[table][key]:
                    primaryKey = ''
                    for pKey in pKeys:
                        if pKey == pKeys[-1]:
                            primaryKey += formInertString(db[table][pKey][itemNo])
                        else:
                            primaryKey += formInertString(db[table][pKey][itemNo]) + ','
                    feature = formInertString(key) + '(' + primaryKey + ') ~ val(' + formInertString(item) + ').'
                    itemNo += 1
                    decomposedFeatures.append(feature)
            for i in range(0, len(db[table][pKeys[0]])):
                feature = formInertString(table) + '('
                for pKey in pKeys:
                    if pKey == pKeys[-1]:
                        feature += formInertString(db[table][pKey][i]) + ') := true.'
                    else:
                        feature += formInertString(db[table][pKey][i]) + ','
                decomposedFeatures.append(feature)
        return decomposedFeatures 

    def createTrainAndTestFile(self, features):
        omissionList = []
        testList = []
        featureList1 = []
        featureList2 = []
        featureList3 = []
        for feat in features:
            included = False
            for pred in OMISSION_PREDICATE:
                if pred in feat:
                    featureList2.append(feat)
                    included = True
                    break
                else:
                    pass
            for pred in TARGET_PREDICATE:
                if pred in feat:
                    featureList3.append(feat)
                    included = True
                    break
                else:
                    pass
            if not included:
                featureList1.append(feat)
        oNum = int((100 - OMISSION_PERCENTAGE)/10)
        tNum = int((100 - TEST_TRAIN_SPLIT)/10)
        featureList2_1 = []
        for i in range(0, len(featureList2)):
            if i%10 >= oNum:
                omissionList.append(featureList2[i])
            else:
                featureList2_1.append(featureList2[i])
        featureList3_1 = []
        for i in range(0, len(featureList3)):
            if i%10 >= tNum:
                testList.append(featureList3[i])
            else:
                featureList3_1.append(featureList3[i])
        self.omissionList = omissionList
        self.testList = testList
        return [featureList1, featureList2_1, featureList3_1, omissionList, testList]
    
    def findModeOfOmissions(self, numOfSamples, dcFileName):
        yapObj = YapPrologInterface()
        yapObj.consultWithOneFile(dcFileName)
        
        for feat in self.omissionList:
            feat = feat.replace(' ', '')
            featName = ''
            for i in feat:
                if i == '(':
                    break
                else:
                    featName+=i
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[featName]
            feat = feat.split('~')
            feat = feat[0]
            evidence = '[]'
            variable = '[X]'
            query = '(' + feat + ' ~= X)'
            samples = yapObj.sample(numOfSamples, query, evidence, variable)
            samples = samples.split(',')
            for i in range(0, len(samples)):
                samples[i] = samples[i].replace('[', '').replace(']', '')
            mode = self.findMode(samples, ty)
            if mode == None:
                pass
            else:
                feat = feat + '~= '
                feat += 'val(' + mode + ').'
                self.omissionListWithMode.append(feat)

    def appendOmissionListWithMode(self, dcFileName):
        f = open(dcFileName, 'a')
        f.write('\n%OmissionMode\n')
        for feat in self.omissionListWithMode:
            f.write(feat + '\n')
    
    def findMode(self, samples, ty):
        mode = None
        if ty == 'continuous':
            empty = False
            for i in range(0, len(samples)):
                if samples[i] == '':
                    empty =True
                    break
                else:
                    samples[i] = float(samples[i])
            if empty:
                mode = None
            else:
                average = sum(samples)/float(len(samples))
                mode = str(average)
        else:
            if samples[0] == '':
                mode = None
            else:
                mode = max(set(samples), key=samples.count)
                mode = str(mode)
        return mode
    
    def writeOmissionAndTestFile(self, fileName, features):
        [a,b,c,omission, test] = self.createTrainAndTestFile(features)
        f = open(fileName, 'w')
        f.write(STRING_AT_START)
        for feat in a:
            f.write(feat + '\n')
        for feat in b:
            f.write(feat + '\n')
        for feat in c:
            f.write(feat + '\n')
        f.write('\n%Background Theory\n')
        f.write(DC_BACKGROUND + '\n')
        f.write(STRING_AT_END)
        f.write('\n%Omission\n')
#        for feat in omission:
#            f.write(feat + '\n')
#        f.write('\n%OmissionMode\n')
        for feat in OMISSION_MODE:
            f.write(feat)
#        f.write('\n%Test\n')
        for feat in test:
            print(feat)
#             f.write(feat + '\n')
        f.close()
    
    def findTestAccuracy(self, numOfSamples, dcFileName):
        yapObj = YapPrologInterface()
        yapObj.consultWithOneFile(dcFileName)
        feat = self.testList[0]
        feat = feat.replace(' ', '')
        featName = ''
        for i in feat:
            if i == '(':
                break
            else:
                featName+=i
        ty = RANDOM_VARIABLE_PREDICATE_TYPE[featName]
        for feat in self.testList:
            feat = feat.replace(' ', '')
            feat = feat.split('~')
            feat[1] = feat[1].replace('val(', '')
            feat[1] = feat[1].replace(').', '')
            evidence = '[]'
            variable = '[X]'
            query = '(' + feat[0] + ' ~= X)'
            samples = yapObj.sample(numOfSamples, query, evidence, variable)
            samples = samples.split(',')
            for i in range(0, len(samples)):
                samples[i] = samples[i].replace('[', '').replace(']', '')
            self.updateAccuracyList(samples, feat[1], ty)
        if ty == 'continuous':
            acc = float('nan')
            if self.numOfElementsInTest == 0:
                print('MAE = %f, Number of tests consider = %d' % (acc, self.numOfElementsInTest))
            else:
                print('MAE = %f, Number of tests consider = %d' % (sum(self.error)/self.numOfElementsInTest, self.numOfElementsInTest))
        else:
            uniqueLabel = list(set(self.yPred + self.yTrue))
            print 'Confusion Matrix:'
            print confusion_matrix(self.yTrue, self.yPred, labels=uniqueLabel)
            print('Labels: '  + str(uniqueLabel))
            if self.numOfElementsInTest == 0:
                print('Accuracy: nan')
            else:
                print('Accuracy: %f'  % accuracy_score(self.yTrue, self.yPred))
            
    def updateAccuracyList(self, samples, originalValue, ty):
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
                self.error.append(abs(average - float(originalValue)))
                self.numOfElementsInTest+=1
        else:
            if samples[0] == '':
                pass
            else:
                yPred = max(set(samples), key=samples.count)
                self.yPred.append(yPred)
                self.yTrue.append(originalValue)
                self.numOfElementsInTest+=1
        
        
if __name__ == '__main__':
    [dbStats, dataBase] = fetchDataFromDB(DATABASE_NAME)
    obj = DCFeatureDecomposition()
    features = obj.featureDecomposition(dataBase)
    obj.writeOmissionAndTestFile('../data/dcHepatitisCont.pl', features)
    #obj.findModeOfOmissions(1000, '../data/dcHepatitisCont.pl')
    #print obj.omissionList
    #print '========== \n'
    #print obj.omissionListWithMode
    #obj.appendOmissionListWithMode('../data/dcHepatitisCont.pl')
    obj.findTestAccuracy(1000, '../data/dcHepatitisCont.pl')
#     f = open('../data/dcUniv.pl', 'w')
#     f.write(STRING_AT_START)
#     for feat in features:
#         f.write(feat + '\n')
#     f.write(STRING_AT_END)
#     f.close()

    
    
    
    