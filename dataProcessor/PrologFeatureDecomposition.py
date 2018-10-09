'''
Created on May 25, 2018

@author: nitesh
'''
from DataFetcher import fetchDataFromDB
from Utils import formInertString
from settings.DatabaseSettings import *
from settings.PrologFileSettings import DECLARATIVE_BIAS
from core.YapPrologInterface import YapPrologInterface

class PrologFeatureDecomposition(object):
    
    def __init__(self):
        self.omissionListEnumerated = []
        self.omissionListDC = []
        
        self.omissionList = []
        self.testList = []
        self.a = []
        self.b = []
        self.c = []
        self.omissionListWithMode = []
        self.omissionListWithModeDC = []
    
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
                        primaryKey += formInertString(db[table][pKey][itemNo]) + ','
                    feature = formInertString(key) + '(' + primaryKey + formInertString(item) + ').'
                    itemNo += 1
                    decomposedFeatures.append(feature)
            for i in range(0, len(db[table][pKeys[0]])):
                feature = formInertString(table) + '('
                for pKey in pKeys:
                    if pKey == pKeys[-1]:
                        feature += formInertString(db[table][pKey][i]) + ').'
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
        self.a = featureList1
        self.b = featureList2_1
        self.c = featureList3_1
    
    def enumerateAllValuesOfOmissions(self):
        self.omissionListEnumerated = []
        for feat in self.omissionList:
            feat = feat.replace(' ', '')
            featName = ''
            for i in feat:
                if i == '(':
                    break
                else:
                    featName+=i
            domain = OMISSION_PREDICATE_DOMAIN[featName]
            if len(domain) == 0:
                feat1 = feat.split(',')
                feat1 = ','.join(feat1[0:-1])
                feat1 += ',' + str(-1) + ').'
                self.omissionListEnumerated.append(feat1)
            else:
                for dom in domain:
                    feat1 = feat.split(',')
                    feat1 = ','.join(feat1[0:-1])
                    feat1 += ',' + dom + ').'
                    self.omissionListEnumerated.append(feat1)
        
    def findModeOfOmissions(self, numOfSamples, dcFileName):
        yapObject = YapPrologInterface()
        yapObject.consultWithOneFile(dcFileName)
        yapObject.setNumOfSamples(numOfSamples)
        for feat in self.omissionList:
            feat = feat.replace(' ', '')
            featName = ''
            for i in feat:
                if i == '(':
                    break
                else:
                    featName+=i
            ty = RANDOM_VARIABLE_PREDICATE_TYPE[featName]
            feat = feat.split(',')
            feat = ','.join(feat[0:-1])
            feat += ')'
            evidence = '[]'
            variable = '[X]'
            query = '(' + feat + ' ~= X)'
            samples = yapObject.sample(numOfSamples, query, evidence, variable)
            samples = samples.split(',')
            for i in range(0, len(samples)):
                samples[i] = samples[i].replace('[', '').replace(']', '')
            mode = self.findMode(samples, ty)
            if mode == None:
                pass
            else:
                feat1 = feat
                feat = feat.replace(')', ',')
                feat += mode + ').'
                feat1 = feat1 + ' ~ '
                feat1+= 'val(' + mode + ').'
                self.omissionListWithMode.append(feat)
                self.omissionListWithModeDC.append(feat1)
                
    def findOmissionListDC(self):
        self.omissionListDC = []
        for feat in self.omissionList:
            feat = feat.replace(' ', '')
            featName = ''
            for i in feat:
                if i == '(':
                    break
                else:
                    featName+=i
            feat = feat.split(',')
            value = feat[-1][0:-2]
            feat = ','.join(feat[0:-1])
            feat += ')'
            feat = feat + ' ~ ' + 'val(' + value + ').'
            self.omissionListDC.append(feat)
    
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
    
    def writeOmissionAndTestFile(self, fileName, fileName1, fileName2, features):
        #self.createTrainAndTestFile(features)
        f2 = open(fileName2, 'w')
        f1 = open(fileName1, 'w')
        f = open(fileName, 'w')
        f.write(DECLARATIVE_BIAS)
        for feat in self.a:
            f.write(feat + '\n')
        for feat in self.b:
            f.write(feat + '\n')
        for feat in self.c:
            f.write(feat + '\n')
        f1.write('\n%Omission\n')
        for feat in self.omissionList:
            f1.write(feat + '\n')
            
        f1.write('\n%OmissionDC\n')
        for feat in self.omissionListDC:
            f1.write(feat + '\n')
            
        f1.write('\n%OmissionMode\n')
        for feat in self.omissionListWithMode:
            f1.write(feat + '\n')
        f1.write('\n%OmissionModeDC\n')
        for feat in self.omissionListWithModeDC:
            f1.write(feat + '\n')

        f2.write('\n%OmissionEnumerated\n')
        for feat in self.omissionListEnumerated:
            f2.write(feat + '\n')
        f2.close()

        f1.write('\n%Test\n')
        for feat in self.testList:
            f1.write(feat + '\n')
        f.close()
        f1.close()

if __name__ == '__main__':
    [dbStats, dataBase] = fetchDataFromDB(DATABASE_NAME)
    obj = PrologFeatureDecomposition()
    features = obj.featureDecomposition(dataBase)
    obj.createTrainAndTestFile(features)
    #obj.findModeOfOmissions(1000, '../data/dcHepatitisCont.pl')
    obj.enumerateAllValuesOfOmissions()
    obj.findOmissionListDC()
    obj.writeOmissionAndTestFile('../data/hepatitisCont.pl', '../data/hepatitisOmissionCont.pl', '../data/hepatitisEnumeratedCont.pl', features)
#     f = open('../data/prologUniv.pl', 'w')
#     f.write(DECLARATIVE_BIAS)
#     for feat in features:
#         f.write(feat + '\n')
#     f.close()
    
    
    
    