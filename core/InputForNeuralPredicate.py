'''
Created on Feb 15, 2018

@author: nitesh
'''
import pickle
from pyswip import Prolog
from PrologInterface import PrologInterface
from mercurial.fileset import predicate

class DumpData(object):
    dataDict = dict()
    listOfPredicates = ['current_posX', 'current_posY', 'current_posZ', 'next_posX', 'next_posY', 'next_posZ', 'move_behind_of', 'move_infront_of', 'move_left_of', 'move_right_of', 'behind_of', 'infront_of', 'left_of', 'right_of']
    predicateArgs = '(X, Y, Z).'
    
    def __init__(self, prologObj, fileHandle):
        self.prologObj = prologObj
        self.fileHandle = fileHandle
    
    def readData(self):
        for pred in self.listOfPredicates:
            res = self.prologObj.simpleQuery(pred + self.predicateArgs)
            for ele in res:
                sampleId = ele['X']
                sampleId = int(sampleId.replace('sample', ''))
                predicate = []
                predicate.append(pred)
                predicate.append(str(ele['Y']))
                predicate.append(str(ele['Z']))
                if(self.dataDict.has_key(sampleId)):
                    val = self.dataDict.get(sampleId)
                    val.append(predicate)
                    self.dataDict[sampleId] = val
                else:
                    self.dataDict[sampleId] = [predicate]
    
    def writeData(self):
        keys = self.dataDict.keys()
        dataList = []
        for key in keys:
            dataEntry = dict()
            data = self.dataDict[key]
            for rec in data:
                if 'current' in rec[0]:
                    if dataEntry.has_key('current_anchors'):
                        d = dataEntry['current_anchors']
                        obj = rec[1]
                        if d.has_key(obj):
                            pos = d[obj]
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        else:
                            pos = dict()
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        dataEntry['current_anchors'] = d
                    else:
                        d = dict()
                        obj = rec[1]
                        if d.has_key(obj):
                            pos = d[obj]
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        else:
                            pos = dict()
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        dataEntry['current_anchors'] = d
                elif 'next' in rec[0]:
                    if dataEntry.has_key('next_anchors'):
                        d = dataEntry['next_anchors']
                        obj = rec[1]
                        if d.has_key(obj):
                            pos = d[obj]
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        else:
                            pos = dict()
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        dataEntry['next_anchors'] = d
                    else:
                        d = dict()
                        obj = rec[1]
                        if d.has_key(obj):
                            pos = d[obj]
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        else:
                            pos = dict()
                            if 'posX' in rec[0]:
                                pos['X'] = float(rec[2])
                            elif 'posY' in rec[0]:
                                pos['Y'] = float(rec[2])
                            elif 'posZ' in rec[0]:
                                pos['Z'] = float(rec[2])
                            else:
                                pass
                            d[obj] = pos
                        dataEntry['next_anchors'] = d
                elif 'move' in rec[0]:
                    dataEntry['action'] = [rec]
                else:
                    dataEntry['current_predicates'] = [rec]
            for k in dataEntry['current_anchors'].keys():
                ob = dataEntry['current_anchors'][k]
                li = []
                li.append(ob['X'])
                li.append(ob['Y'])
                li.append(ob['Z'])
                dataEntry['current_anchors'][k] = li
            for k in dataEntry['next_anchors'].keys():
                ob = dataEntry['next_anchors'][k]
                li = []
                li.append(ob['X'])
                li.append(ob['Y'])
                li.append(ob['Z'])
                dataEntry['next_anchors'][k] = li
            dataList.append(dataEntry)
        print dataList[0]
        pickle.dump(dataList, self.fileHandle)

if __name__ == '__main__':
    obj = PrologInterface('../data/dataWithNeuralPred.pl', 'SWI-prolog')
    f = open('../data/dictionaryOfSamples.p', 'wb')
    dumper = DumpData(obj, f)
    dumper.readData()
    dumper.writeData()
    f.close()
    
