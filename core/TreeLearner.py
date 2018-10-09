'''
Created on Aug 28, 2017

@author: niteshkumar
'''
import time
import copy
import numpy as np
from TypesModesAggregationLanguage import TypesModesAggregationLanguage
from FeatureSpace import FeatureCreator
from PrologInterface import PrologInterface
from ScoreFinder import ScoreFinder
from yapInterface.YapInterface import YapInterface

class TreeLearner(object):
    EPSILON_SCORE = 0.1
    MIN_NUMBER_OF_DATAPOINT_AT_LEAF = 2
    NUM_OF_LOOKAHEADS = 1
    
    def __init__(self, inputFile, crossValidationFile, whichProlog, firstFeatureName):
        self.interface = PrologInterface(inputFile, whichProlog)
        #self.interfaceCrossValidation = PrologInterface(crossValidationFile, whichProlog)
        self.language = TypesModesAggregationLanguage(self.interface)
        self.scoreFinder = ScoreFinder()
        self.rules = []
        self.featureList = []
        self.featureListCopy = []
        self.targetType = ''
        self.targetVariable = ''
        self.targetPredicate = None
        self.targetPredicateString = ''
        self.featureHandler = None
        self.firstFeatureRestrictName = firstFeatureName
        self.isFirst = True
        
    
    def toStringOfRules(self, targetPredicate, currentStringOfFeatures, distribution):
        if currentStringOfFeatures == '':
            rule = targetPredicate + ' ~ ' + distribution + ' :- ' + 'true' + '.'
        else:
            rule = targetPredicate + ' ~ ' + distribution + ' :- ' + currentStringOfFeatures + '.'
        return rule
    
    def oneLookAhead(self, remainingLookAhead, validFeatureFlag, currentNumberOfDataPoint, currentStringOfFeatures, previousFeatureList, currentListOfContinuousVaribales):
        scores = []
        numPoints = []
        actualScores = []
        avgScores = []
        featureSelectedList = []
        actualNumberOfDataPoints = []
        featureId = 0
        for featureAndVar in self.featureList:
            if(validFeatureFlag[featureId] == 0):
                featureId += 1
                continue
            else:
                typeModeConformFeatures = self.featureHandler.getTypeModeConformFeatures(featureAndVar, previousFeatureList, self.targetPredicate)
                for feature in typeModeConformFeatures:
                    distributionVariables = []
                    featureString = feature.featureAsListOfString(self.language)
                    contVariables = feature.getContVariables()
                    for var in contVariables:
                        distributionVariables.append(var)
                    distributionVariables.append(self.targetVariable)
                    for var in currentListOfContinuousVaribales:
                        distributionVariables.append(var)
                    score = 0
                    numOfPoints = 0
                    actualScore = []
                    featureSelected = ''
                    actualNumberOfDataPoint = []
                    relF = False
                    if feature.getRandomVariableType() == '':
                        relF = True
                    else:
                        pass
                    for feat in featureString:
                        distributionVariablesCopy = copy.copy(distributionVariables)
                        contVariablesCopy = copy.copy(contVariables)
                        if feat[0] == '\\':
                            for v in contVariables:
                                distributionVariablesCopy.remove(v)
                                contVariablesCopy.remove(v)
                        else:
                            pass
                        if currentStringOfFeatures == '':
                            stringOfFeat = self.targetPredicateString + ', ' + feat
                        else:
                            stringOfFeat = self.targetPredicateString + ', ' + currentStringOfFeatures + ', ' + feat
                        #self.interface._consult()
                        prologResult = self.interface.listQuery(stringOfFeat, distributionVariablesCopy)
                        bigX = []
                        xVar = []
                        for var in currentListOfContinuousVaribales:
                            bigX.append(prologResult[var])
                            xVar.append(var)
                        for var in contVariablesCopy:
                            bigX.append(prologResult[var])
                            xVar.append(var)
                        bigY = prologResult[self.targetVariable]
                        if (relF == True) and (remainingLookAhead > 0):
                            relF = False
                            prevFeatLst = copy.copy(previousFeatureList)
                            prevFeatLst.append(feature)
                            valFeatFlag = copy.copy(validFeatureFlag)
                            valFeatFlag[featureId] = 0
                            if currentStringOfFeatures == '':
                                stFeat = feat
                            else:
                                stFeat = currentStringOfFeatures + ', ' + feat
                            scoreAndNumOfPoints = self.oneLookAhead(remainingLookAhead-1, valFeatFlag, len(bigY), stFeat, prevFeatLst, currentListOfContinuousVaribales)
                            sc = scoreAndNumOfPoints[0]
                            numP = scoreAndNumOfPoints[1]
                            lookaheadFeature = feat + ', ' + scoreAndNumOfPoints[2]
                        else:
                            #self.interfaceCrossValidation._consult()
                            #prologResult = self.interfaceCrossValidation.listQuery(stringOfFeat, distributionVariables)
                            #bigXCross = []
                            #for var in currentListOfContinuousVaribales:
                            #    bigXCross.append(prologResult[var])
                            #for var in contVariables:
                            #    bigXCross.append(prologResult[var])
                            #bigYCross = prologResult[self.targetVariable]
                            #scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, bigXCross, bigYCross, self.targetType, xVar)
                            scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, bigX, bigY, self.targetType, xVar)
                            sc = scoreAndDistribution['score']
                            numP = len(bigY)
                            lookaheadFeature = feat
                        if feat[0] == '\\':
                            pass
                        elif np.isnan(sc) or (len(bigY) < self.MIN_NUMBER_OF_DATAPOINT_AT_LEAF):
                            score += float('-inf')
                        else:
                            score += sc
                            numOfPoints += numP
                            featureSelected += lookaheadFeature + ';'
                        actualScore.append(sc)
                        actualNumberOfDataPoint.append(len(bigY))
                    totalPointsAfterApplyingFeature = sum(actualNumberOfDataPoint)
                    if (totalPointsAfterApplyingFeature == currentNumberOfDataPoint):
                        scores.append(score)
                        numPoints.append(numOfPoints)
                        featureSelectedList.append(featureSelected)
                        if numOfPoints == 0:
                            avgScores.append(float('-inf'))
                        else:
                            avgScores.append(score/float(numOfPoints))
                        actualScores.append(actualScore)
                        actualNumberOfDataPoints.append(actualNumberOfDataPoint)
                    elif(totalPointsAfterApplyingFeature > currentNumberOfDataPoint):
                        st = ' Total number of data points larger after applying the feature : ' + ''.join(featureString) + '\n and previous string of feature : ' + currentStringOfFeatures + '\n for the target : ' + self.targetPredicateString
                        raise Exception(st)
                    else:
                        pass
                featureId += 1
        score = float('-inf')
        featureId = -1
        idx = 0
        for s in avgScores:
            if(s > score):
                score = s
                featureId = idx
                idx += 1
            else:
                idx += 1
        if featureId == -1:
            return [float('-inf'), 0, '']
        else:
            return [scores[featureId], numPoints[featureId], featureSelectedList[featureId]]

    def findBestFeature(self, validFeatureFlag, currentScore, currentNumberOfDataPoint, currentStringOfFeatures, isRecentRelational, previousFeatureList, currentListOfContinuousVaribales):
        track = 0
        scores = []
        featureSelectedList = []
        avgScores = []
        actualFeatures = []
        actualScores = []
        actualNumberOfDataPoints = []
        actualNumberOfDataPointForScoreCals = []
        continuousVars = []
        featureIds = []
        featureObjects = []
        featureId = 0
        for featureAndVar in self.featureList:
            if(validFeatureFlag[featureId] == 0):
                featureId += 1
                continue
            else:
                typeModeConformFeatures = self.featureHandler.getTypeModeConformFeatures(featureAndVar, previousFeatureList, self.targetPredicate)
                for feature in typeModeConformFeatures:
                    distributionVariables = []
                    featureString = feature.featureAsListOfString(self.language)
                    contVariables = feature.getContVariables()
                    for var in contVariables:
                        distributionVariables.append(var)
                    distributionVariables.append(self.targetVariable)
                    for var in currentListOfContinuousVaribales:
                        distributionVariables.append(var)
                    score = 0
                    numOfPoints = 0
                    featureSelected = ''
                    actualScore = []
                    actualNumberOfDataPoint = []
                    actualNumberOfDataPointForScoreCal = []
                    actualFeature = []
                    relF = False
                    if feature.getRandomVariableType() == '':
                        relF = True
                    else:
                        pass
                    for feat in featureString:
                        distributionVariablesCopy = copy.copy(distributionVariables)
                        contVariablesCopy = copy.copy(contVariables)
                        if feat[0] == '\\':
                            for v in contVariables:
                                distributionVariablesCopy.remove(v)
                                contVariablesCopy.remove(v)
                        else:
                            pass
                        if currentStringOfFeatures == '':
                            stringOfFeat = self.targetPredicateString + ', ' + feat
                        else:
                            stringOfFeat = self.targetPredicateString + ', ' + currentStringOfFeatures + ', ' + feat
                        #self.interface._consult()
                        prologResult = self.interface.listQuery(stringOfFeat, distributionVariablesCopy)
                        bigX = []
                        xVar = []
                        for var in currentListOfContinuousVaribales:
                            bigX.append(prologResult[var])
                            xVar.append(var)
                        for var in contVariablesCopy:
                            bigX.append(prologResult[var])
                            xVar.append(var)
                        bigY = prologResult[self.targetVariable]
                        if relF == True:
                            relF = False
                            prevFeatLst = copy.copy(previousFeatureList)
                            prevFeatLst.append(feature)
                            valFeatFlag = copy.copy(validFeatureFlag)
                            valFeatFlag[featureId] = 0
                            if currentStringOfFeatures == '':
                                stFeat = feat
                            else:
                                stFeat = currentStringOfFeatures + ', ' + feat
                            scoreAndNumOfPoints = self.oneLookAhead(self.NUM_OF_LOOKAHEADS-1, valFeatFlag, len(bigY), stFeat, prevFeatLst, currentListOfContinuousVaribales)
                            sc = scoreAndNumOfPoints[0]
                            numP = scoreAndNumOfPoints[1]
                            lookaheadFeature = feat + ', ' + scoreAndNumOfPoints[2]
                        else:
                            #self.interfaceCrossValidation._consult()
                            #prologResult = self.interfaceCrossValidation.listQuery(stringOfFeat, distributionVariables)
                            #bigXCross = []
                            #for var in currentListOfContinuousVaribales:
                            #    bigXCross.append(prologResult[var])
                            #for var in contVariables:
                            #    bigXCross.append(prologResult[var])
                            #bigYCross = prologResult[self.targetVariable]
                            #scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, bigXCross, bigYCross, self.targetType, xVar)
                            scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, bigX, bigY, self.targetType, xVar)
                            sc = scoreAndDistribution['score']
                            numP = len(bigY)
                            lookaheadFeature = feat
                        if feat[0] == '\\':
                            pass
                        elif np.isnan(sc) or (len(bigY) < self.MIN_NUMBER_OF_DATAPOINT_AT_LEAF):
                            #score += float('-inf')
                            pass
                        else:
                            score += sc
                            numOfPoints += numP
                            featureSelected += lookaheadFeature + ';'
                        actualScore.append(sc)
                        actualFeature.append(feat)
                        actualNumberOfDataPoint.append(len(bigY))
                        actualNumberOfDataPointForScoreCal.append(numP)
                    totalPointsAfterApplyingFeature = sum(actualNumberOfDataPoint)
                    if (totalPointsAfterApplyingFeature == currentNumberOfDataPoint):
                        scores.append(score)
                        featureSelectedList.append(currentStringOfFeatures + ',' + featureSelected)
                        if numOfPoints == 0:
                            avgScores.append(float('-inf'))
                        else:
                            avgScores.append(score/float(numOfPoints))
                        actualScores.append(actualScore)
                        actualNumberOfDataPoints.append(actualNumberOfDataPoint)
                        actualNumberOfDataPointForScoreCals.append(actualNumberOfDataPointForScoreCal)
                        actualFeatures.append(actualFeature)
                        continuousVars.append(contVariables)
                        featureIds.append(featureId)
                        featureObjects.append(feature)
                    elif(totalPointsAfterApplyingFeature > currentNumberOfDataPoint):
                        st = ' Total number of data points larger after applying the feature : ' + ''.join(featureString) + '\n and previous string of feature : ' + currentStringOfFeatures + '\n for the target : ' + self.targetPredicateString
                        raise Exception(st)
                    else:
                        pass
                featureId += 1
        score = float('-inf')
        featureId = -1
        idx = 0
        for s in avgScores:
            if(s > score):
                score = s
                featureId = idx
                idx += 1
            else:
                idx += 1
        featureAndScore = None
        
        ## This is patch ... Rewrite it correctly.
        if(not (self.firstFeatureRestrictName == '') and self.isFirst):
            self.isFirst = False
            for i in range(0, len(actualFeatures)):
                feat = actualFeatures[i][0]
                if self.firstFeatureRestrictName in feat:
                    featureId = i
                    break
                else:
                    pass
            # truncatedActualFeature = [actualFeatures[featureId][0]]
            truncatedActualFeature = actualFeatures[featureId]
            featureAndScore = {'featureId': featureIds[featureId], 'scores': actualScores[featureId], 'actualFeatures': truncatedActualFeature, 'continuosVariables': continuousVars[featureId], 'featureObject': featureObjects[featureId], 'numOfDataPoints': actualNumberOfDataPoints[featureId], 'numOfDataPointsForCal': actualNumberOfDataPointForScoreCals[featureId]}
        ## Patch ends.
        
        elif((score - currentScore) > self.EPSILON_SCORE):
            featureAndScore = {'featureId': featureIds[featureId], 'scores': actualScores[featureId], 'actualFeatures': actualFeatures[featureId], 'continuosVariables': continuousVars[featureId], 'featureObject': featureObjects[featureId], 'numOfDataPoints': actualNumberOfDataPoints[featureId], 'numOfDataPointsForCal': actualNumberOfDataPointForScoreCals[featureId]}
        elif((isRecentRelational == True) and (score - currentScore) >= 0):
            featureAndScore = {'featureId': featureIds[featureId], 'scores': actualScores[featureId], 'actualFeatures': actualFeatures[featureId], 'continuosVariables': continuousVars[featureId], 'featureObject': featureObjects[featureId], 'numOfDataPoints': actualNumberOfDataPoints[featureId], 'numOfDataPointsForCal': actualNumberOfDataPointForScoreCals[featureId]}
        else:
            pass
        return featureAndScore
        
    def growTree(self, validFeatureFlag, currentScore, currentNumberOfDataPoint, currentStringOfFeatures, isRecentRelational, previousFeatureList, currentListOfContinuousVaribales):
        featureAndScore = self.findBestFeature(validFeatureFlag, currentScore, currentNumberOfDataPoint, currentStringOfFeatures, isRecentRelational, previousFeatureList, currentListOfContinuousVaribales)
        if featureAndScore == None:
            variables = [self.targetVariable]
            for var in currentListOfContinuousVaribales:
                variables.append(var)
            if currentStringOfFeatures == '':
                #self.interface._consult()
                prologResult = self.interface.listQuery(self.targetPredicateString, variables)
                #self.interfaceCrossValidation._consult()
                #prologResultCrossValidation = self.interfaceCrossValidation.listQuery(self.targetPredicateString, variables)
            else:
                #self.interface._consult()
                prologResult = self.interface.listQuery(self.targetPredicateString + ', ' + currentStringOfFeatures, variables)
                #self.interfaceCrossValidation._consult()
                #prologResultCrossValidation = self.interfaceCrossValidation.listQuery(self.targetPredicateString + ', ' + currentStringOfFeatures, variables)
            bigX = []
            xVar = []
            for var in currentListOfContinuousVaribales:
                bigX.append(prologResult[var])
                xVar.append(var)
            bigY = prologResult[self.targetVariable]
            #bigXCross = []
            #for var in currentListOfContinuousVaribales:
            #    bigXCross.append(prologResultCrossValidation[var])
            #bigYCross = prologResultCrossValidation[self.targetVariable]
            #scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, bigXCross, bigYCross, self.targetType, xVar)
            scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, bigX, bigY, self.targetType, xVar)
            distribution = scoreAndDistribution['distribution']
            self.rules.append(self.toStringOfRules(self.targetPredicateString, currentStringOfFeatures, distribution))
        else:
            featureId = featureAndScore['featureId']
            scores = featureAndScore['scores']
            actualFeatures = featureAndScore['actualFeatures']
            contVariables = featureAndScore['continuosVariables']
            numOfDataPoints = featureAndScore['numOfDataPoints']
            numOfDataPointsForCal = featureAndScore['numOfDataPointsForCal']
            objFeat = featureAndScore['featureObject']
            if objFeat.getRandomVariableType() == '':
                isRecentRelational = True
            else:
                isRecentRelational = False
            validFeatureFlag[featureId] = 0
            idx = 0
            for feat in actualFeatures:
                if feat[0] == '\\':
                    previousFeatureListCopy = copy.copy(previousFeatureList)
                    objFeat2 = copy.copy(objFeat)
                    objFeat2.setNegation(True)
                    previousFeatureListCopy.append(objFeat2)
                    currentListOfContinuousVaribalesCopy = copy.copy(currentListOfContinuousVaribales)
                else:
                    previousFeatureListCopy = copy.copy(previousFeatureList)
                    objFeat2 = copy.copy(objFeat)
                    objFeat2.setNegation(False)
                    previousFeatureListCopy.append(objFeat2)
                    currentListOfContinuousVaribalesCopy = copy.copy(currentListOfContinuousVaribales)
                    for var in contVariables:
                        currentListOfContinuousVaribalesCopy.append(var)
                if np.isnan(scores[idx]) or (numOfDataPoints[idx] < self.MIN_NUMBER_OF_DATAPOINT_AT_LEAF):
                    pass
                else:
                    newFeature = ''
                    if currentStringOfFeatures == '':
                        newFeature = feat
                    else:
                        newFeature = currentStringOfFeatures + ', ' + feat
                    sc = float('-inf')
                    if numOfDataPointsForCal[idx] > 0:
                        sc = scores[idx]/float(numOfDataPointsForCal[idx])
                    self.growTree(copy.copy(validFeatureFlag), sc, numOfDataPoints[idx], newFeature, isRecentRelational, previousFeatureListCopy, currentListOfContinuousVaribalesCopy)
                idx += 1
    
    def learnRules(self):
        self.featureHandler = FeatureCreator(self.language)
        self.featureListCopy = self.featureHandler.fillFeatureList()
        targets = self.language.target
        for target in targets:
            self.isFirst = True
            self.featureList = copy.copy(self.featureListCopy)
            self.language.setFreeVariable()
            for feature in self.featureListCopy:
                name = feature.getPredicateName()
                if name == target['target']:
                    self.featureList.remove(feature)
            validFeatureFlag = []
            for i in range(0, len(self.featureList)):
                validFeatureFlag.append(1)
            tPredicate = self.language.dictOfPredicates[target['target']]
            tPredicate = self.language.generateTargetPredicateVariables(tPredicate)
            tPredicate.addCurrentAggr('none')
            self.targetPredicate = tPredicate
            self.targetPredicateString = tPredicate.toString()
            self.targetType = target['randomValType']
            pos = target['targetLocation']
            val = tPredicate.getVariables()
            self.targetVariable = val[int(pos)-1]
            #self.interface._consult()
            prologResult = self.interface.listQuery(self.targetPredicateString, [self.targetVariable])
            bigY = prologResult[self.targetVariable]
            #self.interfaceCrossValidation._consult()
            #prologResult = self.interfaceCrossValidation.listQuery(self.targetPredicateString, [self.targetVariable])
            #bigYCross = prologResult[self.targetVariable]
            scoreAndDistribution = self.scoreFinder.getScore([], bigY, [], bigY, self.targetType, [])
            sc = float('-inf')
            if len(bigY) > 0:
                sc = scoreAndDistribution['score']/float(len(bigY))
            self.growTree(copy.copy(validFeatureFlag), sc, len(bigY), '', False, [], [])
        
if __name__ == '__main__':
    #outputFile = '../data/MyDCRules.pl'
    #f = open(outputFile, 'w')
    #obj = TreeLearner('../data/prologUniv.pl', '../data/interp1.pl', 'SWI-prolog', '')
    #obj.learnRules()
    #for rule in obj.rules:
    #    f.write(rule + '\n')
    #f.close()
    obj = YapInterface("../data/dcUniv.pl")
    obj.test()

    
