'''
Created on Aug 28, 2017

@author: niteshkumar
'''
import sys
import time
import logging
import copy
import numpy as np
from TypesModesAggregationLanguage import TypesModesAggregationLanguage
from FeatureSpace import FeatureCreator
#from PrologInterface import PrologInterface
from YapPrologInterface import YapPrologInterface
from ScoreFinderProbabilistic import ScoreFinderProbabilistic
from TranslateToDC import TranslateToDC

class TreeLearnerProbabilistic(object):
    NUM_BIN = 10
    BIN_SIZE = 500
    NUM_OBJ = 3
    NUM_EXMP = (NUM_BIN - 1)*BIN_SIZE
    EPSILON_SCORE = 5
    #MIN_NUMBER_OF_DATAPOINT_AT_LEAF = int(NUM_EXMP*NUM_OBJ/NUM_OBJ**NUM_OBJ)
    NUM_OF_LOOKAHEADS = 3
    NUM_OF_SAMPLES = 100
    def __init__(self, inputFile, dcFileName, whichProlog, firstFeatureName, randomVariablePredicates, num_examples):
        self.MIN_NUMBER_OF_DATAPOINT_AT_LEAF = num_examples
        #self.interface = PrologInterface(inputFile, whichProlog)
        logging.basicConfig(level=logging.DEBUG, filename="treeLearnerLogs", filemode="w", format="%(asctime)-15s %(levelname)-8s %(message)s")
        self.interface = YapPrologInterface(randomVariablePredicates)
        if dcFileName == '':
            self.runProbMode = False
            self.interface.consultWithOneFile(inputFile)
        else:
            self.runProbMode = True
            self.interface.consultWithTwoFiles(inputFile, dcFileName, self.NUM_OF_SAMPLES)
        #self.interfaceCrossValidation = PrologInterface(crossValidationFile, whichProlog)
        self.language = TypesModesAggregationLanguage(self.interface)
        self.scoreFinder = ScoreFinderProbabilistic()
        self.rules = []
        self.rulesFragmented = []
        self.featureList = []
        self.featureListCopy = []
        self.targetType = ''
        self.targetVariable = ''
        self.targetPredicate = None
        self.listOfSubstitutionVars = None
        self.targetPredicateString = ''
        self.featureHandler = None
        self.firstFeatureRestrictName = firstFeatureName
        self.isFirst = True
        self.defaultRuleAddedForCurrentTarget = False
        
    
    def toStringOfRules(self, targetPredicate, currentStringOfFeatures, distribution):
        if currentStringOfFeatures == '':
            rule = targetPredicate + ' ~ ' + distribution + ' :- ' + 'true' + '.'
            self.defaultRuleAddedForCurrentTarget = True
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
                            body = feat
                        else:
                            stringOfFeat = self.targetPredicateString + ', ' + currentStringOfFeatures + ', ' + feat
                            body = currentStringOfFeatures + ', ' + feat
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
                        numOfExamples = len(bigY)
                        if self.runProbMode:
                            if len(xVar) == 0:
                                bigP = self.interface.getProbabilityOfQuery(stringOfFeat, body, self.listOfSubstitutionVars)
                            else:
                                samplesWithProb = self.interface.getSamplesAndProbabilityOfQuery(stringOfFeat, body, self.listOfSubstitutionVars, xVar)
                                updatedXYP = self.handleProbabilisticContinuousVariable(samplesWithProb, bigY, xVar)
                                bigX = updatedXYP['bigX']
                                bigY = updatedXYP['bigY']
                                bigP = updatedXYP['bigP']
                                numOfExamples = updatedXYP['numOfExamples']
                        else:
                            bigP = []
                            for i in range(0, numOfExamples):
                                bigP.append(1.0)
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
                            scoreAndNumOfPoints = self.oneLookAhead(remainingLookAhead-1, valFeatFlag, numOfExamples, stFeat, prevFeatLst, currentListOfContinuousVaribales)
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
                            if (len(xVar) > 0) and (numOfExamples <= 1) and (bigY != []):
                                pass
                            scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, self.targetType, xVar, bigP, numOfExamples, self.NUM_OF_SAMPLES)
                            sc = scoreAndDistribution['score']
                            numP = numOfExamples
                            lookaheadFeature = feat
                        if np.isnan(sc) or (numOfExamples < self.MIN_NUMBER_OF_DATAPOINT_AT_LEAF):
                            #score += float('-inf')
                            pass
                        elif feat[0] == '\\':
                            score += sc
                            numOfPoints += numP
                        else:
                            score += sc
                            numOfPoints += numP
                            featureSelected += lookaheadFeature + ';'
                        actualScore.append(sc)
                        actualNumberOfDataPoint.append(numOfExamples)
                    totalPointsAfterApplyingFeature = sum(actualNumberOfDataPoint)
                    if (totalPointsAfterApplyingFeature == currentNumberOfDataPoint):
                        scores.append(score)
                        numPoints.append(numOfPoints)
                        featureSelectedList.append(featureSelected)
                        if numOfPoints == 0:
                            avgScores.append(float('-inf'))
                        else:
                            avgScores.append(score) #avgScores.append(score/float(numOfPoints))
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

    def handleProbabilisticContinuousVariable(self, samplesWithProb, bigY, xVar):
        numOfExamples = len(bigY)
        bigX = {}
        for var in xVar:
            bigX[var] = []
        updatedbigX = []
        updatedbigY = []
        updatedbigP = []
        updatedNumOfEamples = 0
        for i in range(0, numOfExamples):
            for sample in samplesWithProb[i]:
                xsample = sample[0]
                for j in range(0, len(xVar)):
                    a = bigX[xVar[j]]
                    a.append(xsample[j])
                updatedbigY.append(bigY[i])
                updatedbigP.append(sample[3])
            if len(samplesWithProb[i]) == 0:
                pass
            else:
                updatedNumOfEamples += 1
        for i in range(0, len(xVar)):
            updatedbigX.append(bigX[xVar[i]])
            
        updatedXYP = dict()
        updatedXYP['bigX'] = updatedbigX
        updatedXYP['bigY'] = updatedbigY
        updatedXYP['bigP'] = updatedbigP
        updatedXYP['numOfExamples'] = updatedNumOfEamples
        return updatedXYP
        
    def findBestFeature(self, validFeatureFlag, currentScore, currentNumberOfDataPoint, currentStringOfFeatures, isRecentRelational, previousFeatureList, currentListOfContinuousVaribales):
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
                            body = feat
                        else:
                            stringOfFeat = self.targetPredicateString + ', ' + currentStringOfFeatures + ', ' + feat
                            body = currentStringOfFeatures + ', ' + feat
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
                        numOfExamples = len(bigY)
                        if self.runProbMode:
                            if len(xVar) == 0:
                                bigP = self.interface.getProbabilityOfQuery(stringOfFeat, body, self.listOfSubstitutionVars)
                            else:
                                samplesWithProb = self.interface.getSamplesAndProbabilityOfQuery(stringOfFeat, body, self.listOfSubstitutionVars, xVar)
                                updatedXYP = self.handleProbabilisticContinuousVariable(samplesWithProb, bigY, xVar)
                                bigX = updatedXYP['bigX']
                                bigY = updatedXYP['bigY']
                                bigP = updatedXYP['bigP']
                                numOfExamples = updatedXYP['numOfExamples']
                        else:
                            bigP = []
                            for i in range(0, numOfExamples):
                                bigP.append(1.0)
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
                            scoreAndNumOfPoints = self.oneLookAhead(self.NUM_OF_LOOKAHEADS-1, valFeatFlag, numOfExamples, stFeat, prevFeatLst, currentListOfContinuousVaribales)
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
                            if (len(xVar) > 0) and (numOfExamples <= 1) and (bigY != []):
                                pass
                            scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, self.targetType, xVar, bigP, numOfExamples, self.NUM_OF_SAMPLES)
                            sc = scoreAndDistribution['score']
                            numP = numOfExamples
                            lookaheadFeature = feat
                        if np.isnan(sc) or (numOfExamples < self.MIN_NUMBER_OF_DATAPOINT_AT_LEAF):
                            #score += float('-inf')
                            pass
                        elif feat[0] == '\\':
                            score += sc
                            numOfPoints += numP
                        else:
                            score += sc
                            numOfPoints += numP
                            featureSelected += lookaheadFeature + ';'
                        actualScore.append(sc)
                        actualFeature.append(feat)
                        actualNumberOfDataPoint.append(numOfExamples)
                        actualNumberOfDataPointForScoreCal.append(numP)
                    totalPointsAfterApplyingFeature = sum(actualNumberOfDataPoint)
                    if (totalPointsAfterApplyingFeature == currentNumberOfDataPoint or self.runProbMode or True):
                        scores.append(score)
                        featureSelectedList.append(currentStringOfFeatures + ',' + featureSelected)
                        tempPatch = actualNumberOfDataPoint
                        tempPatch = [value for value in tempPatch if not value == 0]
                        lenTempPatch = len(tempPatch)
                        if lenTempPatch == 0:
                            avgScores.append(float('-inf'))
                        else:
                            avgScores.append(score) #avgScores.append(score/float(lenTempPatch))
                        actualScores.append(actualScore)
                        actualNumberOfDataPoints.append(actualNumberOfDataPoint)
                        actualNumberOfDataPointForScoreCals.append(actualNumberOfDataPointForScoreCal)
                        actualFeatures.append(actualFeature)
                        continuousVars.append(contVariables)
                        featureIds.append(featureId)
                        featureObjects.append(feature)
                        s = 'Current Score: ' + str(currentScore) + '  TargetPredicate: ' + str(self.targetPredicateString) +   ' Actual Features: ' + str(actualFeatures) + '  Actual Scores: ' + str(actualScores) + '  Actual DataPoints: ' + str(actualNumberOfDataPoints)
                        logging.debug(s)
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
        
        logging.info('Target feature:')
        logging.info(self.targetPredicateString)
        logging.info('Current String of features:')
        logging.info(currentStringOfFeatures)
        logging.info('Score Difference:')
        logging.info(score - currentScore)
        logging.info('Scores:')
        logging.info(actualScores)
        logging.info('Actual features:')
        logging.info(actualFeatures)
        logging.info('Current Score:')
        logging.info(currentScore)
        logging.info('Score:')
        logging.info(score)
        logging.info('FeatureId:')
        logging.info(featureId)
        logging.info('Selected Feature:')
        if featureId == -1:
            logging.info('actualFeatures cannot be found because featureId is -1')
        else:
            logging.info(actualFeatures[featureId])
        logging.info('\n\n')
        
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
        
    def growTree(self, validFeatureFlag, currentScore, currentNumberOfDataPoint, currentStringOfFeatures, currentListOfFeatures, isRecentRelational, previousFeatureList, currentListOfContinuousVaribales):
        featureAndScore = self.findBestFeature(validFeatureFlag, currentScore, currentNumberOfDataPoint, currentStringOfFeatures, isRecentRelational, previousFeatureList, currentListOfContinuousVaribales)
        if featureAndScore == None:
            variables = [self.targetVariable]
            for var in currentListOfContinuousVaribales:
                variables.append(var)
            bigX = []
            xVar = []
            if currentStringOfFeatures == '':
                #self.interface._consult()
                prologResult = self.interface.listQuery(self.targetPredicateString, variables)
                body = 'true'
                bigY = prologResult[self.targetVariable]
                for var in currentListOfContinuousVaribales:
                    bigX.append(prologResult[var])
                    xVar.append(var)
                numOfExamples = len(bigY)
                if self.runProbMode:
                    if len(xVar) == 0:
                        bigP = self.interface.getProbabilityOfQuery(self.targetPredicateString, body, self.listOfSubstitutionVars)
                    else:
                        samplesWithProb = self.interface.getSamplesAndProbabilityOfQuery(self.targetPredicateString, body, self.listOfSubstitutionVars, xVar)
                        updatedXYP = self.handleProbabilisticContinuousVariable(samplesWithProb, bigY, xVar)
                        bigX = updatedXYP['bigX']
                        bigY = updatedXYP['bigY']
                        bigP = updatedXYP['bigP']
                        numOfExamples = updatedXYP['numOfExamples']
                else:
                    bigP = []
                    for i in range(0, numOfExamples):
                        bigP.append(1.0)
                #self.interfaceCrossValidation._consult()
                #prologResultCrossValidation = self.interfaceCrossValidation.listQuery(self.targetPredicateString, variables)
            else:
                #self.interface._consult()
                prologResult = self.interface.listQuery(self.targetPredicateString + ', ' + currentStringOfFeatures, variables)
                body = currentStringOfFeatures
                bigY = prologResult[self.targetVariable]
                for var in currentListOfContinuousVaribales:
                    bigX.append(prologResult[var])
                    xVar.append(var)
                numOfExamples = len(bigY)
                if self.runProbMode:
                    if len(xVar) == 0:
                        bigP = self.interface.getProbabilityOfQuery(self.targetPredicateString + ', ' + currentStringOfFeatures, body, self.listOfSubstitutionVars)
                    else:
                        samplesWithProb = self.interface.getSamplesAndProbabilityOfQuery(self.targetPredicateString + ', ' + currentStringOfFeatures, body, self.listOfSubstitutionVars, xVar)
                        updatedXYP = self.handleProbabilisticContinuousVariable(samplesWithProb, bigY, xVar)
                        bigX = updatedXYP['bigX']
                        bigY = updatedXYP['bigY']
                        bigP = updatedXYP['bigP']
                        numOfExamples = updatedXYP['numOfExamples']
                else:
                    bigP = []
                    for i in range(0, numOfExamples):
                        bigP.append(1.0)
                
                #self.interfaceCrossValidation._consult()
                #prologResultCrossValidation = self.interfaceCrossValidation.listQuery(self.targetPredicateString + ', ' + currentStringOfFeatures, variables)


            #bigXCross = []
            #for var in currentListOfContinuousVaribales:
            #    bigXCross.append(prologResultCrossValidation[var])
            #bigYCross = prologResultCrossValidation[self.targetVariable]
            #scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, bigXCross, bigYCross, self.targetType, xVar)
            if (len(xVar) > 0) and (numOfExamples <= 1) and (bigY != []):
                pass
            scoreAndDistribution = self.scoreFinder.getScore(bigX, bigY, self.targetType, xVar, bigP, numOfExamples, self.NUM_OF_SAMPLES)
            distribution = scoreAndDistribution['distribution']
            self.rules.append(self.toStringOfRules(self.targetPredicateString, currentStringOfFeatures, distribution))
            ruleFragment = {}
            ruleFragment[self.targetPredicateString] = currentListOfFeatures
            self.rulesFragmented.append(ruleFragment)
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
                    newFeatureList = []
                    if currentStringOfFeatures == '':
                        newFeature = feat
                        newFeatureList = [feat]
                    else:
                        newFeature = currentStringOfFeatures + ', ' + feat
                        newFeatureList = currentListOfFeatures + [feat]
                    sc = float('-inf')
                    if numOfDataPointsForCal[idx] > 0:
                        sc = scores[idx] #TODO Removed division by number of points
                    self.growTree(copy.copy(validFeatureFlag), sc, numOfDataPoints[idx], newFeature, newFeatureList, isRecentRelational, previousFeatureListCopy, currentListOfContinuousVaribalesCopy)
                idx += 1
    

    def learnRule(self, target):
        self.defaultRuleAddedForCurrentTarget = False
        self.isFirst = True
        self.featureHandler = FeatureCreator(self.language, target['target'])
        self.language.setTargetPredicateName(target['target'])
        self.featureListCopy = self.featureHandler.fillFeatureList()
        self.featureList = copy.copy(self.featureListCopy)
        self.language.setFreeVariable()
        for feature in self.featureListCopy:
            name = feature.getPredicateName()
            if name == target['target']:
                #self.featureList.remove(feature) # uncomment this
                pass
        validFeatureFlag = []
        for i in range(0, len(self.featureList)):
            validFeatureFlag.append(1)
        tPredicate = self.language.dictOfPredicates[target['target']]
        tPredicate = self.language.generateTargetPredicateVariables(tPredicate)
        tPredicate.addCurrentAggr('none')
        self.targetPredicate = tPredicate
        self.targetPredicateString = tPredicate.toString()
        
        self.targetType = target['randomValType']
        self.listOfSubstitutionVars = tPredicate.getVariables()
        pos = target['targetLocation']
        val = tPredicate.getVariables()
        self.targetVariable = val[int(pos)-1]
        #self.interface._consult()
        prologResult = self.interface.listQuery(self.targetPredicateString, [self.targetVariable])
        body = 'true'
        bigY = prologResult[self.targetVariable]
        numOfExamples = len(bigY)
        if self.runProbMode:
            bigP = self.interface.getProbabilityOfQuery(self.targetPredicateString, body, self.listOfSubstitutionVars)
        else:
            bigP = []
            for i in range(0, numOfExamples):
                bigP.append(1.0)
        #self.interfaceCrossValidation._consult()
        #prologResult = self.interfaceCrossValidation.listQuery(self.targetPredicateString, [self.targetVariable])
        #bigYCross = prologResult[self.targetVariable]
        if (numOfExamples <= 1) and (bigY != []):
            pass
        scoreAndDistribution = self.scoreFinder.getScore([], bigY, self.targetType, [], bigP, numOfExamples, self.NUM_OF_SAMPLES)
        sc = float('-inf')
        if len(bigY) > 0:
            sc = scoreAndDistribution['score'] # TODO: Check
        self.growTree(copy.copy(validFeatureFlag), sc, numOfExamples, '', [], False, [], [])
        if not self.defaultRuleAddedForCurrentTarget:
            distribution = scoreAndDistribution['distribution']
            self.rules.append(self.toStringOfRules(self.targetPredicateString, '', distribution))
            ruleFragment = {}
            ruleFragment[self.targetPredicateString] = []
            self.rulesFragmented.append(ruleFragment)
    
    def generateDependencyStructureOfRules(self, listOfDictOfRules):
        predicateNames = self.language.dictOfPredicates.keys()
        dependency = {}
        for rule in listOfDictOfRules:
            ruleHead = rule.keys()[0]
            ruleHeadName = ''
            for pName in predicateNames:
                ppName = pName+'('
                if ppName in ruleHead:
                    if dependency.has_key(pName):
                        pass
                    else:
                        dependency[pName] = set()
                    ruleHeadName = pName
                    break
            
            for atom in rule[ruleHead]:
                tempSet = ()
                for pName in predicateNames:
                    ppName = pName+'('
                    if ppName in atom:
                        tempSet += (pName,)
                dependency[ruleHeadName].add(tempSet)
        dependencyStructure = {}
        for target in dependency.keys():
            dependencyStructure[target] = []
            for atom in dependency[target]:
                if atom == ():
                    pass
                else:
                    tempList = []
                    for eachAtom in atom:
                        tempList.append(eachAtom)
                    dependencyStructure[target].append(tempList)
        return dependencyStructure
    
    def generateBasePredicates(self):
        basePredicates = {}
        for key in self.language.dictOfPredicates.keys():
            basePredicates[key] = self.language.dictOfPredicates[key].getBasePredicate()
        return basePredicates

    def learnRules(self):
        targets = self.language.target
        for target in targets:
            self.defaultRuleAddedForCurrentTarget = False
            self.isFirst = True
            self.featureHandler = FeatureCreator(self.language, target['target'])
            self.language.setTargetPredicateName(target['target'])
            self.featureListCopy = self.featureHandler.fillFeatureList()
            self.featureList = copy.copy(self.featureListCopy)
            self.language.setFreeVariable()
            for feature in self.featureListCopy:
                name = feature.getPredicateName()
                if name == target['target']:
                    #self.featureList.remove(feature) # uncomment this
                    pass
            validFeatureFlag = []
            for i in range(0, len(self.featureList)):
                validFeatureFlag.append(1)
            tPredicate = self.language.dictOfPredicates[target['target']]
            tPredicate = self.language.generateTargetPredicateVariables(tPredicate)
            tPredicate.addCurrentAggr('none')
            self.targetPredicate = tPredicate
            self.targetPredicateString = tPredicate.toString()
            
            self.targetType = target['randomValType']
            self.listOfSubstitutionVars = tPredicate.getVariables()
            pos = target['targetLocation']
            val = tPredicate.getVariables()
            self.targetVariable = val[int(pos)-1]
            #self.interface._consult()
            prologResult = self.interface.listQuery(self.targetPredicateString, [self.targetVariable])
            body = 'true'
            bigY = prologResult[self.targetVariable]
            numOfExamples = len(bigY)
            if self.runProbMode:
                bigP = self.interface.getProbabilityOfQuery(self.targetPredicateString, body, self.listOfSubstitutionVars)
            else:
                bigP = []
                for i in range(0, numOfExamples):
                    bigP.append(1.0)
            #self.interfaceCrossValidation._consult()
            #prologResult = self.interfaceCrossValidation.listQuery(self.targetPredicateString, [self.targetVariable])
            #bigYCross = prologResult[self.targetVariable]
            if (numOfExamples <= 1) and (bigY != []):
                pass
            scoreAndDistribution = self.scoreFinder.getScore([], bigY, self.targetType, [], bigP, numOfExamples, self.NUM_OF_SAMPLES)
            sc = float('-inf')
            if len(bigY) > 0:
                sc = scoreAndDistribution['score'] # TODO: Check
            self.growTree(copy.copy(validFeatureFlag), sc, numOfExamples, '', [], False, [], [])
            if not self.defaultRuleAddedForCurrentTarget:
                distribution = scoreAndDistribution['distribution']
                self.rules.append(self.toStringOfRules(self.targetPredicateString, '', distribution))
                ruleFragment = {}
                ruleFragment[self.targetPredicateString] = []
                self.rulesFragmented.append(ruleFragment)
        #self.interface.terminate()
        #del self.interface
  
def learn_rules_det(in_name, out_name,scen,num_exmp,first_feat):
  dir_name = '../data/cross_validation/'
  #dir_name = ''
  RND_VARIABLE = ['posX_t0', 'posX_t1', 'posY_t0', 'posY_t1', 'shape', 'heavy', 'move_left_of', 'move_north_of', 'almove_left_of', 'armove_left_of', 'displX', 'displY', 'left_of', 'north_of', 'l_o_shape', 'l_o_posX_t0', 'atleastOne', 'latleastOne', 'ratleastOne', 'mmshl', 'mmshr', 'avglshpos', 'avgrshpos', 'all_combined', 'atleastOneLeft', 'atleastOneNorth', 'atleastOneRight', 'atleastOneSouth', 'blocked_left', 'blocked_right', 'blocked_north', 'blocked_south', 'mlo', 'mno', 'mro', 'mso', 'case']
  f = open(dir_name+scen + out_name, 'w')
  obj = TreeLearnerProbabilistic(dir_name+scen + in_name, '', 'SWI-prolog', first_feat, RND_VARIABLE, int(num_exmp))
  obj.learnRules()
  obj1 = TranslateToDC()
  obj1.setRandomVariablePredicates(RND_VARIABLE)
  for rule in obj.rules:
    f.write(obj1.translate(rule) + '\n')
  f.close()
def learn_rules_noisy(in_name_det, in_name_noisy, out_name,scen, num_exmp,first_feat):
  dir_name = '../data/cross_validation/'
  #dir_name = ''
  RND_VARIABLE = ['posX_t0', 'posX_t1', 'posY_t0', 'posY_t1', 'shape', 'heavy', 'move_left_of', 'move_north_of', 'almove_left_of', 'armove_left_of', 'displX', 'displY', 'left_of', 'north_of', 'l_o_shape', 'l_o_posX_t0', 'atleastOne', 'latleastOne', 'ratleastOne', 'mmshl', 'mmshr', 'avglshpos', 'avgrshpos', 'all_combined', 'atleastOneLeft', 'atleastOneNorth', 'atleastOneRight', 'atleastOneSouth', 'blocked_left', 'blocked_right', 'blocked_north', 'blocked_south', 'mlo', 'mno', 'mro', 'mso', 'case'] 
  f = open(dir_name+scen + out_name, 'w')
  obj = TreeLearnerProbabilistic(dir_name+scen + in_name_det, dir_name+scen + in_name_noisy, 'SWI-prolog', first_feat, RND_VARIABLE, int(num_exmp))
  obj.learnRules()
  obj1 = TranslateToDC()
  obj1.setRandomVariablePredicates(RND_VARIABLE)
  for rule in obj.rules:
    f.write(obj1.translate(rule) + '\n')
  f.close()

if __name__ == '__main__':
  if sys.argv[1] == 'false':
    learn_rules_det(sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5],sys.argv[6])
  elif sys.argv[1] == 'true':
    learn_rules_noisy(sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5],sys.argv[6],sys.argv[7])
  #learn_rules_det('../data/dc_input.pl','../data/dc_output.pl')
  
'''
1D: forula
  Flat:  
    simple: 'atleastOne'
    constrained: ''
  Hier:
    simple: ''
    constrained: ''
2D:
  Flat: 10
    simple: 'shape' yes
    constrained: heavy?
  Hier: 
    simple: ''
    constrained: heavy?
'''  
  
'''        
if __name__ == '__main__':
    directory = 'final_hier'
    outputFile = '../data/'+directory+'/worlds_dc_rules.pl'
    inputFile = '../data/' + directory +'/worlds_facts.pl'
    #inputFileDC =  '../data/' + directory +'/worlds_facts_noisy.pl'
    f = open('../data/dc_output.pl', 'w')
    obj = TreeLearnerProbabilistic('../data/z_nitesh_example/Financial.pl', '../data/z_nitesh_example/FinancialDC.pl', 'SWI-prolog', '', ['hasLoan', 'clientDistrict', 'hasAccount', 'clientLoan', 'loanAmount', 'loanStatus','monthlyPayments', 'gender', 'age', 'freq', 'avgSalary', 'ratUrbInhab', 'avgNrWith', 'avgSumOfInc', 'avgSumOfW', 'stdMonthInc', 'stdMonthW'])
    
    RND_VARIABLE = ['posX_t0', 'posX_t1', 'posY_t0', 'posY_t1', 'shape', 'move_left_of','displ']'good_cube', 'good_sphere', 'left_of_t0', 'left_of_t1', 'right_of_t0', 'right_of_t1', 'shape'], 'posX_l_t1', 'posX_r_t1', 'posY_l_t1', 'posY_r_t1']
    obj = TreeLearnerProbabilistic('../data/dc_input.pl', '', 'SWI-prolog', '', RND_VARIABLE)
    obj.learnRules()
    obj1 = TranslateToDC()
    obj1.setRandomVariablePredicates(RND_VARIABLE)
    for rule in obj.rules:
        rule = obj1.translate(rule)
        f.write(rule + '\n')
    f.close()'''


    
