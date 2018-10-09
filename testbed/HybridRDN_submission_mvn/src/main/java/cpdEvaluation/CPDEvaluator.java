package hybrid.cpdEvaluation;

import java.util.HashMap;
import java.util.Random;

import hybrid.cpds.CPD;
import hybrid.cpds.ProbabilityMassFunction;
import hybrid.dependencies.MarkovBlanket;
import hybrid.network.GroundAtom;
import hybrid.network.Value;
import hybrid.parameters.Parameters;
import hybrid.penalties.Penalty;
import hybrid.queryMachine.QueryMachine;
import hybrid.querydata.QueryData;

/**
 * Interface representing cpd evaluators for parameter estimation, probability retrieval for
 * Markov blanket assignments and so on...
 * @author irma
 *
 * @param <P> - the Parameters of a CPD
 */

public interface CPDEvaluator<P extends Parameters> {
	
	P estimateParameters(QueryData trainingData);
	Double getProbability(MarkovBlanket mB, P par);
	double calculatePLL(QueryData queryResults, P pars,Penalty pen);
	Value getPrediction(MarkovBlanket mB, P parameters);
	Value getPrediction(MarkovBlanket mB, P parameters,Random ran);
	Value getPrediction_no_noise(MarkovBlanket mB, P parameters);
	Double getProbability(Value val,MarkovBlanket mB, P par);
	
}
