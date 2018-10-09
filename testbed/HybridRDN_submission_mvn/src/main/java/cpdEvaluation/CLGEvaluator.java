package hybrid.cpdEvaluation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Random;

import hybrid.dependencies.Dependency;
import hybrid.dependencies.MarkovBlanket;
import hybrid.features.Feature;
import hybrid.interpretations.Interpretation;
import hybrid.network.RangeDiscrete;
import hybrid.network.UndefinedValue;
import hybrid.network.Value;
import hybrid.parameters.AssignmentKey;
import hybrid.parameters.CLGParameters;
import hybrid.parameters.FeatureValuePair;
import hybrid.parameters.LinearGParameters;
import hybrid.parameters.Parameters;
import hybrid.parameters.UndefinedAssignmentKey;
import hybrid.penalties.Penalty;
import hybrid.queryMachine.QueryMachine;
import hybrid.querydata.QueryData;
import hybrid.utils.Logarithm2;

/**
 * This class is responsible for estimating parameters and evaluating conditional
 * linear Gaussian distribution. 
 * @author irma
 *
 */

public class CLGEvaluator extends ConditionalGaussians implements ContinuousEval<CLGParameters> {

	
	/**
	 * Estimate CLG parameters given training data
	 */
	@Override
	public CLGParameters estimateParameters(QueryData trainingData) {
		List<AssignmentKey> assignmentKeys=this.generateAllKeys(trainingData.getDep());
		List<MarkovBlanket> marginalValues=new ArrayList<MarkovBlanket>(); 
		HashMap<AssignmentKey, List<MarkovBlanket>> filteredData=filterData(assignmentKeys,trainingData);
		HashMap<AssignmentKey,LinearGParameters> pars=new HashMap<AssignmentKey, LinearGParameters>();
		LinearGaussianEvaluator lGeval=new LinearGaussianEvaluator();
		for(AssignmentKey k:filteredData.keySet()){
			try {
				try {
					pars.put(k, (LinearGParameters) lGeval.estimateParameters(filteredData.get(k), trainingData.getDep()));
				} catch (NotEnoughDataForLinearRegression e) {
					//not enough data to estimate the parameters
					//for now I put null for this Linear Gaussian
					pars.put(k, null);
				}
			//    System.out.println("--------------------------------------------------------------------");
			} catch (BadParentSpecification e) {
				e.printStackTrace();
			}
		}
		CLGParameters return_pars=new  CLGParameters(trainingData.getDep(), pars, estimateGaussianStatisticsForHeadValues(marginalValues));
	    trainingData.getDep().getCpd().setParameters(return_pars);
	    return return_pars;
	}
	
	@Override
	public Double getProbability(MarkovBlanket mB, CLGParameters par) {
		AssignmentKey key=this.extractAssignmentKey(mB.getDep(), mB.getFeatureValues());
		LinearGaussianEvaluator lgEval=new LinearGaussianEvaluator();
		//if no parameters estimated for this combination - do Naive Bayes?
		if(par.getParameters(key)==null){
			return Double.MIN_VALUE;
		}
		return lgEval.getProbability(mB, par.getParameters(key));
	}
	
	@Override
	public double calculatePLL(QueryData testData, CLGParameters pars,Penalty pen) {
		double sumProb=0;
		int nr_data_points=0;
		for(Interpretation i:testData.getQuery_results().keySet()){
			for(MarkovBlanket mB:testData.getQuery_results().get(i)){	
				sumProb+=Logarithm2.logarithm2(this.getProbability(mB, pars));
				nr_data_points++;
			}
		}
		return sumProb-pen.calculatePenalty(testData.getDep(), nr_data_points);
	}


	/**
	 * Generate all possible keys for discrete features
	 * @param dep
	 * @return
	 */
	protected List<AssignmentKey> generateAllKeys(Dependency dep) {
		List<AssignmentKey> tmp=new ArrayList<AssignmentKey>();
		List<List<FeatureValuePair>> featureValuePairs= getAllFeatureValuePairs(dep);
		List<List<FeatureValuePair>> cartProd=getCartesianProductsOfFeatureValues(featureValuePairs);
		for(List<FeatureValuePair> f:cartProd){
			tmp.add(new AssignmentKey(f));
		}
		return tmp;
	}
	
	/**
	 * Get all possible feature-value pairs for discrete features in the dependency
	 */
	protected List<List<FeatureValuePair>> getAllFeatureValuePairs(Dependency dep){
		List<List<FeatureValuePair>> featureValuePairs=new ArrayList<List<FeatureValuePair>>();
		for(Feature ft:dep.getDiscreteFeatures()){
			List<FeatureValuePair> tmp=new ArrayList<FeatureValuePair>();
			for(Value val:((RangeDiscrete)ft.getRange()).getValues()){
				tmp.add(new FeatureValuePair(ft,val));
				
			}
			featureValuePairs.add(tmp);
		}
		return featureValuePairs;
	}
	
	
	/**
	 * Filter data and assign head values to each combination of discrete parents. We also obtain 
	 * arraylist of head values. 
	 * @param marginalValues - array of marginal values filled with this 
	 * @param trainingData
	 * @return
	 */
	protected HashMap<AssignmentKey, List<MarkovBlanket>> filterData(List<AssignmentKey> assignmentKeys,QueryData trainingData) {
		HashMap<AssignmentKey, List<MarkovBlanket>> tmp=new HashMap<AssignmentKey, List<MarkovBlanket>>();
		for(AssignmentKey k:assignmentKeys){
			tmp.put(k, new ArrayList<MarkovBlanket>());
		}
		tmp.put(new UndefinedAssignmentKey(), new ArrayList<MarkovBlanket>());
		
		for(Interpretation i:trainingData.getQuery_results().keySet()){
			for(MarkovBlanket mB:trainingData.getQuery_results().get(i)){	
				AssignmentKey key=extractAssignmentKey(trainingData.getDep(),mB.getFeatureValues());
                tmp.get(key).add(mB);
			}
		}
		return tmp;
	}


	/**
	 * Extract Assignment Key for discrete parents
	 * @param dep
	 * @param featureValues
	 * @return
	 */
	public AssignmentKey extractAssignmentKey(Dependency dep,HashMap<Feature, Value> featureValues) {
		AssignmentKey a=null;
		List<FeatureValuePair> tmp=new ArrayList<FeatureValuePair>();
		//for creating the key of CLG we use only discrete features in the order specified when the dependency was created!
		for(Feature f:dep.getDiscreteFeatures()){
			if(featureValues.get(f) instanceof UndefinedValue){
				return new UndefinedAssignmentKey();
			}
			tmp.add(new FeatureValuePair(f,featureValues.get(f)));
		}
		return new AssignmentKey(tmp);
	}

	@Override
	public Value getPrediction(MarkovBlanket mB, CLGParameters parameters) {
		AssignmentKey key=this.extractAssignmentKey(mB.getDep(), mB.getFeatureValues());
		LinearGaussianEvaluator lgEval=new LinearGaussianEvaluator();
		return lgEval.getPrediction(mB, parameters.getParameters(key));
	}

	@Override
	public Double getProbability(Value val, MarkovBlanket mB, CLGParameters par) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Value getPrediction(MarkovBlanket mB, CLGParameters parameters,Random ran) {
		AssignmentKey key=this.extractAssignmentKey(mB.getDep(), mB.getFeatureValues());
		LinearGaussianEvaluator lgEval=new LinearGaussianEvaluator();
		return lgEval.getPrediction(mB, parameters.getParameters(key),ran);
	}

	@Override
	public Value getPrediction_no_noise(MarkovBlanket mB,CLGParameters parameters) {
		AssignmentKey key=this.extractAssignmentKey(mB.getDep(), mB.getFeatureValues());
		LinearGaussianEvaluator lgEval=new LinearGaussianEvaluator();
		return lgEval.getPrediction_no_noise(mB, parameters.getParameters(key));
	}
	
	

	
	
	
}
