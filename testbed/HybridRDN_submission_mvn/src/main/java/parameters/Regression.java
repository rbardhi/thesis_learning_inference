package hybrid.parameters;

import java.util.HashMap;
import java.util.List;

import hybrid.dependencies.Dependency;
import hybrid.features.Feature;
/**
 * The class representing parameters for a sequence of features F1,F2,...Fn of the form
 * intercept+F1*weight_1+...+Fn*weight_n
 * @author irma
 *
 */
public class Regression {

	protected Double intercept;
	protected HashMap<Feature,Double> weights;
	
	
	public Regression(List<Feature> features) {
		weights=new HashMap<Feature, Double>();
		for(Feature ft:features){
			weights.put(ft, new Double(0.0));
		}
	}

	
	/**
	 * Creating Regression with weights and intercept
	 * @param intercept - intercept of the regressors
	 * @param fts - sequence of features
	 * @param reg - sequence of regressors (corresponding to the order of features)
	 */
	public Regression(Dependency dep,Double intercept,Double[] reg) {
		this.intercept=intercept;
		weights=new HashMap<Feature, Double>();
		int i=0;
		for(Feature f:dep.getContinuousFeatures()){
			weights.put(f,reg[i++]);
		}
	}
	
	/**
	 * Creating Regression with weights and intercept
	 * @param intercept - intercept of the regressors
	 * @param fts - sequence of features
	 * @param reg - sequence of regressors (corresponding to the order of features)
	 */
	public Regression(Double intercept, List<Feature> fts, Double[] reg) {
		this.intercept=intercept;
		weights=new HashMap<Feature, Double>();
		int i=0;
		for(Feature f:fts){
			weights.put(f,reg[i++]);
		}
	}
	
	/**
	 * Creating Regression with weights and intercept
	 * @param intercept - intercept of the regressors
	 * @param fts - sequence of features
	 * @param reg - sequence of regressors (corresponding to the order of features)
	 */
	public Regression(Double intercept, HashMap<Feature,Double> param) {
		this.intercept=intercept;
		weights=new HashMap<Feature, Double>();
		int i=0;
		for(Feature f:param.keySet()){
			weights.put(f,param.get(f));
		}
	}

	
	public Double getIntercept() {
		return intercept;
	}

	public void setIntercept(Double intercept) {
		this.intercept = intercept;
	}

	public HashMap<Feature, Double> getWeights() {
		return weights;
	}

	public void setWeights(HashMap<Feature, Double> weights) {
		this.weights = weights;
	}

	@Override
	public String toString() {
		String tmp=intercept+"+";
		for(Feature f:weights.keySet()){
			tmp+=f+"*"+weights.get(f);
		}
		return tmp+="\n";
	}
	
	/**
	 * Get number of weights (coefficients) for this Regression parameters
	 * @return
	 */
	public int get_nr_pars(){
		return this.weights.size()+1;
	}


	public boolean isNthRegressor() {
		return false;
		
	}
	
	

}
