package hybrid.parameters;

import hybrid.cpds.WrongParameterNumber;
import hybrid.dependencies.Dependency;
import hybrid.dependencies.MarkovBlanket;
import hybrid.features.Feature;
import hybrid.network.Atom;
import hybrid.network.RangeDiscrete;
import hybrid.network.Value;
import hybrid.network.WrongValueType;

import java.util.HashMap;

/**
 * This class represents parameters of logistic regression for a specific dependency.
 * Regressors map each value of the target predicate to specific 
 * regression coefficients and intercept for features in the parent set.
 * @author irma
 *
 */
public class LogRegregressors extends Parameters {

	private Dependency dep;
	private HashMap<Value,Regression> regressors;
	private Integer nr_free_pars;
	private Double averageTrainingAlpha=null;
	
	/**
	 * initializes regression parameters with zero weights
	 * @param dep
	 */
	public LogRegregressors(Dependency dep){
		this.dep=dep;
		regressors=new HashMap<Value, Regression>();
		for(Value v:((RangeDiscrete)dep.getHead().getPredicate().getRange()).getValues()){
			regressors.put(v, new Regression(dep.getFeatures()));
		}
	}


	@Override
	public String toString() {
		String tmp="";
		for(Value val:regressors.keySet()){
			tmp+=val+" ---> "+regressors.get(val);
		}
		return tmp;
	}


	/**
	 * Add a parameter for a specific value of the target predicate
	 * @param val - value from the range of a predicate
	 * @param parameters
	 * @throws WrongParameterNumber
	 * @throws WrongValueType
	 * @throws WrongValueSpecification
	 */
	public void addConditionalParameter(Value val, Regression parameters) throws WrongParameterNumber, WrongValueType, WrongValueSpecification {
		if(dep.getFeatures().size()!=parameters.getWeights().size()){
			throw new WrongParameterNumber("The number of features and regressors is not the same!");
		}
		if(!dep.getHead().hasInRange(val)){
			 throw new WrongValueSpecification("There is no: "+val+" in the range of "+dep.getHead());
		}
		for(int i=0;i<dep.getFeatures().size();i++){
			regressors.put(val, parameters);
		}
		
	}

    /**
     * Returns intercept value for target predicate's value
     * @param val
     * @return
     */
	public double getInterceptForValue(Value val){
		return this.regressors.get(val).getIntercept();
	}
	
	/**
	 * returns feature's ft coefficient for a target predicate's value val
	 * @param val
	 * @param ft
	 * @return
	 */
	public double getCoefficientForValueAndFeature(Value val,Feature ft){
		return this.regressors.get(val).getWeights().get(ft);
	}
	
	/**
	 * check if there are regression coefficient for a value 
	 * @param value
	 * @return
	 */
	public boolean hasValues(Value value) {
		if(this.regressors.containsKey(value)){
			return true;
		}
		else{
			return false;
		}
	}

	@Override
	public int getNumberOfFreeParameters() {
		if(this.nr_free_pars==null){
			int tmp=1;
			for(Value val:this.regressors.keySet()){
				if(!this.regressors.get(val).isNthRegressor()){
				tmp+=this.regressors.get(val).get_nr_pars();
				}
			}
			this.nr_free_pars=tmp;
		}
		return this.nr_free_pars;
	}

	/**
	 * get regression coefficients for a value
	 * @param val
	 * @return
	 */
   public Regression getRegressionCoefficients(Value val){
	   return regressors.get(val);
   }
   /**
    * This method checks whether there are coefficients specified for this value. If not, it means it is the nth parameter
    * (where n=|range(head_atom)|), and it needs not to be estimated.
    * @param val
    * @return
    */
   public boolean isNthCoefficient(Value val){
	   return this.regressors.get(val).isNthRegressor();
   }

/**
 * Add average alpha for this dependency (if the subsampling occured)
 * @param averageAlpha
 */
public void addAverageTrainingAlpha(Double averageAlpha) {
	this.averageTrainingAlpha=averageAlpha;
	
}

/**
 * get the average alpha from the training data
 * @return
 */
public Double getTraining_data_averaged_alphas() {
	return this.averageTrainingAlpha;
}
	
	
}
