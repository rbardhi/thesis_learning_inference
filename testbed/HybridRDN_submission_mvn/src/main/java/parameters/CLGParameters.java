package hybrid.parameters;

import hybrid.cpdEvaluation.CGEvaluator;
import hybrid.cpdEvaluation.CLGEvaluator;
import hybrid.dependencies.Dependency;
import hybrid.features.Feature;
import hybrid.network.Value;

import java.util.HashMap;

/**
 * Class representing CLG parameters
 * @author irma
 *
 */
public class CLGParameters extends Parameters {
	private HashMap<AssignmentKey,LinearGParameters> parameters;
	private Gaussian marginal;
	
	/**
	 * Create empty CLG parametera for dependency dep
	 * @param dep
	 * @param g
	 * @param marginal_distr
	 */
	public CLGParameters(Dependency dep) {
		this.dep=dep;
		this.parameters=new HashMap<AssignmentKey, LinearGParameters>();
	}
	

	/**
	 * Create CLG parameter for dependency dep by having a separate Linear Gaussian distribution over head atom for each assignments of discrete 
	 * parents and, finally, a Gaussian marginal distribution for the head.
	 * @param dep
	 * @param g
	 * @param marginal_distr
	 */
	public CLGParameters(Dependency dep, HashMap<AssignmentKey, LinearGParameters> g,Gaussian marginal_distr) {
		this.dep=dep;
		this.parameters=g;
		this.marginal=marginal_distr;
	}
	
	@Override
	public String toString() {
		String tmp=" ";
		for(AssignmentKey key:this.parameters.keySet()){
			try{
			tmp+="KEY: "+key+ " -->" + parameters.get(key).toString()+"\n";
			}
			catch(NullPointerException e){
				tmp+="KEY: "+key+ " -->" + "the parameters couldn't be estimated!\n";
			}
		}
		return tmp;
	}
	
	
	public void addParameter(AssignmentKey key,LinearGParameters lg){
		this.parameters.put(key, lg);
	}
	
	
	/**
	 * Get parameters for dependency dep, parent
	 * @param dep
	 * @param parentValues
	 * @param par
	 * @return
	 */
	public LinearGParameters getParameters(Dependency dep,HashMap<Feature, Value> parentValues, CLGEvaluator cgEval) {
		return this.parameters.get(cgEval.extractAssignmentKey(dep, parentValues));
		
	}
	
	public LinearGParameters getParameters(AssignmentKey key){
		return this.parameters.get(key);
	}


	@Override
	public int getNumberOfFreeParameters() {
		return this.parameters.size()*(this.dep.getContinuousFeatures().size()+2);
	}
	
}
