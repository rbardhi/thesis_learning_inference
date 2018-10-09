package hybrid.parameters;

import hybrid.cpdEvaluation.CGEvaluator;
import hybrid.dependencies.Dependency;
import hybrid.dependencies.MarkovBlanket;
import hybrid.features.Feature;
import hybrid.network.RangeDiscrete;
import hybrid.network.StringValue;
import hybrid.network.Value;
import hybrid.utils.CartesianProduct;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Class representing CG parameters
 * @author irma
 *
 */
public class CGParameters extends Parameters {

	private HashMap<AssignmentKey,Gaussian> parameters;
	private Gaussian marginal;
	
	//Creating empty parameters
	public CGParameters(Dependency dep) {
		this.parameters=new HashMap<AssignmentKey, Gaussian>();
		this.marginal=new Gaussian();
	}

	/**
	 * Create CG parameter for dependency dep by having a separate Gaussian distribution over head atom for each assignments of discrete 
	 * parents and, finally, a Gaussian marginal distribution for the head.
	 * @param dep
	 * @param g
	 * @param marginal_distr
	 */
	public CGParameters(Dependency dep, HashMap<AssignmentKey, Gaussian> g,Gaussian marginal_distr) {
		this.dep=dep;
		this.parameters=g;
		this.marginal=marginal_distr;
	}

	public void addConditionalParameter(AssignmentKey key,Gaussian par) {
		this.parameters.put(key, par);
		
	}

	@Override
	public String toString() {
		String tmp=" ";
		for(AssignmentKey key:this.parameters.keySet()){
			tmp+="KEY: "+key+ " --> "+"N("+this.parameters.get(key).getMean()+","+this.parameters.get(key).getStd()+")\n";
		}
		return tmp;
	}

	
	/**
	 * Get parameters for dependency dep, parent
	 * @param dep
	 * @param parentValues
	 * @param par
	 * @return
	 */
	public Gaussian getParameters(Dependency dep,HashMap<Feature, Value> parentValues, CGParameters par, CGEvaluator cgEval) {
		return this.parameters.get(cgEval.extractAssignmentKey(dep, parentValues));
		
	}
	
	/**
	 * Get marginal probability for the head atom
	 * @return
	 */
	public Gaussian getMarginalProb(){
		return this.marginal;
	}

	public Gaussian getParameters(AssignmentKey key){
		return this.parameters.get(key);
	}

	public HashMap<AssignmentKey, Gaussian> getParameters() {
		return parameters;
	}

	public Gaussian getMarginal() {
		return marginal;
	}

	public int getNumberOfFreeParameters() {
		int tmp=0;
		Iterator entries = this.parameters.entrySet().iterator();
		while (entries.hasNext()) {
			Map.Entry entry = (Map.Entry) entries.next();
			tmp+=((Gaussian)entry.getValue()).getNumberOfFreeParameters();
		}
		return tmp;
	}
	


	
	
	
	
	
}
