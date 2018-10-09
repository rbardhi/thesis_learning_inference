package hybrid.parameters;
import java.text.DecimalFormat;
import java.util.HashMap;

import hybrid.dependencies.Dependency;
import hybrid.dependencies.MarkovBlanket;
import hybrid.network.*;

/**
 * The class representing Probability Mass function which gives the probability that a discrete
 * obtains a single value
 * @author irma
 *
 */

public class PMF extends Parameters {

	private Dependency dep;
	private HashMap<Value,Double> parameters;
	private Double training_data_averaged_alphas;
	
	/**
	 * Initializing marginal parameters with probabilities given a head of a dependency (since the dependency doesn't have
	 * any parent set, we only care about the head value)
	 * @param head
	 * @throws BadProbabilityDistribution 
	 */
	public PMF(Atom head,HashMap<Value,Double> parameters,Double training_data_averaged_alpha) throws BadProbabilityDistribution {
		this.parameters=new HashMap<Value,Double>();
		this.training_data_averaged_alphas=training_data_averaged_alpha;
		double d=0;
		
		for(Value v:((RangeDiscrete)head.getPredicate().getRange()).getValues()){
			//System.out.println("Parameters: "+parameters+ " "+parameters.get(v));
			this.parameters.put(v, parameters.get(v));
			try{
			  d+=parameters.get(v);
			}
			catch(NullPointerException e){
				throw new BadProbabilityDistribution("No value: "+v+ " in the specified probability for "+head);
			}
		}
		DecimalFormat df = new DecimalFormat("#.00");
		System.out.println("SUM : "+d+ " "+ Double.valueOf(df.format(d)));
		if(d!=1.0 && !(d>=(1-0.00001))){
			throw new BadProbabilityDistribution(" The probabilities for " + head+ " CPT prior don't sum up to one!");
		}
	}

/**
 * Creating empty marginal parameters
 * @param dep
 */
	public PMF(Dependency dep) {
		this.dep=dep;
	}


	@Override
	public String toString() {
		String tmp=" ";
		for(Value val:parameters.keySet()){
			tmp+=val+ " ---> "+parameters.get(val)+"\n";
		}
		return tmp;
	}
	
	
	public HashMap<Value,Double> getParameters(){
		return this.parameters;
	}
	
	@Override
	public int getNumberOfFreeParameters() {
		return this.parameters.size();
	}

	public Double getTraining_data_averaged_alphas() {
		return training_data_averaged_alphas;
	}

	
	
	

}
