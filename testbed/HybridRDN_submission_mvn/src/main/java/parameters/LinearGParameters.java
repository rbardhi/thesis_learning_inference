package hybrid.parameters;

import hybrid.cpds.WrongParameterNumber;
import hybrid.dependencies.Dependency;
import hybrid.dependencies.MarkovBlanket;
import hybrid.features.Feature;
import hybrid.network.Value;
import hybrid.network.WrongValueType;

import java.util.HashMap;

/**
 * Parameters for linear gaussian. 
 * @author irma
 *
 */

public class LinearGParameters extends Parameters {
	
	private LinearGaussianCoeff reg_coeff;
	
	/**
	 * Create empty linear gaussian parameters
	 * @param dep
	 */
	public LinearGParameters(Dependency dep) {
		this.dep=dep;
		
	}
	
	/**
	 * Create linear gaussian parameters 
	 * @param dep TODO
	 * @param parameters
	 * @param std
	 */
	public LinearGParameters(Dependency dep, Regression parameters, Double std){
		this.dep=dep;
		this.reg_coeff=new LinearGaussianCoeff(parameters, std);	
	}
	
	

	@Override
	public String toString() {
		return this.reg_coeff.toString();
	}


	public LinearGaussianCoeff getPars() {
		return reg_coeff;
	}


	public void setPars(LinearGaussianCoeff pars) {
		this.reg_coeff = pars;
	}

	
	@Override
	public int getNumberOfFreeParameters() {
		return 1+reg_coeff.getReg_coeff().get_nr_pars();
	}
	
	
	
}
