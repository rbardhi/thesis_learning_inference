package hybrid.penalties;

import hybrid.dependencies.Dependency;
import hybrid.interpretations.Data;
import hybrid.parameters.Parameters;

public class MDLPenalty extends Penalty {

	/**
	 * Calculate MDL penalty
	 */
	@Override
	public double calculatePenalty(Dependency dep, long nr_data_points) {
		return (hybrid.utils.Logarithm2.logarithm2(nr_data_points)*dep.getCpd().getParameters().getNumberOfFreeParameters()*dep.getNumLiterals())/2;
	}


	public String toString(){
		return "MDL PENALTY as log2(nr_data_points)*number_of_free_parameters*number_of_literals_per_feature)/2";
	}

}
