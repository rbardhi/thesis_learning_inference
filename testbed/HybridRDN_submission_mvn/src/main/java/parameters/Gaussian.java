package hybrid.parameters;

import hybrid.dependencies.Dependency;
import hybrid.dependencies.MarkovBlanket;
import hybrid.network.NumberValue;

/**
 * The class represents the Gaussian parameters
 * mean and standard deviation
 * @author irma
 *
 */

public class Gaussian extends Parameters {

	private double mean;
	private double std;
	
	/**
	 * Create a gaussian parameter with specific mean and standard deviation
	 * @param mean
	 * @param std
	 */
	public Gaussian(double mean,double std){
		this.mean=mean;
		this.std=std;
	}
	
	/**
	 * Create a standardized Gaussian with mean=0 and standard deviation=1
	 */
	public Gaussian() {
		this.mean=0;
		this.std=1;
	}

	/**
	 * Get mean of this Gaussian distribution
	 * @return
	 */
	public double getMean() {
		return mean;
	}
	/**
	 * Set mean for this Gaussian distribution
	 * @param mean
	 */
	public void setMean(double mean) {
		this.mean = mean;
	}
	/**
	 * Get standard deviation for this Gaussian distribution
	 * @return
	 */
	public double getStd() {
		return std;
	}
	/**
	 * Set standard deviation for this Gaussian distribution
	 * @param std
	 */
	public void setStd(double std) {
		this.std = std;
	}
	
	@Override
	public String toString() {
		return "Mean: "+mean+ " STD: "+std;
		}

	@Override
	public int getNumberOfFreeParameters() {
		return 2;
	}
	
	
	
	
}
