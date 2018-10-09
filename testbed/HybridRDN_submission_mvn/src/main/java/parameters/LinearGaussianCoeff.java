package hybrid.parameters;


/**
 * This class represents a coefficient of a linear Gaussian for a specific . It consists of two parts:
 * - Regression coefficients - which determine the linear combination of continuous parents representing the mean of the Gaussian
 * - std - representing the standard deviation
 * @author irma
 *
 */
public class LinearGaussianCoeff {

	private Regression reg_coeff;
	private double std;
	
	public LinearGaussianCoeff(Regression reg_coeff,double std){
		this.reg_coeff=reg_coeff;
		this.std=std;
	}

	public Regression getReg_coeff() {
		return reg_coeff;
	}

	public double getStd() {
		return std;
	}

	@Override
	public String toString() {
		return "LinearGaussianPar [reg_coeff=" + reg_coeff + ", std=" + std
				+ "]";
	}
	
	
	
	
}
