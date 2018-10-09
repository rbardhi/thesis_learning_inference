package hybrid.comparators;

import hybrid.network.Value;
import hybrid.network.WrongValueType;

/**
 * This comparator checks if a value is in between two thresholds.
 * @author irma
 *
 */
public class InBetween implements Comparator {
    
	private Double threshold1;
	private Double threshold2;
	
	public InBetween(Double threshold1,Double threshold2) {
       this.threshold1=threshold1;
       this.threshold2=threshold2;
	}

	@Override
	public boolean compare(Value v1) {
		try {
			if(v1.toNumber()>this.threshold1 && v1.toNumber()<this.threshold2){
				return true;
			}
			else{
				return false;
			}
		} catch (WrongValueType e) {
			e.printStackTrace();
		}
		return false;
	}
	
	public void setThresholds(Double t1,Double t2){
		this.threshold1=t1;
		this.threshold2=t2;
	}
	
	public String toString(){
		return this.threshold1+ "<"+"X<"+this.threshold2;
	}

	
	

}
