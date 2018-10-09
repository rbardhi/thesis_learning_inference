package hybrid.comparators;

import hybrid.network.Value;
import hybrid.network.WrongValueType;

/**
 * This comparator is comparing if a value given is bigger than some threshold.
 * E.g., new Bigger(20) gives true for compare(15.0) 
 * @author irma
 *
 */
public class Bigger implements Comparator {
    
	private Double threshold;
	
	public Bigger(Double threshold1) {
		this.threshold=threshold1;
	}

	@Override
	public boolean compare(Value v1) {
		try {
			if(v1.toNumber()>this.threshold){
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
	
	public String toString(){
		return ">"+this.threshold;
	}


	public void setupperbound(Double t1) {
		this.threshold=t1;
	}



}
