package hybrid.operators;

import hybrid.network.NumberValue;
import hybrid.network.Value;
import hybrid.network.WrongValueType;

/**
 * Multiply two numerical values
 * @author irma
 *
 */

public class Multiplication extends Operator<NumberValue> {

	public Multiplication() {
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public Value calculate(NumberValue val1, NumberValue val2) {
		try {
			return new NumberValue(val1.toNumber()*val2.toNumber());
		} catch (WrongValueType e) {
			e.printStackTrace();
		}
		return null;
	}
	
	@Override
	public Double calculate_numbers(Double val1,Double val2) {
			return val1*val2;

	}
	
	public String toString(){
		return "*";
	}

}
