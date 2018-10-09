package hybrid.network;
/**
 * Range of values
 * @author irma
 *
 */
public abstract class Range {

	public abstract boolean isInRange(Value val) throws WrongValueType;
	public abstract void addValueToRange(Value val) throws WrongValueType;
	
	
	public int getIndexOfValue(Value value) {
		// TODO Auto-generated method stub
		return 0;
	}
	
}
