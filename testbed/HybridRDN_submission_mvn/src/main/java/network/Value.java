package hybrid.network;


/**
 * Representing a value a ground atom can have
 * @author irma
 *
 */
public abstract class Value {

public abstract boolean isnumeric();
public abstract boolean isBoolean();
public abstract boolean isDiscrete();
public abstract Double toNumber() throws WrongValueType;
	
	
	
}
