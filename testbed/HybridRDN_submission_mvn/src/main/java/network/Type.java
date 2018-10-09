package hybrid.network;

/**
 * class Type represents a certain type of objects in the domain. E.g, student, course etc.
 * @author irma
 *
 */
public class Type extends TermRDN{

	private String name;
	
	/**
	 * create type with a specific name
	 * @param symbol
	 */
	public Type(String symbol){
		this.name=symbol;
	}

	public String getName() {
		return name;
	}
	
	public String toString(){
		return name;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Type other = (Type) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}


	@Override
	public String createFOLTerm() {
		return this.name;
	}



	
	
	
}
