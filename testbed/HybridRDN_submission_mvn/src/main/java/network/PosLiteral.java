package hybrid.network;

import java.util.ArrayList;
import java.util.List;
/**
 * Positive literal
 * @author irma
 *
 */
public class PosLiteral extends Literal {


	public PosLiteral(Atom a){
		super(a);
	}
	

	@Override
	public String createFOLTerm() {
		return this.a.createFOLTerm();
	}
	
	public String toString(){
		return this.a.toString();
	}

/**
 * Adding only positive literals
 */
	@Override
	public List<Literal> getLiterals(List<Atom> list) {
		List<Literal> posLiterals=new ArrayList<Literal>();
		for(Atom a:list){
			posLiterals.add(new PosLiteral(a));
		}
		return posLiterals;
	}
	
	

}
