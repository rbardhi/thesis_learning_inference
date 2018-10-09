package hybrid.features;

import hybrid.featureGenerator.AbstractConjunction;
import hybrid.featureGenerator.Standard_Conjunction;
import hybrid.network.Atom;
import hybrid.network.Literal;
import hybrid.network.Logvar;
import hybrid.network.Range;
import hybrid.network.Value;
import hybrid.queryMachine.ArrayFeatureValues;
import hybrid.queryMachine.QueryDispatcher;

import java.util.List;
import java.util.Set;

import alice.tuprolog.NoSolutionException;
/**
 * This class represents a value feature. It just represents a function over exactly one element.
 * 
 * @author irma
 * @param <T>
 *
 */
public  class ValueFt extends Feature<ArrayFeatureValues> {
	
	private Standard_Conjunction c;
	
	public ValueFt(Standard_Conjunction c){
		super(c);
		this.c=c;
	}


	@Override
	public boolean isDiscreteInput() {
		if(this.c.getNon_boolean_literal().getAtom().getPredicate().isDiscrete()){
			return true;
		}
	    return false;
	}

	@Override
	public boolean isContinuousInput() {
		if(this.c.getNon_boolean_literal().getAtom().getPredicate().isDiscrete()){
			return false;
		}
	    return true;
	}

	@Override
	public boolean isContinuousOutput() {
		if(this.c.getNon_boolean_literal().getAtom().getPredicate().isDiscrete()){
			return false;
		}
	    return true;
	}

	@Override
	public boolean isDiscreteOutput() {

		if(this.c.getNon_boolean_literal()==null && this.c.getBooleanAtoms().size()!=0){
			//it is a boolean feature hence discrete
			return true;
		}
		if(this.c.getNon_boolean_literal().getAtom().getPredicate().isDiscrete()){
			return true;
		}
	    return false;
	}

	public String toString(){
		return super.toString()+"Value "+this.c.toString();
	}



	@Override
	public Range getRange() {
		return this.c.getNon_boolean_literal().getAtom().getPredicate().getRange();
	}



	@Override
	public String getFeatureIdentifier() {
		String tmp="Val";
		for(Literal a:((List<Literal>)this.c.getLiteralList())){
			tmp+=a;
		}
		return tmp+=this.hashCode();
	}
	
	public String getFeatureIdentifier_weka() {
		String tmp="Val";
		for(Literal a:((List<Literal>)this.c.getLiteralList())){
			tmp+=a.getAtom().getPredicate().getPredicateName()+a.getLogvarsDashDelimited()+"_";
		}
		return tmp+=String.valueOf(this.hashCode()).replace("-", "");
	}

	@Override
	public boolean isComplex() {
		return false;
	}

	@Override
	public boolean is_with_operator() {
		return false;
	}

	@Override
	public Value processValue(ArrayFeatureValues featureValue) {
		return featureValue.getValues().get(0);
	}


	@Override
	public boolean isDeterministic() {
		return true;
	}


	@Override
	public Value dispatch(QueryDispatcher queryDisp) {
		return queryDisp.getValue(this);
	}



}
