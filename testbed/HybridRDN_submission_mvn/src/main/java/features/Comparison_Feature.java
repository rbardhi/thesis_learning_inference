package hybrid.features;

import alice.tuprolog.NoSolutionException;
import hybrid.comparators.Comparator;
import hybrid.featureGenerator.ComplexConjunction;
import hybrid.featureGenerator.Standard_Conjunction;
import hybrid.network.BoolValue;
import hybrid.network.Range;
import hybrid.network.RangeDiscrete;
import hybrid.network.Value;
import hybrid.queryMachine.ArrayFeatureValues;
import hybrid.queryMachine.QueryDispatcher;

public class Comparison_Feature extends Feature<ArrayFeatureValues> {

	
	private Standard_Conjunction conjunction;
	private Feature non_deterministic_feature;
	private Comparator comparator;
	
	public Comparison_Feature(Standard_Conjunction c,Comparator comparator,Feature processing_feature) {
		super(c);
		this.conjunction=c;
		this.non_deterministic_feature=processing_feature;
		this.comparator=comparator;
	}
	
	@Override
	public boolean isDiscreteInput() {
		return false;
	}

	@Override
	public boolean isContinuousInput() {
		return true;
	}

	@Override
	public boolean isContinuousOutput() {
		return false;
	}

	@Override
	public boolean isDiscreteOutput() {
		return true;
	}

	@Override
	public boolean isComplex() {
		return true;
	}

	@Override
	public boolean isDeterministic() {
		return false;
	}

	@Override
	public boolean is_with_operator() {
		return false;
	}

	@Override
	public Range getRange() {
		return new RangeDiscrete(new String[]{"true","false"});
	}

	@Override
	public String getFeatureIdentifier() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Value processValue(ArrayFeatureValues featureValue) {
		Value value=non_deterministic_feature.processValue(featureValue);
		   if(this.comparator==null){
			   return value;
		   }
		   else{
			   return new BoolValue(this.comparator.compare(value));
		   }
	}
	
	public String toString(){
		return this.non_deterministic_feature+" { "+this.conjunction+"}"+this.comparator;
	}
    
	
	

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result
				+ ((comparator == null) ? 0 : comparator.hashCode());
		result = prime * result
				+ ((conjunction == null) ? 0 : conjunction.hashCode());
		result = prime
				* result
				+ ((non_deterministic_feature == null) ? 0
						: non_deterministic_feature.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		Comparison_Feature other = (Comparison_Feature) obj;
		if (comparator == null) {
			if (other.comparator != null)
				return false;
		} else if (!comparator.equals(other.comparator))
			return false;
		if (conjunction == null) {
			if (other.conjunction != null)
				return false;
		} else if (!conjunction.equals(other.conjunction))
			return false;
		if (non_deterministic_feature == null) {
			if (other.non_deterministic_feature != null)
				return false;
		} else if (!non_deterministic_feature
				.equals(other.non_deterministic_feature))
			return false;
		return true;
	}

	@Override
	public Value dispatch(QueryDispatcher queryDisp)  {
		return queryDisp.getValue(this);
	}

}
