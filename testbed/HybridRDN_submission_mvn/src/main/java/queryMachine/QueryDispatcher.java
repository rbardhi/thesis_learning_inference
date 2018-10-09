package hybrid.queryMachine;

import hybrid.features.Average;
import hybrid.features.Comparison_Feature;
import hybrid.features.ContinuousOutputAggregate;
import hybrid.features.DiscreteInputContinuousOutput;
import hybrid.features.DiscretizedProportion;
import hybrid.features.Exist;
import hybrid.features.Max;
import hybrid.features.Min;
import hybrid.features.Mode;
import hybrid.features.Operator_Feature;
import hybrid.features.Proportion;
import hybrid.features.ValueFt;
import hybrid.interpretations.Interpretation;
import hybrid.network.GroundAtom;
import hybrid.network.Value;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.Prolog;

public interface QueryDispatcher {
	Value getValue(DiscreteInputContinuousOutput ft);
	Value getValue(Comparison_Feature ft);
	Value getValue(Mode ft);
	Value getValue(ValueFt ft);
	Value getValue(ContinuousOutputAggregate ft) ;
	Value getValue(Operator_Feature operator_Feature);
	
	
}
