

import hybrid.dependencies.Dependency;
import hybrid.experimenter.AlgorithmParameters;
import hybrid.experimenter.ParseArguments;
import hybrid.featureGenerator.DistributeFeaturesInBlocks;
import hybrid.featureGenerator.FeatureBlock;
import hybrid.featureGenerator.FeatureGeneratorNoRestrictions;
import hybrid.features.Feature;
import hybrid.interpretations.Data;
import hybrid.interpretations.DiscretizedRangeExtractor;
import hybrid.interpretations.NoCycles;
import hybrid.interpretations.TuPrologDataLoader;
import hybrid.interpretations.TuPrologInterpretationCreator_NoSubsampling;
import hybrid.interpretations.TuPrologInterpretationCreator_Subsampling;
import hybrid.network.Atom;
import hybrid.network.BooleanPred;
import hybrid.network.CategoricalPred;
import hybrid.network.DiscretizedPredicate;
import hybrid.network.GaussianPred;
import hybrid.network.Logvar;
import hybrid.network.MinMaxValue;
import hybrid.network.NetworkInfo;
import hybrid.network.Predicate;
import hybrid.network.Type;
import hybrid.penalties.BIC_score;
import hybrid.penalties.MDLPenalty;
import hybrid.penalties.NoPenalty;
import hybrid.queryMachine.TuPrologQueryMachine;
import hybrid.querydata.QueryData;
import hybrid.structure_learning.GreedySearch;
import hybrid.structure_learning.StructureLearner;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

public class LearnUniversityHybrid {
private static  NetworkInfo ntw;
	
	public static void main(String[] args) throws Exception{
		AlgorithmParameters parameters=new AlgorithmParameters();
        ParseArguments getAlgorithmParameters=new ParseArguments(" Boolean subsampling fixed - full feature space for all - FIXED PROBLEM WITH LOADING INTERPRETATIONS - added features to blocks functionality");
        getAlgorithmParameters.parseArgumentsHRDN(args);
        
        System.out.println("Parameters: "+parameters);
	    	
        UniversityHybrid hybrid_university=new UniversityHybrid();
		ntw=hybrid_university.defineApplicationNetwork(1);
			
		//LOAD DATA
		TuPrologDataLoader dataLoader=new TuPrologDataLoader(new TuPrologInterpretationCreator_Subsampling(2));
		TuPrologDataLoader dataLoader_no_subsampling=new TuPrologDataLoader(new TuPrologInterpretationCreator_NoSubsampling());
	 
		Data d_training=dataLoader.loadData(parameters.input_path+"/train/","interp", "pl",ntw);
	    Data d_validation=dataLoader.loadData(parameters.input_path+"/validate/", "interp", "pl",ntw);
	    Data d_test=dataLoader_no_subsampling.loadData(parameters.input_path+"/test/","interp", "pl",ntw);
	    
	    
	    //query machines
	    TuPrologQueryMachine training_data_machine=new TuPrologQueryMachine(d_training, AlgorithmParameters.getPenaltyType());
	    TuPrologQueryMachine validation_machine=new TuPrologQueryMachine(d_validation, AlgorithmParameters.getPenaltyType());
	    TuPrologQueryMachine test_machine=new TuPrologQueryMachine(d_test, AlgorithmParameters.getPenaltyType());
	    FeatureGeneratorNoRestrictions fGen=new FeatureGeneratorNoRestrictions(AlgorithmParameters.feature_length, AlgorithmParameters.nr_logvar_renamings);
		List<Feature> fts=fGen.generateFeatures(ntw.getPredicateNameToAtom().get("intelligence"),ntw.getAtomsAndEqualityConstraints());
		/*for(Feature f:fts){
			System.out.println(f);
		}*/
		/*Dependency dep=new Dependency(ntw.getPredicateNameToAtom().get("intelligence"),new Feature[]{fts.get(6),fts.get(0)});
		QueryData qd=training_data_machine.getQueryResults(dep);
	    dep.getCpd().getCpdEvaluator().estimateParameters(qd);
		QueryData qd_val=validation_machine.getQueryResults(dep);
		double pll=dep.getCpd().getCpdEvaluator().calculatePLL(qd_val, dep.getCpd().getParameters(), new BIC_score());
	    System.out.println(pll);*/
		 StructureLearner str_learner=new StructureLearner(fGen,new GreedySearch(),ntw,training_data_machine,validation_machine,test_machine);	

        str_learner.learnStructureAndEvaluate(ntw.getLiterals().toArray(new Atom[ntw.getLiterals().size()]));
	
	}
}
