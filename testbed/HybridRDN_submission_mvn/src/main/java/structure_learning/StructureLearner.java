package hybrid.structure_learning;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import hybrid.dependencies.Dependency;
import hybrid.experimenter.AlgorithmParameters;
import hybrid.experimenter.CVIteration;
import hybrid.experimenter.CrossValidation;
import hybrid.featureGenerator.FeatureGeneratorNoRestrictions;
import hybrid.featureGenerator.FeatureGeneratorAbstract;
import hybrid.featureGenerator.SampleFeatureSpace;
import hybrid.features.Feature;
import hybrid.interpretations.Data;
import hybrid.network.Atom;
import hybrid.network.NetworkInfo;
import hybrid.penalties.NoPenalty;
import hybrid.queryMachine.ComputedFeatureSpace;
import hybrid.queryMachine.QueryMachine;
import hybrid.queryMachine.TuPrologQueryMachine;
import hybrid.querydata.QueryData;
import hybrid.utils.FileSearching;
import hybrid.utils.GenerateUniqueIDforAtom;

/**
 * This class is used to learn the structure for a network specified in NetworkInfo, with a specific query machine,
 * training data (used for parameter estimation), test_data (used for scoring the learned structure) and validation data (used to select
 * the structure).
 * @author irma
 *
 */
public class StructureLearner {

	//private HashMap<Atom,List<Feature>> additional_feautures=new HashMap<Atom,List<Feature>>();
	private FeatureGeneratorAbstract feature_generator;
	private StructureSearch structure_search;
	private NetworkInfo ntw;
	private QueryMachine query_training;
	private QueryMachine query_test;
	private QueryMachine query_validate;
	
	public StructureLearner(FeatureGeneratorAbstract f_generator,StructureSearch struct_search,NetworkInfo ntw,QueryMachine training,QueryMachine validation,QueryMachine test){
		this.feature_generator=f_generator;
		this.structure_search=struct_search;
		this.ntw=ntw;
		this.query_training=training;
		this.query_validate=validation;
		this.query_test=test;
		this.ntw.initializeRandvarTests();
	}

	//TBA: cross validation experiments
	public LearnedStructure learnStructureAndEvaluate_cross_validation(Atom[] learn_structure_for,List<CVIteration> cv_iterations) throws IOException, ResultAlreadyExistsException{		
		for(CVIteration cv:cv_iterations){
		    this.query_training=new TuPrologQueryMachine(new Data(cv.getTraining()), AlgorithmParameters.getPenaltyType());
		    this.query_validate=new TuPrologQueryMachine(new Data(cv.getValidation()), AlgorithmParameters.getPenaltyType());
		    this.query_test=new TuPrologQueryMachine(new Data(cv.getTest()), AlgorithmParameters.getPenaltyType());
		    LearnedStructure lndstr=learnStructureAndEvaluate(learn_structure_for);
		}
	     
	     return null;
	}
	/**
	 * General structure learnin method
	 * @param learn_structure_for - a list of literals we will learn the structure for
	 * @param ntw - network containing all the atoms
	 * @param query_machine_training - training query machine
	 * @param query_machine_validation - validation query machine
	 * @param query_machine_test - test query machine
	 * @param fgen - feature generator
	 * @param search - structure search method
	 * @return
	 * @throws IOException
	 * @throws ResultAlreadyExistsException
	 */
	public LearnedStructure learnStructureAndEvaluate(Atom[] learn_structure_for) throws IOException, ResultAlreadyExistsException{
		LearnedStructure learnedStructure=new LearnedStructure();
		List<String> particular_predicates=hybrid.experimenter.AlgorithmParameters.predicates; //load particular predicates from algorithm parameters (if set)
		hybrid.structure_learning.NetworkWPLLStat.setOutputFile(AlgorithmParameters.getOutput_path()+"/wpll_stat"); //set output file for all the wplls

		double total_time=0;
		double total_wpll = 0;
		HashMap<Atom,Boolean> result_exists=new HashMap<Atom, Boolean>();	

		//iterate through atoms
		for(Atom a:learn_structure_for){
			System.out.println(" Learning structure for: "+a);
			//check if the results exist for this atom
			if(exist_results(a,result_exists,total_wpll)){
				continue;
			}

			//check if the predicate of this atom is equal to the particular predicate we set (if the flag is set to particular predicate)
			if(particular_predicates!=null){
				if(!particular_predicates.contains(a.getPredicate().getPredicateName())){
					continue;
				}
			}
			//output some information: nr training randvars
			hybrid.loggers.DetailedStructureLearningForAtom.setOutFile(a,AlgorithmParameters.output_path+"/structure_learning_Log_");
			hybrid.loggers.DetailedStructureLearningForAtom.println("Nr Training randvars for target predicate "+this.query_training.getData().getNrGroundingsInData(a));
			hybrid.loggers.DetailedStructureLearningForAtom.println("Nr Training randvars (all) "+this.query_training.getData().getNrRandvars());
			//generate general feature space for atom a based on feature generator fgen
			List<Feature> feature_space=generateFeatureSpace(a,this.feature_generator,ntw);
			//ouput feature size
			hybrid.loggers.DetailedStructureLearningForAtom.println("FEATURE SPACE SIZE: "+feature_space.size());
			//learn dependency for this atom
			double cacheing_time=0;
			LearnedDependency learned=null;
			try {
				System.out.println(" Learn dependency for "+a);
				learned = learnDependency(a,this.query_training,this.query_validate,ntw,feature_space,this.structure_search);
				learnedStructure.addLearnedDependency(a, learned);
			} catch (ResultAlreadyExistsException e) {
				continue;

			} catch (ResultNotObtainedError e) {
				e.printStackTrace(hybrid.loggers.DetailedStructureLearningForAtom.getStream());
				continue;
			}
			//do some outputing
			report(learned,learnedStructure,a,cacheing_time);
			//Perform evaluation of the learned structure
			if(AlgorithmParameters.isEvaluation_flag()){
				evaluateLearnedStructure(a,learned,this.query_test);
			}

		}
		File total_wpll_file=new File(AlgorithmParameters.getOutput_path()+"/wpll_stat");
		FileWriter fW=new FileWriter(total_wpll_file);
		fW.append(String.valueOf(total_wpll));
		fW.close();
		return learnedStructure;
	}


	private void report(LearnedDependency learned, LearnedStructure learnedStructure, Atom a, double cacheing_time) {
		//output the learned dependency
		hybrid.loggers.DetailedStructureLearningForAtom.print("LEARNED: ");  
		//if no features learned output NONE
		if(learned.getDep().getFeatures().size()==0){
			hybrid.loggers.DetailedStructureLearningForAtom.print("NONE"+"\n");
		}
		for(Feature f:learned.getDep().getFeatures()){
			hybrid.loggers.DetailedStructureLearningForAtom.print(f.getIndex_in_feature_space()+"\t");
		}
		hybrid.loggers.DetailedStructureLearningForAtom.println("\nParameters: "+learnedStructure.getLearnedDependency().get(a).getDep().getCpd().getParameters().toString());
		learned.getStatistics().setLearningTime(learned.getStatistics().getLearningTime()+cacheing_time);
		hybrid.loggers.DetailedStructureLearningForAtom.close();
	}

	private boolean exist_results(Atom a, HashMap<Atom, Boolean> result_exists, double total_wpll) throws FileNotFoundException {
		FileSearching fS=new FileSearching();	
		if(!AlgorithmParameters.isredoingExperiment() && fS.containsString("WPLL_test",AlgorithmParameters.output_path+"/"+(a.getPredicate().getPredicateName()+"_stat.res"))){
			result_exists.put(a, true);
			double wpll=fS.extractWpll(AlgorithmParameters.output_path+"/"+(a.getPredicate().getPredicateName()+"_stat.res"),"WPLL_test=",1);
			total_wpll+=wpll;
			System.out.println("***************************************************");
			System.out.println(" The result exists! for "+a+ ". Wpll is "+wpll);
			System.out.println("***************************************************");
			return true;
		}
		else{
			System.out.println(" No results for "+a);
			result_exists.put(a, false);
			return false;
		}
	}
	/**
	 * Evaluate the learned structure on test data		
	 * @param a
	 * @param learned
	 * @param tuPrologQueryMachine_test
	 * @throws IOException
	 */
	private void evaluateLearnedStructure(Atom a,LearnedDependency learned,QueryMachine tuPrologQueryMachine_test) throws IOException {
		//Evaluate atoms designated to be evaluated immediately
		hybrid.structure_learning.LearnedDepStatistics.setOutFile(AlgorithmParameters.output_path+"/", a.getPredicate().getPredicateName()+"_stat.res");				
		System.out.println(" EVALUATION FOR: "+a+ "EVALUATING : "+tuPrologQueryMachine_test.getData().getNrGroundingsInData(a));
		hybrid.structure_learning.LearnedDepStatistics.println(" DATA INFO: \n");
		hybrid.structure_learning.LearnedDepStatistics.println(" TEST DATA: \n"+tuPrologQueryMachine_test.getData().getInfo());
		hybrid.structure_learning.LearnedDepStatistics.println("--------");
		try{
			evaluate(tuPrologQueryMachine_test,learned);
			System.out.println("Evaluation completed ...");
			hybrid.structure_learning.LearnedDepStatistics.println(learned.getStatistics().toString());
			hybrid.structure_learning.LearnedDepStatistics.close();
			hybrid.structure_learning.NetworkWPLLStat.println(a.getPredicate().getPredicateName(),learned.getStatistics().getWPLL_score_test());						
			hybrid.structure_learning.NetworkWPLLStat.flush();
		}
		catch(Exception e){
			e.printStackTrace();
			hybrid.structure_learning.LearnedDepStatistics.println("PROBLEM: "+a);
			hybrid.structure_learning.LearnedDepStatistics.close();
			hybrid.structure_learning.NetworkWPLLStat.println(a.getPredicate().getPredicateName(),Double.NaN);
			hybrid.structure_learning.NetworkWPLLStat.flush();
		}
		hybrid.structure_learning.LearnedDepStatistics.close();
		hybrid.structure_learning.NetworkWPLLStat.close();		
	}

	/**
	 * Generate feature space
	 * @param a
	 * @param fgen
	 * @param ntw
	 * @return
	 * @throws IOException
	 */
	private List<Feature> generateFeatureSpace(Atom a, FeatureGeneratorAbstract fgen,NetworkInfo ntw) throws IOException {
		if(AlgorithmParameters.isDebuggingFlag()){
			hybrid.loggers.Debugger.setOutFile(a,AlgorithmParameters.output_path+"/debugging");
		}
		List<Feature> features=null;

		if(!AlgorithmParameters.isLearn_independent_model()){
			hybrid.loggers.DetailedStructureLearningForAtom.println("Generating features based on: "+ntw.getAtomsAndEqualityConstraints().toString());
			features=fgen.generateFeatures(a, ntw.getAtomsAndEqualityConstraints());
		}
		else{
			features=new ArrayList<Feature>();
		}
		//if space limit set, N features will be sampled from the complete feature space
		if(AlgorithmParameters.getFeatureSpace_Sampling_Limit()!=-1){
			SampleFeatureSpace sFS=new SampleFeatureSpace(AlgorithmParameters.getFeatureSpace_Sampling_Limit());
			return sFS.samplefeatures(features);
		}
		//if cutoff determined return a subset of all features
		else if(AlgorithmParameters.getFeatureSpaceCutoff()!=-1){
			if(AlgorithmParameters.getFeatureSpaceCutoff()>=features.size()){
				return features;
			}
			else{
				return features.subList(0, AlgorithmParameters.getFeatureSpaceCutoff());
			}
		}
		return features;
	}

	/**
	 * Procedure for learning a dependency for one atom.
	 * @param a
	 * @param result_exists
	 * @param query_machine_training
	 * @param query_machine_validation
	 * @param ntw
	 * @param feature_space
	 * @param search
	 * @return
	 * @throws FileNotFoundException
	 * @throws ResultAlreadyExistsException
	 * @throws ResultNotObtainedError
	 */
	private LearnedDependency learnDependency(Atom a,QueryMachine query_machine_training, QueryMachine query_machine_validation, NetworkInfo ntw, List<Feature> feature_space, StructureSearch search) throws FileNotFoundException, ResultAlreadyExistsException, ResultNotObtainedError {
		System.out.println("********************************* LEARNING STRUCTURE FOR: "+a+" ***********************************");
		//if the results for this atom still exist, skip
		double cache_calculation_time=0;
		//outputing data infos
		hybrid.loggers.DetailedStructureLearningForAtom.println(" DATA INFO: \n");
		hybrid.loggers.DetailedStructureLearningForAtom.println(" TRAINING DATA: \n"+query_machine_training.getData().getInfo());
		hybrid.loggers.DetailedStructureLearningForAtom.println(" VALIDATION DATA: \n"+query_machine_validation.getData().getInfo());
		hybrid.loggers.DetailedStructureLearningForAtom.println("FEATURES for "+a);
		long time_begin=System.nanoTime();
		hybrid.loggers.DetailedStructureLearningForAtom.println(" FEATURE SPACE SIZE: "+feature_space.size());

		//setting CACHE
		double cache_begin=System.nanoTime();
		System.out.println(" SETTING CACHE FOR TRAINING DATA: ......");
		query_machine_training.setCache(a, query_machine_training.calculateCache(a, feature_space,"training"));
		hybrid.loggers.DetailedStructureLearningForAtom.println(" Training data cache time: "+((double)(System.nanoTime()-cache_begin))/1000000000.0);
		System.out.println(" SETTING CACHE FOR VALIDATION DATA: ......");
		cache_begin=System.nanoTime();
		query_machine_validation.setCache(a, query_machine_validation.calculateCache(a, feature_space,"validation"));
		hybrid.loggers.DetailedStructureLearningForAtom.println(" Validation data cache time: "+((double)(System.nanoTime()-cache_begin))/1000000000.0);
		cache_calculation_time=((double)(System.nanoTime()-time_begin))/1000000000.0;
		hybrid.loggers.DetailedStructureLearningForAtom.println(" CACHE CALCULATION TIME: "+cache_calculation_time+ " seconds");

		//PERFORMIN THE SEARCH
		try{
			double search_time=0;
			LearnedDependency learnedDependency=search.performSearchForAtom(a,feature_space,query_machine_training,query_machine_validation);
			search_time=learnedDependency.getStatistics().getLearningTime();
			learnedDependency.getStatistics().setLearningTime(search_time+cache_calculation_time);
			System.out.println(" LEARNED STRUCTURE: "+learnedDependency);
			return learnedDependency;
		}

		catch(Exception e){
			e.printStackTrace();
			throw new ResultNotObtainedError();
		}
	}

	/**
	 * Evaluate a learned dependency with a query machine test containing all neccessary interpretations.
	 * The statistics of the learned dependency is accordignly updated to contains scores on test data.
	 * @param query_machine_test
	 * @param learned
	 */
	public void evaluate(QueryMachine query_machine_test,LearnedDependency learned) {
		hybrid.loggers.DetailedStructureLearningForAtom.println("EVALUATING : "+learned+" ------------------------------");
		System.out.println("EVALUATING : "+learned+" ------------------------------");
		if(learned==null){
			return;
		}

		long time_begin=System.nanoTime();
		QueryData qD=query_machine_test.getQueryResults(learned.getDep());
		double wpll=learned.getDep().getCpd().getCpdEvaluator().calculatePLL(qD,learned.getDep().getCpd().getParameters(),query_machine_test.getPenalty());
		double final_inference_time=((double)(System.nanoTime()-time_begin))/1000000000.0;

		learned.getStatistics().setNr_test_instances(query_machine_test.getData().getNrGroundingsInData(learned.getDep().getHead()));		
		learned.getStatistics().setWPLL_score_test(wpll/learned.getStatistics().getNr_test_instances());		
		learned.getStatistics().setInferenceTime(learned.getDep().getHead(),final_inference_time);
		hybrid.loggers.DetailedStructureLearningForAtom.println("TIME PER GROUND ATOM (INFERENCE): "+qD.getInference_time_per_ground_atom().get(learned.getDep().getHead()));
		hybrid.loggers.DetailedStructureLearningForAtom.close();
	}

}
