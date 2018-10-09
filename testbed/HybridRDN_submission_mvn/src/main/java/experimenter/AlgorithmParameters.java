package hybrid.experimenter;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.apache.log4j.Logger;

import hybrid.network.Atom;
import hybrid.penalties.Penalty;
import hybrid.structure_learning.Paths;

/**
 * This class represents all algorithm parameters
 * @author irma
 *
 */
public class AlgorithmParameters {

	private static AlgorithmParameters singleton = new AlgorithmParameters( );

	/* A private Constructor prevents any other 
	 * class from instantiating.
	 */	   
	/* Static 'instance' method */
	public static AlgorithmParameters getInstance( ) {
		return singleton;
	}
	public static Logger log_errors;//logger for errors
	public static String input_path; // path to input file
	public static String output_path; //path to output file
	public static String discretization_info_path; //path to the file containing the discretization information
	//predicate_names contains a predicates from which structure learning is to be performed
	//if not specified then structure learning is performed for all predicates
	public static List<String> predicates; //predicate names for which we do the learning
	public static boolean learn_for_a_subset_of_predicates=false; //flag: learning only for a subset of all possible predicates?
	public static boolean learn_independent_model=false; //flag: learn independent model?
	public static boolean discretization_flag=false; //flag: did discretization occur?
	public static boolean useSubsampledNegatives=false; //subsample negative examples
	public static boolean detailed_logging_flag=false; //use detailes logging flag
	public static boolean redoing_experiments=false; //redoing experiments (deleting all logs)
	public static int discretization_level=-1; //the number of bins used for discretization
	public static Penalty penalty_validation; //penalty used for scoring
	//if cache flag set, the feature space will first be pre-calculated and then searched over. Otherwise,
	//the feature space is evaluated feature by feature. Warning, PROBLEM with cache being TRUE!!!
	public static boolean cache=false;
	public static boolean debugging_flag=false;
	public static boolean evaluation_flag=true; //determines if the evaluation is to be done on test data
	//feature_length represents the allowed length of the features in the feature space
	public static int feature_length=3;
	public static int feature_space_sampling_limit=-1;
	//nr_logvar_renamings determines how many different logvars can be used per feature
	public static int nr_logvar_renamings=2; //number of allowed logvar renamings
    public static HashMap<Atom,Double> ratio_for_subsampling=new HashMap<Atom, Double>();
	private static Integer nr_randvarTest_per_feature=1;
	private static File data_loading_info_file;
	private static BufferedWriter trackRunTimeFile;
	//flag for tracking the running time
	private static boolean track_running_time;
	//with this cutoff we don't sample n feature, but instead we ignore all the features (in order they are generated) after the cutoff
	//if this is set we don't sample, and vice versa for feature_space_limit
	private static Integer feature_space_cutoff=-1;
	//this flag denotes whether we group features defined on same conjunctions together. E.g., we would group max(nrhours(C)) and min(nrhours(C)) in one 
	//block. The result is that we would calculate Markov blankets only for the first feature. These Markov blankets will be used then for
	//min(nrhours(C)) which would calculate minimum instead of maximum. The result is saving time for costly queries in TuProlog. 
	//We set to default that this method is used.
	private static Boolean useFeatureBlocks=true;


	public static boolean isDiscretization_flag() {
		return discretization_flag;
	}

	public static void setDiscretization_flag(boolean discretization_flag) {
		AlgorithmParameters.discretization_flag = discretization_flag;
	}

	public static void setDetailedLoggingFlag() {
		detailed_logging_flag=true;

	}

	public static void useSubsampledNegatives() {
		useSubsampledNegatives=true;
	}

	public static boolean doesItuseSubsampledNegatives() {
		return useSubsampledNegatives;
	}


	public static String getInput_path() {
		return input_path;
	}

	public static void setInput_path(String input_path) {
		AlgorithmParameters.input_path = input_path;
	}

	public static String getOutput_path() {
		return output_path;
	}

	public static void setOutput_path(String output_path) {
		File f=new File(output_path);
		f.mkdirs();
		AlgorithmParameters.output_path = output_path;
	}

	public static String getDiscretization_info_path() {
		return discretization_info_path;
	}

	public static void setDiscretization_info_path(String discretization_info_path) {
		AlgorithmParameters.discretization_info_path = discretization_info_path;
	}

	public static boolean isUseSubsampledNegatives() {
		return useSubsampledNegatives;
	}

	public static void setUseSubsampledNegatives(boolean useSubsampledNegatives) {
		AlgorithmParameters.useSubsampledNegatives = useSubsampledNegatives;
	}

	public static int getDiscretization_level() {
		return discretization_level;
	}

	public static void setDiscretization_level(int discretization_level) {
		setDiscretization_flag(true);

		AlgorithmParameters.discretization_level = discretization_level;
	}

	public static List<String> getPredicate_names() {
		return predicates;
	}

	public static void setPredicates(List<Object> predicate_names) {
		if(predicate_names.size()!=0){
			AlgorithmParameters.predicates = new ArrayList<String>();
			for(Object o:predicate_names){
				predicates.add(o.toString());
			}
			AlgorithmParameters.setLearn_for_a_subset_of_predicates(true);
		}
		else{
			AlgorithmParameters.predicates=null;
			AlgorithmParameters.setLearn_for_a_subset_of_predicates(false);

		}

	}

	public static boolean isLearn_for_a_subset_of_predicates() {
		return learn_for_a_subset_of_predicates;
	}

	public static void setLearn_for_a_subset_of_predicates(
			boolean learn_for_a_subset_of_predicates) {
		AlgorithmParameters.learn_for_a_subset_of_predicates = learn_for_a_subset_of_predicates;
	}

	public static boolean isDetailed_logging_flag() {
		return detailed_logging_flag;
	}

	public static void setDetailed_logging_flag(boolean detailed_logging_flag) {
		AlgorithmParameters.detailed_logging_flag = detailed_logging_flag;
	}

	public static Penalty getPenaltyType(){
		return penalty_validation;
	}
	
	public static void setPenaltyType(Penalty penalty){
		penalty_validation=penalty;
	}
	

	public String toString() {
		return "----------------  AlgorithmParameters --------------------- "
				+"\n input_path = " + input_path
				+"\n output_path = " + output_path
				+"\n discretization info path = " + discretization_info_path
				+"\n is learning independent model = " + learn_independent_model
				+"\n is learning discretized model = " + discretization_flag
				+"\n discretization_level ="+ discretization_level
				+"\n is using subsampled negatives = " + useSubsampledNegatives
				+"\n is doing detailed logging = "+ detailed_logging_flag
				+"\n is redoing experiments = "+redoing_experiments
				+"\n is using feature blocks =  "+isUsingFeatureBlocks() 
				+"\n is cache being used (default is true) =  "+cache
				+"\n feature length = "+feature_length
				+"\n feature space cutoff = "+feature_space_cutoff
				+"\n is model to be evaluated = "+evaluation_flag
				+"\n debugging flag set = "+debugging_flag
				+"\n no. logvar renamings = "+nr_logvar_renamings
				+"\n penalty validation = "+penalty_validation
				+"\n ratios_for_subsampling = "+ratio_for_subsampling
				+"\n output_path =" + output_path
				+"\n discretization_info_path =" + discretization_info_path
				+"\n nr_randvarTestsPerFeature = "+nr_randvarTest_per_feature
				+"\n predicate_name =" + predicates
				+"\n feature space limit (sampling) =  "+feature_space_sampling_limit
				+"\n learn_for_a_subset_of_predicates = "+learn_for_a_subset_of_predicates;
			    
			    
	}

	
	public static AlgorithmParameters getSingleton() {
		return singleton;
	}
	
	public static void addRatio_for_Subsampling(Atom a,double ratio){
		ratio_for_subsampling.put(a, ratio);
	}
	
	public static double getRatio_for_subsampling(Atom a){
		return ratio_for_subsampling.get(a);
	}

	public static void setSingleton(AlgorithmParameters singleton) {
		AlgorithmParameters.singleton = singleton;
	}


	public static boolean isLearn_independent_model() {
		return learn_independent_model;
	}

	public static void setLearn_independent_model(boolean learn_independent_model) {
		AlgorithmParameters.learn_independent_model = learn_independent_model;
	}

	public static void setRedoingFlag(Boolean redo) {
		redoing_experiments=redo;
	}
	
	public static boolean isredoingExperiment(){
		return redoing_experiments;
	}
	
	public static void setcache(boolean flag){
		cache=flag;
	}
	
	public static boolean isCacheSet(){
		return cache;
	}
	
	public static int getFeatureLength(){
		return feature_length;
	}
	
	public static void setFeatureLength(int feature_length1){
		feature_length=feature_length1;
	}

	public static int getNrLogvarRenamings(){
		return nr_logvar_renamings;
	}
	
	public static void setNrLogvarRenamings(int nr_logvar_renamings1){
		nr_logvar_renamings=nr_logvar_renamings1;
	}

	public static boolean isDebuggingFlag() {
		return debugging_flag;
	}
	
	public static void setDebugging(boolean flag) {
		debugging_flag=flag;
	}

	public static boolean isEvaluation_flag() {
		return evaluation_flag;
	}

	public static void setEvaluation_flag(boolean evaluation_flag) {
		AlgorithmParameters.evaluation_flag = evaluation_flag;
	}

	public static void setFeatureSpaceLimitSample(Integer limit) {
		feature_space_sampling_limit=limit;
		
	}
	
	public static int getFeatureSpace_Sampling_Limit(){
		return feature_space_sampling_limit;
	}

	public static void setNrRandvarTestPerFeature(Integer int1) {
		nr_randvarTest_per_feature=int1;
		
	}
	
	public static int getNrRandvarTestPerFeature() {
		return nr_randvarTest_per_feature;
		
	}

	public static void setDataLoaderFile(File file) {
		data_loading_info_file=file;
	}
	
	public static File getDataLoadingFile(){
		return data_loading_info_file;
	}
	
	public static boolean isTrackRunningTimesFlag() {
		return track_running_time;
	}
	
	public static BufferedWriter getTrackRunTimeWriter() {
		return trackRunTimeFile;
	}

	public static void setTrackRunningTimeFlag() {
		track_running_time=true;
		
	}

	public static void setWriter(BufferedWriter bufferedWriter) {
		trackRunTimeFile=bufferedWriter;
		
	}

	public static void writeToTrackWriter(String string) {
		try {
			trackRunTimeFile.append(string+"\n");
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}

	public static void setFeatureSpaceCutoff(Integer int1) {
		feature_space_cutoff=int1;
		
	}
	
	public static int getFeatureSpaceCutoff(){
		return feature_space_cutoff;
	}

	public static void setUsingFeatureBlocks(Boolean boolean_flag) {
		useFeatureBlocks=boolean_flag;
		
	}
	
	public static boolean isUsingFeatureBlocks(){
		return useFeatureBlocks;
	}
	
	
}
