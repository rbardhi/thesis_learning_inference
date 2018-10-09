package hybrid.experimenter;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.DailyRollingFileAppender;
import org.apache.log4j.FileAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;

import hybrid.network.Predicate;
import hybrid.penalties.Penalty;
import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.Namespace;


/**
 * Argument parser from string input
 * @author irma
 *
 */
public class ParseArguments {
	String message="";
	
	public ParseArguments(String message){
		this.message=message;
	}
	
	public ParseArguments(){
		
	}
	

	public void parseArgumentsHRDN(String[] args) throws IOException{
		
		ArgumentParser parser = ArgumentParsers.newArgumentParser("prog")
	    .defaultHelp(true)
		.description("Process paths.");
		
		/**
		 * input represents input to the data
		 */
		parser.addArgument("-input")
		.dest("input_path")
		.type(String.class)
		.help("Path containing folder train/test/validate with interpretations used by the learning algorithm")
		.required(true)
		;

		/*
		 * output represents the path where the outputs will be written
		 */
		parser.addArgument("-output")
		.dest("outputPath")
		.help("Path where the logging and results will be written")
		.type(String.class)
		.required(true);
		
		/*
		 * penalty validation used for scoring. Supported: bic and mdl
		 */
		parser.addArgument("-penalty_validation")
		.dest("penalty_validation")
		.help("Penalty")
		.type(String.class)
		.setDefault(new String("mdl"))
		.required(false);

		/*
		 * predicate names of predicates for which the learning will be performed
		 */
		parser.addArgument("-predicates")
		.dest("predicates")
		.help("Specify one predicate for which the structure learning will be learned")
		.type(String.class)
		.nargs("+")
		.setDefault(new Predicate[]{})
		.required(false);
		
		/*
		 * specifies if the evaluation will be performed. If not, only the models will be learned
		 */
		parser.addArgument("-evaluation")
		.dest("evaluation")
		.help("Specifies if the evaluation is to be performed. By default it is true. If not, only the model is learned.")
		.type(Boolean.class)
		.setDefault(true)
		.required(false);


		/*
		 * flag that denotes that discretization occured
		 */
		parser.addArgument("-discretization")
		.dest("discretization_flag")
		.help("Flag which denotes whether the learner is to perform in only discretized mode")
		.type(Boolean.class)
		.setDefault(new Boolean(false))
		.required(false);

		/*
		 * path to file which contains discrtetization info of the data
		 * that has been discretized already
		 */
		parser.addArgument("-discretization_path")
		.dest("discretization_info_path")
		.help("Argument which shows the path to the file which contains discretization info for each discretized predicate")
		.type(String.class)
		.setDefault("")
		.required(false);


		/*
		 * discretization level 
		 */
		parser.addArgument("-discretization_level")
		.dest("Level")
		.help("discretization level")
		.type(Integer.class)
		.setDefault(new Integer(-1))
		.required(false);
		
		/**
		 * use feature blocks when evaluating features
		 */
		parser.addArgument("-useFeatureBlocks")
		.dest("useFeatureBlocks")
		.help("The flag representing whether the features defined on the same conjunction (e..g, mac(nrhours(C)), min(nrhours(C)),average(nrhours(C))), should be treaded as one block. The result is in computation savings. Calling prolog too often is avoided. Default = true")
		.type(Boolean.class)
		.setDefault(new Boolean(true))
		.required(false);
		
		
		/**
		 * set maximum feature length
		 */
		parser.addArgument("-feature_length")
		.dest("feature_length")
		.help("feature length when creating feature space. Default=3")
		.type(Integer.class)
		.setDefault(new Integer(5))
		.required(false);
		
		/**
		 * When we want to reduce learning, we can limit the size of the feature space. With this flag set
		 * the features will be randomly selected from the complete feature set
		 */
		parser.addArgument("-feature_space_sample_limit")
		.dest("feature_space_sample_limit")
		.help("Limit feature space size")
		.type(Integer.class)
		.setDefault(new Integer(-1))
		.required(false);
		
		/**
		 * set feature space cutof 
		 */
		parser.addArgument("-feature_space_cutoff")
		.dest("feature_space_cutoff")
		.help("Ignore all the feature after the cutoff")
		.type(Integer.class)
		.setDefault(new Integer(-1))
		.required(false);
		
		/**
		 * set the number of logical variable renaming
		 */
		parser.addArgument("-nr_logvar_renamings")
		.dest("nr_logvar_renamings")
		.help("number of different logvar renamings allowed when creating feature space. Default=3")
		.type(Integer.class)
		.setDefault(new Integer(2))
		.required(false);
		
		/**
		 * limit the number of randvar tests per feature
		 */
		parser.addArgument("-nr_randvarTest_per_feature")
		.dest("nr_randvarTest_per_feature")
		.help("number of different logvar renamings allowed when creating feature space. Default=3")
		.type(Integer.class)
		.setDefault(new Integer(2))
		.required(false);

		/**
		 * set the flag for learning an independent model
		 */
		parser.addArgument("-independent")
		.dest("independent_flag")
		.help("The flag representing that an independent model should be learnt (true if yes)")
		.type(Boolean.class)
		.setDefault(new Boolean(false))
		.required(false);
		
		parser.addArgument("-debugging")
		.dest("debugging")
		.help("The flag representing whether debugging is to be done or not (true if yes)")
		.type(Boolean.class)
		.setDefault(new Boolean(false))
		.required(false);
		
		parser.addArgument("-rt")
		.dest("rt")
		.help("The flag denoting whether there would be a file with runtime information")
		.type(Boolean.class)
		.setDefault(new Boolean(false))
		.required(false);
		
		parser.addArgument("-redo")
		.dest("redo")
		.help("A flag for redoing the experiments for this instance. This would erase all the existing files and re-write them. Otherwise, it will preserve the files for which results exist.")
		.type(Boolean.class)
		.setDefault(new Boolean(false))
		.required(false);
			
		parser.addArgument("-logging")
		.dest("logging_flag")
		.help("The flag denoting if logging is to be done (by default is true)")
		.type(Boolean.class)
		.setDefault(new Boolean(true))
		.required(false);
		
		

		Namespace res=null;

		try {
			res = parser.parseArgs(args);
		} catch (ArgumentParserException e) {
			System.out.println(" Didn't succeed to parse the parameters");
			parser.handleError(e);
			System.exit(1);
		}
		hybrid.experimenter.AlgorithmParameters.setFeatureSpaceLimitSample(res.getInt("feature_space_sample_limit"));
		hybrid.experimenter.AlgorithmParameters.setUsingFeatureBlocks(res.getBoolean("useFeatureBlocks"));
		hybrid.experimenter.AlgorithmParameters.setFeatureSpaceCutoff(res.getInt("feature_space_cutoff"));
		hybrid.experimenter.AlgorithmParameters.setNrRandvarTestPerFeature(res.getInt("nr_randvarTest_per_feature"));
		hybrid.experimenter.AlgorithmParameters.setInput_path(res.getString("input_path"));
		hybrid.experimenter.AlgorithmParameters.setFeatureLength(res.getInt("feature_length"));
		hybrid.experimenter.AlgorithmParameters.setNrLogvarRenamings(res.getInt("nr_logvar_renamings"));
		hybrid.experimenter.AlgorithmParameters.setRedoingFlag(res.getBoolean("redo"));
		hybrid.experimenter.AlgorithmParameters.setEvaluation_flag(res.getBoolean("evaluation"));
		hybrid.experimenter.AlgorithmParameters.setOutput_path(res.getString("outputPath"));
		hybrid.experimenter.AlgorithmParameters.setPredicates(res.getList("predicates"));
		hybrid.experimenter.AlgorithmParameters.setDiscretization_flag(res.getBoolean("discretization_flag"));
		File data_loading_info_file=new File(res.getString("outputPath")+"/data_loading.info");
		if(data_loading_info_file.exists()){
			data_loading_info_file.delete();
		}
		hybrid.experimenter.AlgorithmParameters.setDataLoaderFile(data_loading_info_file);
		hybrid.experimenter.AlgorithmParameters.setDebugging(res.getBoolean("debugging"));
		
		if(res.getInt("Level")!=-1){
			System.out.println(" Discretization set");
			if(res.getString("discretization_info_path").isEmpty()){
				System.out.println(" Discretization path is empty");
				hybrid.experimenter.AlgorithmParameters.setDiscretization_info_path(res.getString("input_path")+"discretization_info.info");
			
			}
			else{
				hybrid.experimenter.AlgorithmParameters.setDiscretization_info_path(res.getString("discretization_info_path"));
			}
			hybrid.experimenter.AlgorithmParameters.setDiscretization_level(res.getInt("Level"));
		}
		
		if(res.getBoolean("rt")){
			hybrid.experimenter.AlgorithmParameters.setTrackRunningTimeFlag();
			hybrid.experimenter.AlgorithmParameters.setWriter(new BufferedWriter(new FileWriter(new File(res.getString("outputPath")+"/runtime.log"))));
		}
		//I need a path for error reporting static and reachable for all - !!!
		hybrid.experimenter.AlgorithmParameters.log_errors=Logger.getLogger("errors");
		hybrid.experimenter.AlgorithmParameters.log_errors.setLevel(Level.INFO);
		//FileAppender appender = new FileAppender(new PatternLayout(PatternLayout.DEFAULT_CONVERSION_PATTERN), hybrid.experimenter.AlgorithmParameters.getOutput_path()+"/errors.log");
		//hybrid.experimenter.AlgorithmParameters.log_errors.addAppender(appender);
		hybrid.experimenter.AlgorithmParameters.setLearn_independent_model(res.getBoolean("independent_flag"));
		hybrid.experimenter.AlgorithmParameters.setcache(true); //always true
		hybrid.experimenter.AlgorithmParameters.setDetailed_logging_flag(res.getBoolean("logging_flag"));
		hybrid.experimenter.AlgorithmParameters.setPenaltyType(Penalty.instantiatePenalty(res.getString("penalty_validation")));
        File parameter_file=new File(hybrid.experimenter.AlgorithmParameters.getOutput_path()+"/parameters.pars");
        FileWriter fw=new FileWriter(parameter_file);
        hybrid.experimenter.AlgorithmParameters inst=new AlgorithmParameters();
        fw.write("Version info: "+this.message+"\n");
        fw.write(inst.toString());
        fw.close();
	}


}
