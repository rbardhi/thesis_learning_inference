package hybrid.parameters;

import static org.junit.Assert.*;
import hybrid.cpds.WrongParameterNumber;
import hybrid.dependencies.Dependency;
import hybrid.featureGenerator.AbstractConjunction;
import hybrid.featureGenerator.Standard_Conjunction;
import hybrid.featureGenerator.ConjunctionConstructionProblem;
import hybrid.features.Feature;
import hybrid.features.FeatureTypeException;
import hybrid.features.Mode;
import hybrid.features.ValueFt;
import hybrid.network.Atom;
import hybrid.network.BooleanPred;
import hybrid.network.CategoricalPred;
import hybrid.network.GaussianPred;
import hybrid.network.Logvar;
import hybrid.network.NetworkInfo;
import hybrid.network.PosLiteral;
import hybrid.network.Predicate;
import hybrid.network.StringValue;
import hybrid.network.Type;
import hybrid.network.WrongValueType;

import org.junit.Before;
import org.junit.Test;

public class TestLogRegregressors {

	private static NetworkInfo ntw;
	private static String pathToInterpretations=System.getProperty("user.dir")+"/tests/test_data/small_university";
	private static Atom intelligence;
	private static Atom grade;
	private static Atom teaches;
	private static Atom ability;
    private static Atom takes;
	private static Atom difficulty;
	private static Atom friend;
	private static Logvar student;
	private static Logvar student1;
	private static Logvar course;
	private static Logvar professor;
	private static Dependency dep1;
	private static Dependency dep2;
	private static AbstractConjunction testClass;
	
	/**
	 * Creating very simple network of only grade and intelligence predicate. 
	 * Testing on two dependencies:
	 * intelligence(S) | Mode{grade(S,c)}
	 * grade(S,C) | Value{intelligence(S)}
	 * @throws FeatureTypeException 
	 */
	@Before
	public void setUp() throws FeatureTypeException{
		System.out.println(pathToInterpretations);
		Type stud=new Type("student");
		Type c=new Type("course");
		Type p=new Type("prof");
		student=new Logvar("S",stud);
		student1=new Logvar("S1",stud);
		course=new Logvar("C",c);
		professor=new Logvar("P",p);
		Predicate intel=new GaussianPred("intelligence",1,1.0,2.0);
		Predicate gr=new CategoricalPred("grade",2,new String[]{"low","mid","high"});
		Predicate tk=new BooleanPred("takes",2);
		Predicate ab=new GaussianPred("ability",1,1.0,2.0);
		Predicate tch=new BooleanPred("teaches",2);
		Predicate fr=new BooleanPred("friend",2);
		Predicate diff=new CategoricalPred("difficulty",1,new String[]{"easy","medium","hard"});
		intelligence=new Atom(intel, new Logvar[]{student});
		grade=new Atom(gr, new Logvar[]{student,course});
		takes=new Atom(tk, new Logvar[]{student,course});
		ability=new Atom(ab,new Logvar[]{professor});
		teaches=new Atom(tch,new Logvar[]{professor,course});
		difficulty=new Atom(diff,new Logvar[]{course});
		friend=new Atom(fr,new Logvar[]{student,student});
		ntw=new NetworkInfo(new Atom[]{intelligence,grade,takes,teaches,ability,difficulty},new Type[]{stud,c,p});	
		}
	
	@Test
	public void testnrParameters() throws FeatureTypeException, WrongParameterNumber, WrongValueType, WrongValueSpecification, ConjunctionConstructionProblem{
		ValueFt ft1=new ValueFt(new Standard_Conjunction(grade,new PosLiteral(intelligence)));
		ValueFt ft2=new ValueFt(new Standard_Conjunction(grade,new PosLiteral(takes),new PosLiteral(teaches),new PosLiteral(ability)));
		Dependency dep=new Dependency(grade,new Feature[]{ft1,ft2});
		LogRegregressors pars=new LogRegregressors(dep);
		pars.addConditionalParameter(new StringValue("low"), new Regression(20.5,dep.getFeatures(),new Double[]{2.5,3.5}));
		pars.addConditionalParameter(new StringValue("mid"), new Regression(40.5,dep.getFeatures(),new Double[]{8.5,9.5}));
		pars.addConditionalParameter(new StringValue("high"), new Regression(50.5,dep.getFeatures(),new Double[]{10.5,12.5}));
        assertEquals(10,pars.getNumberOfFreeParameters());
		
	}
	
	
}
