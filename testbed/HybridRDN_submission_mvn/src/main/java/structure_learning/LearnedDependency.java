package hybrid.structure_learning;

import java.util.concurrent.atomic.AtomicStampedReference;
import java.util.*;
import hybrid.dependencies.Dependency;
import hybrid.experimenter.LearnedDependencyStatistics;
import hybrid.features.Feature;
import hybrid.queryMachine.FeatureCache;
import hybrid.querydata.QueryData;

/**
 * This class represents a pair of dependency and its score in the current
 * learnin procedure
 * @author irma
 *
 */
public class LearnedDependency implements Comparable {
	private LearnedDependencyStatistics statistics;
	private Dependency dep;
	private double score;
	
	public LearnedDependency(Dependency dep,double score){
	   this.dep=dep;
	   this.score=score;
	   
	}

	public Dependency getDep() {
		return dep;
	}

	public void setDep(Dependency dep) {
		this.dep = dep;
	}

	public double getScore() {
		return score;
	}

	public void setScore(double score) {
		this.score = score;
	}
	
	public String toString(){
		return "Score("+dep+") = "+score;
	}

	@Override
	public int compareTo(Object arg0) {
		if(this.score>((LearnedDependency)arg0).score){
			return -1;
		}
		if(this.score<((LearnedDependency)arg0).score){
			return 1;
		}
		return 0;
	}

	public boolean betterThan(LearnedDependency currentScore) {
		if(this.score>currentScore.getScore()){
			return true;
		}
		return false;
	}

	public LearnedDependencyStatistics getStatistics() {
		return statistics;
	}

	public void setStatistics(LearnedDependencyStatistics statistics) {
		this.statistics = statistics;
	}
	
	
	
	
	
	
}

