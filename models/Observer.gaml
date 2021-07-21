/**
* Name: Observer
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Observer

import "Global.gaml"

global {	
	
	// ########################
	// Job Satisfaction stability
	
	// Window of agent satisfaction
	list<list<float>> sats;
	
	// Constants for observers
	string MIN <- "min"; string MAX <- "max"; string MED <- "median"; string AVR <- "mean"; string STD <- "std";
	
	// Satisfaction equilibrium
	reflex equilibrium {
		if sats=nil or empty(sats) { sats <- list_with(length(worker),list<float>([])); }
		ask worker { 
			sats[int(self)] <+ job_satisfaction;
			if length(sats[int(self)]) > windows { sats[int(self)][] >- 0; }
		}
	}
	
	// stop the simulation if satisfaction does not move more than 'epsilon' in a 'windows' time frame for every agent
	bool stop_sim(float epsilon <- EPSILON) {
		do syso(sample(sats)); 
		return sats none_matches (length(each) < windows or abs(min(each) - max(each)) > epsilon);
	}
	
	// #########################
	// SATISFACTION DISTRIBUTION
	
	/*
	 * The index is a distance metric based on: </br>
	 * <ul>
	 *  <li> Peak : how far the least disperse percentil is from Q3
	 *  <li> Curve : how many percentil does not fit the descending curve until Q3 then up to max
	 * </ul> <p>
	 * closest index to 0 means a good fit, closest to 2 means a poor fit 
	 */
	float sat_dist_index(observer obs) {
		if obs.quantiles=nil or empty(obs.quantiles) {return -1;}
		float i; // the index
		list<float> std_percentiles <- obs.quantiles collect (each[obs.stat_profile index_of STD]);
		
		// least deviatives percentile
		int idx_least_std <- (std_percentiles index_of min(std_percentiles)); 
		
		// Peak should be close to Q3
		i <- i + abs(idx_least_std / length(std_percentiles) - 3.0 * length(std_percentiles) / 4);
		
		// How std curve fits u-shape
		int curve_dist;
		loop idx from:0 to:length(std_percentiles)-2 {
			if idx < idx_least_std { if std_percentiles[idx] > std_percentiles[idx+1] {curve_dist <- curve_dist+1;} }
			else { if std_percentiles[idx] < std_percentiles[idx+1] {curve_dist <- curve_dist+1;} }
		}
		i <- i + 1-curve_dist/(length(std_percentiles)-1);
		
		return i;
	} 
	
	// #########################
	// GENDER SATISFACTION
	
	/*
	 * The index measure how women does compare to men: <p>
	 * - if it is positive, it means women are more satisfied (upper it is, the more there are satisfied overall) <p>
	 * - if it is negative, it means women are less satisfied <p>
	 */
	float gender_index(observer obs) {
		if obs.ws=nil or empty(obs.ws) or obs.ms=nil or empty(obs.ms) {return -1;}
		return obs.ws[obs.stat_profile index_of AVR] - obs.ms[obs.stat_profile index_of AVR];
	}
	
	// #########################
	// AGE SATISFACTION DISTRIBUTION
	
	float age_index(observer obs, int y <- 25, int e <- 55) {
		if obs.age_distribution=nil or empty(obs.age_distribution) {return -1;}
		list<float> young <- []; list<float> mid <- []; list<float> elder <- [];
		loop age over:obs.age_distribution.keys {
			float m <- obs.age_distribution[age][obs.stat_profile index_of AVR];
			if age < y { young <+ m;} else if age < e { mid <+ m;} else { elder <+ m;}
		}
		return (young count (each > min(mid)) / 1.0*length(young)) * (mid count (each < min(elder)) / 1.0*length(mid)); 
	}
	
	/*
	 * return p_value, for age sat-unsat (higher or lower than median sat) correlation
	 */
	float age_dependancy_p_value(observer obs) {
		float med <- obs.overall_profile[obs.stat_profile index_of MED];
		
		float st;
		loop age over:obs.age_distribution {
			float expected <- length(age)/2.0;
			st <- st + (age count (each < med) - expected)^2/expected + (age count (each > med) - expected)^2/expected;
		}
		
		float alpha <- 0.5;
		loop while:true {
			if st < chi_square(alpha,length(obs.age_distribution)-1) {return alpha;}
			alpha <- alpha/10;
		}
	}
}

species observer {
	
	list<string> stat_profile <- [MIN,MAX,MED,AVR,STD];
	list<worker> target_agents;
	list<float> overall_profile;
	
	// Job satisfaction distribution
	// each Q = min,max,median,mean,std
	list<list<float>> quantiles;
	
	// Women vs men
	list<float> ws;
	list<float> ms;
	
	// U-shape with age
	// each age = min,max,median,mean,std
	map<int,list<float>> age_distribution;
	
	int freq <- 0;
	bool triggered <- false;
	
	/*
	 * The method that observe what happens in the simulation
	 */
	reflex observe when:every(freq) and triggered {
		// Overall
		overall_profile <- statistic_profile(target_agents collect (each.job_satisfaction));
		
		ask world {do syso(sample(every(myself.freq) and myself.triggered));}
		
		// Quantiles
		quantiles <- [];
		list<list<float>> all_sat_sorted <- list<list<float>>((target_agents collect (each.job_satisfaction)) split_in q_number);
		loop q over:all_sat_sorted {quantiles <+ statistic_profile(q);}
		
		// W vs M
		loop gender over:target_agents group_by (each.demographics[GENDER]) {
			list<float> gender_sat <- gender collect (each.job_satisfaction);
			if first(gender).demographics[GENDER]="M" { ms <- statistic_profile(gender_sat);} else {ws <- statistic_profile(gender_sat);}
		}
		
		// U-shaped age x sat
		age_distribution <- [];
		map<int,list<worker>> age_workers <- target_agents group_by (AGE.get_numerical_value(each.demographics[AGE]));
		loop age over:age_workers.keys { age_distribution[age] <- statistic_profile(age_workers[age] collect (each.job_satisfaction)); }
	}
	
	/*
	 * Build the statistic profile
	 */
	action statistic_profile(list<float> values) {
		list<float> sp <- [];
		loop s over:stat_profile {
			switch s {
				match MIN { sp <+ min(values); }
				match MAX { sp <+ max(values); }
				match MED { sp <+ median(values); }
				match AVR { sp <+ mean(values); }
				match STD { sp <+ standard_deviation(values); }
			}
		}
		return sp;
	}
}
