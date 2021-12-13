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
	// OBSERVER
	
	// Constants for observers
	string MIN <- "min"; string MAX <- "max"; string MED <- "median"; string AVR <- "mean"; string STD <- "std";
		
	// INIT OBSERVER
	action init_observer {
		create observer with:[target_agents::list(worker),freq::1];
		main_observer <- first(observer);
	}
	
	// ########################
	// Job Satisfaction stability
	
	// Window of agent satisfaction
	list<list<float>> sats;
	
	// Satisfaction equilibrium
	reflex equilibrium {
		if sats=nil or empty(sats) { sats <- list_with(length(worker),list<float>([])); }
		ask worker { 
			sats[int(self)] <+ _job_satisfaction;
			if length(sats[int(self)]) > windows { sats[int(self)][] >- 0; }
		}
	}
	
	// stop the simulation if satisfaction does not move more than 'epsilon' in a 'windows' time frame for every agent
	bool stop_sim(float epsilon <- EPSILON) {
		do syso(sample(sats),level::debug_level(0)); 
		bool stop <- sats none_matches (length(each) < windows or abs(min(each) - max(each)) > epsilon);
		return stop;
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
		
		// Peak should be close to 0.75 percentile
		i <- abs(idx_least_std / length(std_percentiles) - 0.75);
		
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
	 * Pearson's correlation coefficient
	 * - if it is positive, it means women (numerical code is 1) are more satisfied <p>
	 * - if it is negative, it means women are less satisfied <p>
	 */
	float gender_pearson(observer obs) {
		if obs.gender_sat=nil or empty(obs.gender_sat) {return 0;}
		return correlation(
			obs.gender_sat collect (each.key),
			obs.gender_sat collect (each.value)
		);
	}
	
	// #########################
	// AGE SATISFACTION DISTRIBUTION
	
	float age_pseudo_two_lines_index(observer obs, int c <- 55) {
		if not(obs.triggered) {return 0.0;}
		pair p <- obs.get_age_matrix(c); 
		regression below_c <- build(p.key);
		regression above_c <- build(p.value);
		
		float res;
		
		list pbc <- []; list ebc <- [];
		loop r over:rows_list(p.key) { pbc <+ last(r); ebc <+ predict(below_c,[first(r)]); } 
		float pvalue_bc <- 0.0; //tTest(pbc,ebc);
		list pac <- []; list eac <- [];
		loop r over:rows_list(p.value) { pac <+ last(r); eac <+ predict(above_c,[first(r)]); }
		float pvalue_ac <- 0.0; //tTest(pac,eac);
		
		if first(below_c.parameters) < 0 { res <- res + 1; }
		if first(above_c.parameters) > 0 { res <- res + 2; }
		
		return res*(1-pvalue_bc)*(1-pvalue_ac)/2;
	}
	
	float age_index(observer obs, int y <- 25, int e <- 55) {
		if obs.age_distribution=nil or empty(obs.age_distribution) {return -1;}
		list<float> young <- []; list<float> mid <- []; list<float> elder <- [];
		loop age over:obs.age_distribution.keys {
			float m <- obs.age_distribution[age][obs.stat_profile index_of AVR];
			if age < y { young <+ m;} else if age < e { mid <+ m;} else { elder <+ m;}
		}
		return (young count (each > max(mid)) / (1.0*length(young))) * (mid count (each < min(elder)) / (1.0*length(mid))); 
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
	list<pair<int,float>> gender_sat;
	
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
		overall_profile <- statistic_profile(target_agents collect (each._job_satisfaction));
		
		ask world {do syso(sample(every(myself.freq) and myself.triggered));}
		
		// Quantiles
		quantiles <- [];
		list<list<float>> all_sat_sorted <- list<list<float>>((target_agents collect (each._job_satisfaction)) split_in q_number);
		loop q over:all_sat_sorted {quantiles <+ statistic_profile(q);}
		
		// W vs M
		loop a over:target_agents { gender_sat <+ a.numerical(GENDER)::a._job_satisfaction; }
	
		// U-shaped age x sat
		age_distribution <- [];
		map<int,list<worker>> age_workers <- target_agents group_by (each.numerical(AGE));
		loop age over:age_workers.keys { age_distribution[age] <- statistic_profile(age_workers[age] collect (each._job_satisfaction)); }
		
		
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
	
	/*
	 * Returns age matrix for statistical test <\p>
	 * parameter c (int) is the inflection point, splitting age matrix below/above c (below inclusive)
	 */
	 pair<matrix<float>,matrix<float>> get_age_matrix(int c) {
	
	 	// Age matrix below c
		matrix bc <- (target_agents where (each.numerical(AGE) <= c)) 
			accumulate ([each.numerical(AGE),each._job_satisfaction])
			as_matrix {2,length(target_agents)};
		// Age matrix above c
		matrix ac <- (target_agents where (each.numerical(AGE) > c)) 
			accumulate ([each.numerical(AGE),each._job_satisfaction])
			as_matrix {2,length(target_agents)};
			
		return bc::ac;
	 }
}
