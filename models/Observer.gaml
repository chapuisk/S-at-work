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
	list<string> MOMENTS <- [MIN,MAX,MED,AVR,STD];
		
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
		if stop {
			ask main_observer {do end_outputs;}
			if batch_mode { ask main_observer { do update_qSat; do update_aSat; do update_gSat; } }
		}
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
	float sat_dist_index(observer obs, bool std_based <- true) {
		if obs.quantiles=nil or empty(obs.quantiles) {return -1;}
		float i; // the index
		
		list<float> i_percentiles;
		
		if std_based { i_percentiles <- obs.quantiles collect (each[MOMENTS index_of STD]); }
		else {i_percentiles <- obs.quantiles collect (
			each[MOMENTS index_of MIN] - each[MOMENTS index_of MAX]
		);}
		
		do syso(sample(i_percentiles));
		
		// least deviatives percentile
		int idx_least_std <- (i_percentiles index_of (i_percentiles min_of abs(each)));
		do syso(sample(idx_least_std));
		int idx_theoretic_least_std <- int(length(i_percentiles) * 0.75);
		
		// Peak should be close to 0.75 percentile
		i <- 1 - abs(idx_least_std / length(i_percentiles) - 0.75);
		
		// How std curve fits u-shape
		regression below_c <- nil;
		if idx_least_std>0 {
			matrix<float> mat_b <- {2,idx_least_std+1} matrix_with 0.0;
			loop idx from:0 to:idx_least_std {mat_b[0,idx] <- idx; mat_b[1,idx] <- i_percentiles[idx];}
			do syso(string(mat_b));
			regression below_c <- build(mat_b);
			do syso(sample(below_c));
		}
		
		regression above_c <- nil;
		if length(i_percentiles)-idx_least_std>1 {
			matrix<float> mat_a <- {2,length(i_percentiles)-idx_least_std} matrix_with 0.0;
			loop idx from:0 to:length(i_percentiles)-idx_least_std-1 {mat_a[0,idx] <- idx; mat_a[1,idx] <- i_percentiles[idx+idx_least_std];}
			do syso(string(mat_a));
			regression above_c <- build(mat_a);
			do syso(sample(above_c));
		}
		
		return i * ((below_c = nil or below_c.parameters[1] < 0 ? 0.5 : 0.0) + (above_c = nil or above_c.parameters[1] > 0 ? 0.5 : 0.0)) ;
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
		
		if below_c.parameters[1] < 0 { res <- res + 1; }
		if above_c.parameters[1] > 0 { res <- res + 2; }
		
		return res*(1-pvalue_bc)*(1-pvalue_ac)/2;
	}

}

species observer {
	
	list<worker> target_agents;
	list<float> overall_profile;
	
	// Job satisfaction distribution
	// each Q = min,max,median,mean,std
	list<list<float>> quantiles;
	// Flat quantile mean satisfaction
	list<float> qSat <- list_with(q_number,0.0);
	
	// Women vs men
	list<pair<int,float>> gender_sat;
	// MIN, AVR, MAX for each gender
	map<string,list<float>> gSat <- [];
	
	// U-shape with age
	// each age = min,max,median,mean,std
	map<int,list<float>> age_distribution;
	// Age range average satisfaction
	map<pair<int,int>,float> aSat <- [];
	
	// ------------------------------ //
	// REFLEXES

	int freq <- 1;

	/*
	 * Step wise output computation
	 * Update outputs
	 */
	reflex step_outputs when:not batch_mode and every(freq) { do update_qSat; do update_aSat; do update_gSat; }
	
	// ------------------------------ //
	// END SIM
	
	bool triggered <- false;
	
	/*
	 * The method that observe what happens in the simulation
	 */
	action end_outputs {
		triggered <- true;
		// Overall
		overall_profile <- statistic_profile(target_agents collect (each._job_satisfaction));
		
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
	
	// ------------------------------ //
	// UTILS
	
	/*
	 * Build the statistic profile
	 */
	list<float> statistic_profile(list<float> values, list<string> moments <- MOMENTS) {
		list<float> sp <- [];
		loop s over:moments {
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
	 * Return a given moment of quantiles value
	 */
	list<float> quantile_moment(list<float> values, string moment) {
		list<float> res <- [];
		if length(remove_duplicates(values)) < q_number {return list_with(q_number,0.0);}
		list<list<float>> all_sat_sorted <- list<list<float>>(values split_in q_number);
		loop q over:all_sat_sorted {res <+ first(statistic_profile(q,[moment]));}
		return res;
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
	 
	// ------------------------------ //
	// INNER ACTIONS
	
	action update_qSat { qSat <- quantile_moment(worker collect each._job_satisfaction,STD); }
	
	action update_aSat(list<pair<int,int>> age_range <- [pair(0::25),pair(25::35),pair(35::45),pair(45::55),pair(55::65),pair(65::MAX_AGE)]) { 
		loop k over:age_range {
			aSat[pair<int,int>(k)] <- mean(worker where (each.numerical(AGE) >= int(k.key) 
				and each.numerical(AGE) < int(k.value)) collect (each._job_satisfaction));
		}
	}
	
	action update_gSat {
		loop g over:GENDER.get_space() {
			gSat[g] <- statistic_profile( worker where (each.get(GENDER)=g) collect (each._job_satisfaction) );
		}
	}
}
