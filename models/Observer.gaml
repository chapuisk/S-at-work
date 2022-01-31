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
	list<pair<int,int>> AGE_RANGE_OBS <- list<pair<int, int>>([pair(0::25),pair(25::35),pair(35::45),pair(45::55),pair(55::65),pair(65::MAX_AGE)]);
		
	// INIT OBSERVER
	action init_observer {
		create observer with:[target_agents::list(worker),freq::1];
		main_observer <- first(observer);
		sats <- []; loop times:length(worker) {sats <+ [];}
	}
	
	// ########################
	// Job Satisfaction stability
	
	// Window of agent satisfaction
	list<list<float>> sats;
	
	// Satisfaction equilibrium
	reflex equilibrium {
		ask worker { list ls <- sats[int(self)]; ls <+ _job_satisfaction; if length(ls) > windows { sats[int(self)] <- ls copy_between (1,windows+1); } }
	}
	
	// stop the simulation if satisfaction does not move more than 'epsilon' in a 'windows' time frame for every agent
	// TODO : have a end cycle in case there is no equilibria state reached
	bool stop_sim(float epsilon <- EPSILON) { 
		bool stop <- sats none_matches (length(each) < windows or abs(min(each) - max(each)) > epsilon);
		if stop {
			ask main_observer {do end_outputs;}
			if batch_mode { 
				ask main_observer { do update_qSat; do update_aSat; do update_gSat; }
				s_index_batch <- sat_dist_index(main_observer);
				g_index_batch <- gender_pearson(main_observer);
				a_index_batch <- age_pseudo_two_lines_index(main_observer); 
				avr_sat_batch <- mean(worker collect (each._job_satisfaction));
				end_cycle_batch <- cycle;	
			}
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
		if obs.quantiles=nil or empty(obs.quantiles) {return -1.0;}
		float i; // the index
		
		list<float> i_percentiles;
		
		if std_based { i_percentiles <- obs.quantiles collect (each[MOMENTS index_of STD]); }
		else {i_percentiles <- obs.quantiles collect (
			each[MOMENTS index_of MIN] - each[MOMENTS index_of MAX]
		);}
		
		do syso(sample(i_percentiles),level::debug_levels[0]);
		
		// least deviatives percentile
		int idx_least_std <- (i_percentiles index_of (i_percentiles min_of abs(each)));
		do syso(sample(idx_least_std),level::debug_levels[0]);
		int idx_theoretic_least_std <- int(length(i_percentiles) * 0.75);
		
		// Peak should be close to 0.75 percentile
		i <- 1 - abs(idx_least_std / length(i_percentiles) - 0.75);
		
		// How std curve fits u-shape
		regression below_c <- nil;
		float pvb;
		if idx_least_std>0 {
			matrix<float> mat_b <- {2,idx_least_std+1} matrix_with 0.0;
			loop idx from:0 to:idx_least_std {mat_b[0,idx] <- idx; mat_b[1,idx] <- i_percentiles[idx];}
			do syso(sample(mat_b),level::debug_levels[0]);
			below_c <- build(mat_b);
			do syso(sample(below_c),level::debug_levels[0]);
			list b <- []; list bp <- [];
			loop r over:rows_list(mat_b) { b <+ last(r); bp <+ predict(below_c,[first(r)]); }
			pvb <- t_test(b,bp);
		}
		
		regression above_c <- nil;
		float pva;
		if length(i_percentiles)-idx_least_std>1 {
			matrix<float> mat_a <- {2,length(i_percentiles)-idx_least_std} matrix_with 0.0;
			loop idx from:0 to:length(i_percentiles)-idx_least_std-1 {mat_a[0,idx] <- idx; mat_a[1,idx] <- i_percentiles[idx+idx_least_std];}
			do syso(sample(mat_a),level::debug_levels[0]);
			above_c <- build(mat_a);
			do syso(sample(above_c),level::debug_levels[0]);
			list a <- []; list ap <- [];
			loop r over:rows_list(mat_a) { a <+ last(r); ap <+ predict(above_c,[first(r)]); }
			pva <- t_test(a,ap);
		}
		
		return i * (((below_c = nil or below_c.parameters[1] < 0 ? 1-pvb : 0.0) + (above_c = nil or above_c.parameters[1] > 0 ? 1-pvb : 0.0)))/2 ;
	} 
	
	// #########################
	// GENDER SATISFACTION
	
	/*
	 * Pearson's correlation coefficient
	 * - if it is positive, it means women (numerical code is 1) are more satisfied <p>
	 * - if it is negative, it means women are less satisfied <p>
	 */
	float gender_pearson(observer obs) {
		if obs.gender_sat=nil or empty(obs.gender_sat) {return 0.0;}
		return correlation(
			obs.gender_sat collect (each.key),
			obs.gender_sat collect (each.value)
		);
	}
	
	// #########################
	// AGE SATISFACTION DISTRIBUTION
	
	float age_pseudo_two_lines_index(observer obs, int c <- 55, int c_flat <- 5) {
		if not(obs.triggered) {return 0.0;}
		pair p <- obs.get_age_matrix(c); 
		regression below_c <- build(p.key);
		regression above_c <- build(p.value);
		
		float sb <- mean((worker where (each.numerical(AGE) >= c-c_flat 
			and each.numerical(AGE) <= c+c_flat)) collect (each._job_satisfaction));
		float sbm <- mean((worker where (each.numerical(AGE) < c-c_flat)) collect (each._job_satisfaction));
		float sbp <- mean((worker where (each.numerical(AGE) > c+c_flat)) collect (each._job_satisfaction));
		
		float res <- sb<sbm?(sb<sbp?1:0.5):0.0;
		
		list pbc <- []; list ebc <- [];
		loop r over:rows_list(p.key) { pbc <+ last(r); ebc <+ predict(below_c,[first(r)]); } 
		float pvalue_bc <- t_test(pbc,ebc);
		list pac <- []; list eac <- [];
		loop r over:rows_list(p.value) { pac <+ last(r); eac <+ predict(above_c,[first(r)]); }
		float pvalue_ac <- t_test(pac,eac);
		
		if first(below_c.parameters) < 0 { res <- res + 1 - pvalue_bc; }
		if first(above_c.parameters) > 0 { res <- res + 1 - pvalue_ac; }
		
		return res/3;
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
	map<string,list<float>> gSat <- GENDER.get_space() as_map (each::list_with(length(MOMENTS),0.0));
	
	// U-shape with age
	// each age = min,max,median,mean,std
	map<int,list<float>> age_distribution;
	// Age range average satisfaction
	map<pair<int,int>,float> aSat <- AGE_RANGE_OBS as_map (each::0.0);
	
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
		list<list<float>> all_sat_sorted <- partitions((target_agents collect (each._job_satisfaction)),q_number);
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
	 * Makes a partition of a given list of float into 'bins' sub list of (quasi) equal size
	 */
	list<list<float>> partitions(list<float> vals, int bins, bool ordered <- false) {
		list<list<float>> res;
		list<float> cloned_vals <- ordered ? copy(vals) : vals sort each;
		int bins_length <- int(length(vals) / bins);
		loop times:bins-1 {
			list<float> bin_vals <- cloned_vals copy_between (0,bins_length);
			res <+ bin_vals;
			cloned_vals >>- bin_vals;
		}
		res <+ cloned_vals;
		return res;
	}
	
	/*
	 * Return a given moment of quantiles value
	 */
	list<float> quantile_moment(list<float> values, string moment, int quanta <- q_number) {
		list<float> res <- [];
		list<list<float>> all_sat_sorted <- partitions(values, quanta);
		if length(all_sat_sorted) != quanta {error "Partitions operator return "+length(all_sat_sorted)+ " split, while asked for "+quanta;}
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
	
	action update_aSat(list<pair<int,int>> age_range <- AGE_RANGE_OBS) { 
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
