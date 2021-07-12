/**
* Name: Worker
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Worker

import "Work.gaml"
import "Global.gaml"

global {
	
	// Personality trait profile with BFI
	pair<int,int> minmax_o <- 10::50;
	pair<int,int> minmax_c <- 9::45;
	pair<int,int> minmax_e <- 8::40;
	pair<int,int> minmax_a <- 9::45;
	pair<int,int> minmax_n <- 8::40;
	
	list<worker> random_workforce_synthesis(int nb, float male_prop, map<point,float> age_distribution, 
		map<string,float> education_distribution, map<string,float> family_distribution
	) {
		if empty(WORK_CHARACTERISTICS) {error "Work characteristic should be initialze first";}
		create worker number:nb returns:work_force {
			demographics[GENDER] <- flip(male_prop)?string(GENDER.get_space()[0]):string(GENDER.get_space()[1]);
			point age_range <- rnd_choice(age_distribution);
			demographics[AGE] <- string(int(rnd(age_range.x,age_range.y)));
			demographics[EDUCATION] <- rnd_choice(education_distribution);
			demographics[FAMILY] <- rnd_choice(family_distribution);
			
			_o <- int(truncated_gauss(point(minmax_o.key+(minmax_o.value-minmax_o.key)/2.0,(minmax_o.value-minmax_o.key)/2)));
			_c <- int(truncated_gauss(point(minmax_c.key+(minmax_c.value-minmax_c.key)/2.0,(minmax_c.value-minmax_c.key)/2.0)));
			_e <- int(truncated_gauss(point(minmax_e.key+(minmax_e.value-minmax_e.key)/2.0,(minmax_e.value-minmax_e.key)/2.0)));
			_a <- int(truncated_gauss(point(minmax_a.key+(minmax_a.value-minmax_a.key)/2.0,(minmax_a.value-minmax_a.key)/2.0)));
			_n <- int(truncated_gauss(point(minmax_n.key+(minmax_n.value-minmax_n.key)/2.0,(minmax_n.value-minmax_n.key)/2.0)));
			
			loop c over:WORK_CHARACTERISTICS {
				work_evaluator[c] <- [rnd(0.0,1.0),truncated_gauss(1.0,1.0),gauss(#e,0.5),flip(0.5)?0:rnd(1.0,5.0)];
			} 
			
		} 
		return work_force;
	}
	
	/*
	 * DEBUG MODE FOR WORKERS
	 */
	bool DEBUG_WORKER <- true;
	float DEBUG_PROP <- 1.0;
	list<worker> DEBUG_TARGET <- [];
	
	// LOCAL INIT
	init { if DEBUG_PROP > 0.0 and empty(DEBUG_TARGET) { DEBUG_TARGET <- int(DEBUG_PROP * length(worker)) among worker; } }
	
}

species individual virtual:true {
	
	map<characteristic,string> demographics;
	
	list<individual> relatives;
	list<individual> friends;
	
	// PERSONALITY //
	
	int _o; // openess = inventive/curious vs. consistent/cautious - 10 items
	int _c; // consienciousness = efficient/organized vs. extravagant/careless - 9 items
	int _e; // extraversion = outgoing/energetic vs. solitary/reserved - 8 items 
	int _a; // agreeableness = friendly/compassionate vs. critical/rational - 9 items 
	int _n; // neuroticism = sensitive/nervous vs. resilient/confident - 8 items
	
	// -------------
	
}

species worker parent:individual {
	
	work my_work;
	map<characteristic, string> work_aspects; // work aspect with there subjective value
	
	// ----------------------------- //
	
	// ATTITUDE TOWARD THE JOB //
	
	float job_satisfaction;
	
	// PEAK-END KANHEMAN HEURISTICS
	int memory_length <- 1; // how long do we recall freezed satisfaction
	list<float> sat_memory;
	float sat_peak <- #infinity;
	
	bool update_work_eval <- every(step);
	bool update_social_references <- false;
	int nb_interactions_with_social_references <- 1;
	
	/*
	 * First update the perception of work characteristics <br>
	 * ------------------- <br>
	 * For a perception to be updated, the perception should be removed from work aspects, e.g. during an emotional_event or organizational_event 
	 * <p>
	 */
	reflex update_work_characteristic_perception {
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		loop c over:WORK_CHARACTERISTICS {
			switch c {
				match SALARY {work_aspects[SALARY] <- string(my_work=nil?0:my_work.salary);}
				match WORKING_TIME {
					if not(work_aspects contains_key WORKING_TIME) { 
						work_aspects[WORKING_TIME] <- string(with_precision(my_work.working_time_per_week/#h,2));
					}
				}
				match CONTRACT {
					if not(work_aspects contains_key CONTRACT) {  work_aspects[CONTRACT] <- string(my_work.contract); }
				}
				default {
					if not(work_aspects contains_key c) { work_aspects[c] <- perceive_work_characteristics(c,my_work);}
				}
			}
		}
		if DEBUG_WORKER and t != 0 {ask world { do syso(sample(myself.work_aspects),machine_time-t,myself,"update_work_characteristic_perception",debug_level(0)); }}
	}
	
	/*
	 * Secondly assess one emotional reaction to recent work events
	 */
	reflex update_emotion_resp when:not(empty(work_event)) {
		do emotional_response_to_work_events();
		work_event <- [];
	}
	
	/*
	 * Third evaluate work characteristics
	 */
	reflex update_cognitive_resp when:update_work_eval {
		do socio_cognitive_evaluation_of_work(update_social_references, nb_interactions_with_social_references);
	}
	
	/*
	 * Finally assess one's attitude toward job
	 */
	reflex update_attitude_toward_job {
		if length(sat_memory) = memory_length { sat_memory >- last(sat_memory); } 
		sat_memory <+ cognitive_resp + emotional_resp;
		list<float> peakend <- sat_memory; if sat_peak!=#infinity { peakend <+ sat_peak; }  
		job_satisfaction <- mean(sat_memory);
		if sat_peak=#infinity or abs(sat_peak) < abs(job_satisfaction) { sat_peak <- job_satisfaction;}
	}

	// ---------- //
	// PERCEPTION //
	// ---------- //
	
	float task_identity_exigence <- 1.0;
	
	string perceive_work_characteristics(characteristic job_characteristic, work the_work) {
		string perception;
		switch job_characteristic {
			match task_identity { return task_identity.perceived_as((1 - 1 / (length(the_work.tasks)^task_identity_exigence)) * 2 - 1); }
			// match task_significance { }
			// match skill_variety { }
			// match autonomy { }
			// match feedback { }
			// match job_security { }
			// match advancement_op  { }
			// match interesting_job { }
	 		// match work_independently { }
			// match help_people { }
			// match useful_to_society { }
			// match rl_with_manager { }
			// match rl_with_colleague { }
			default { 
				perception <- job_characteristic.perceived_as(rnd(PERCEPTION_VALENCE.key,PERCEPTION_VALENCE.value));
			}
		}
		return perception;
	}
	
	// --------- //
	// COGNITION //
	// --------- //
	
	float cognitive_resp;
	
	/*
	 * Update cognitive work aspect
	 */
	action socio_cognitive_evaluation_of_work(bool update_social_ref <- true, int nb_interactions <- -1) {
		
		// Update evaluation criterion
		if empty(social_references) or update_social_ref {social_references <- update_social_references();}
		if nb_interactions <= 0 {do contrast_and_assimilation;}
		else {do contrast_and_assimilation(nb_interactions among social_references);}
		
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		// Assess one job's characteristics impact based on vitamin model
		map<characteristic, pair<float,float>> val_weights <- [];
		loop wc over:WORK_CHARACTERISTICS {
			val_weights[wc] <- get_warr_factor(wc,work_aspects[wc], work_evaluator[wc][1], work_evaluator[wc][2], work_evaluator[wc][3])::work_evaluator[wc][0];
		}
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso(sample(val_weights),machine_time-t,myself,"warr's vitamin model",debug_level(0)); }
			t <- machine_time;
		}
		
		// Aggregate characteristics' impact vector into a single job impact
		cognitive_resp <- wowa(val_weights);
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso(sample(myself.cognitive_resp),machine_time-t,myself,"WOWA",debug_level(0)); }
		}
	}
	
	// ---------------------------
	// Social relations
	
	list<worker> social_references;
	float org_kindship_factor <- 1.0;
	
	/*
	 * Update the list of social references
	 */
	list<worker> update_social_references(float family_prop <- 1.0, float friends_prop <- 1.0, 
		bool organizational_kinship <- false, float colleagues_factor <- 1.0
	) {
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		list<worker> sr;
		// Add family
		list<worker> fam <- empty(relatives)?[]:relatives collect worker(each);
		loop f over:fam {if flip(family_prop) {sr <+ f;}}
		// Add friends
		list<worker> fri <- empty(friends)?[]:friends collect worker(each);
		loop f over:fri {if flip(friends_prop) {sr <+ f;}}
		// Add colleague
		loop e over:my_work.org.workers-self { 
			if organizational_kinship {if flip(get_organizational_kinship(e)) {sr <+ e;}}
			else if flip(colleagues_factor) {sr <+ e;}
		}
		// -------
		
		if DEBUG_WORKER and t != 0 { ask world { do syso(sample(sr),machine_time-t,myself,"update_social_references",debug_level(0)); } }
		return sr;
	}
	
	/*
	 * Compute how close current worker is in relation with another individual
	 * -
	 * Default implementation provide organization based relationship,i.e. how close jobs of the two individuals are
	 */
	float get_organizational_kinship(individual i) {
		float kinship;
		if type_of(i) is worker and my_work.org = worker(i).my_work.org {
			kinship <- 1 / ((my_work.org.get_distance(my_work,worker(i).my_work) + DEFAULT_HORIZONTAL_DISTANCE_UNIT=0?1:0 )^org_kindship_factor);
		} else {
			kinship <- 0.0;
		}
		return kinship;
	}
	
	// ---------------------
	// WARR's vitamine model
	
		
	// key is work carac and value is :
	// index 0 = weights of work caracteristic
	// index 1 = 'a' parameter of warr's vitamine model
	// index 2 = 'b' parameter of warr's vitamine model
	// index 3 = 'd' parameter of warr's vitamine model
	map<characteristic, list<float>> work_evaluator;
	
	// slope of the log
	float a <- 1.0;
	// base of the log 
	float b <- #e;
	// decreasing factor
	float d <- 0.0;
	
	/*
	 * basic function is a * log_b(x+1) * (tanh(x*d)+1)
	 * <p>
	 * when d = 0 pure vitamine (no decrement) </br>
	 * when d > 0 nutriment behavior </br>
	 * 
	 */
	float get_warr_factor(characteristic wc, string value, float slope <- a, float base <- b, float decrease <- d){
		return slope * log(wc.get_numerical_value(value) + 1) / log(base) * (tanh(-wc.get_numerical_value(value)*decrease) + 1);
	}
	
	// ------------------------------------
	// ANCHOR FOR CONTRAST AND ASSIMILATION
	
	int nb_wc_exchange <- 1;
	
	/*
	 * Get closer to evaluation criteria from assimilated individuals and away from contrasting ones
	 */
	action contrast_and_assimilation(list<worker> interact_with <- social_references) {
		float t; map<worker,float> interact_magnitude;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		loop sr over:interact_with { 
			
			float assim <- get_anchor(sr, get_organizational_kinship(sr));
			list<characteristic> wcs <- nb_wc_exchange among (work_aspects.keys union sr.work_aspects.keys);
			
			if t != 0 {interact_magnitude[sr] <- assim;}
			loop wc over:wcs {	
				list<float> mine <- work_evaluator[wc];
				list<float> yours <- sr.work_evaluator[wc];
				
				loop i from:1 to:length(mine)-1 {
					float diff <- mine[i] - yours[i];
					mine[i] <- mine[i] + diff * (2 * assim - 1);
				}	
			}
		}
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso(sample(interact_magnitude),machine_time-t,myself,"contrast_and_assimilation",debug_level(0)); }
		}
	}
	
	/*
	 * Get similarity distance with another worker based on how close i am with him (impacting the knowledge, I invovle in anchoring)
	 * <p>
	 * H1 : the current hypothesis made is based on how close people are compare to the ambiguity level, like close > ambiguity then compare the 2 values </br>
	 * TODO H2 : 
	 * people make a comparison with what they know from other, i.e. a vector of work characteristic that should be constant over time except if new information arises. This
	 * means construct a refined version of social_references (i.e. a map that bound workers, with known - biased - list of characteristics) 
	 * 
	 */
	float get_anchor(worker compare_to, float close) {
		float similarities;
		int nb_anchors <- length(demographics);
		loop dc over:compare_to.demographics.keys {
			if close > dc.ambiguity and dc.compare_values(compare_to.demographics[dc],demographics[dc])=0 {similarities <- similarities+1;}
		}
		list<characteristic> anchors <- compare_to.work_aspects.keys where ( each.ambiguity < close );
		nb_anchors <- nb_anchors + length(anchors); 
		loop anchor over:anchors {
			if work_aspects contains_key anchor {
				if anchor.compare_values(work_aspects[anchor],compare_to.work_aspects[anchor]) = 0 {similarities <- similarities+1;}
			}
		}
		return similarities / nb_anchors;
	}
	
	// -----------------------
	// W-OWA
	//
	// see : https://github.com/sorend/fuzzy4j/blob/master/src/main/java/fuzzy4j/aggregation/weighted/WeightedOWA.java
	
	float rho <- 0.0; // [0,1] = 1 means 'at least one', while 0 means 'everything count' 
	float gamma <- 1.0; // [0,1] the weights of weights
	
	float wowa(map<characteristic, pair<float,float>> val_weights, float andness <- rho, float ww <- gamma) {
		
		list<float> weights <- list_with(length(val_weights),0.0);
		if (rho = 1.0) { // handle border-case, rho = 1.0
            weights[length(weights)-1] <- 1.0;
        } else {
        	// calculate the two roots for rho
        	float t_m <- (-(rho - 0.5) - sqrt(((rho - 0.5) * (rho - 0.5)) - (4 * (rho - 1) * rho))) / (2 * (rho - 1));
        	float t_p <- (-(rho - 0.5) + sqrt(((rho - 0.5) * (rho - 0.5)) - (4 * (rho - 1) * rho))) / (2 * (rho - 1));
        	float t <- max(t_m, t_p);

        	float s <- 0.0;
        	loop i from:0 to:length(val_weights)-1 {
        		weights[i] <- t^i;
        		s <- s + weights[i]; 
        	}
        	loop i from:0 to:length(val_weights)-1 {
            	weights[i] <- weights[i] / s;
        	}
        }
		
		list<characteristic> sorted_carac <- val_weights.keys sort_by (val_weights[each].key);
		sorted_carac <- reverse(sorted_carac);
		float s <- 0.0;
		if (rho >= 0.5) {
            float R_exp <- gamma * ((2 * rho) - 1);
            loop v over:sorted_carac {
            	float I_R <- 1 - val_weights[v].value ^ R_exp * (1 - val_weights[v].key);
                s <- s + weights[sorted_carac index_of v] * I_R;
            }
        }
        else {
            float R_exp <- gamma * ((2 * (1 - rho)) - 1);
            loop v over:sorted_carac {
                float I_R <- 1 - val_weights[v].value ^ R_exp * val_weights[v].key;
                s <- s + weights[sorted_carac index_of v] * I_R;
            }
        }
        return s;
	}
	
	// ------- //
	// EMOTION //
	// ------- //
	
	float emotional_resp;
	
	list<float> work_event;
	
	action emotional_response_to_work_events {
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		// TODO
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso("empty emotional process",machine_time-t,myself,debug_level(0)); }
		}
	}
	
}
