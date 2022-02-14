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
	
	// ----------------- //
	// WARR'S MODEL INIT //
	
	float WC_WEIGHT_CONSTANT <- 1.0;
	point WC_WEIGHT_DEFAULT <- {0,1}; // uniform draw min (x) and max (y)
	point WARR_MULTI_DEFAULT <- {1,1}; // truncated gaussian mean (x) and deviation (y)
	point WARR_BASELOG_DEFAULT <- {#e,0}; // truncated gaussian mean (x) and deviation (y)
	point WARR_NUTRIMENT_DEFAULT <- {1,5}; // uniform draw min (x) and max (y)
	
	// --------------------------- //
	// Work characteristic weights //
	
	float salary_weight <- WC_WEIGHT_CONSTANT;
	float working_time_weight <- WC_WEIGHT_CONSTANT;
	float contract_weight <- WC_WEIGHT_CONSTANT;
	float autonomy_weight <- WC_WEIGHT_CONSTANT;
	float interaction_weight <- WC_WEIGHT_CONSTANT;
	float intensity_weight <- WC_WEIGHT_CONSTANT;
	float meaningful_weight <- WC_WEIGHT_CONSTANT;
	
	/*
	 * list of warr's parameter model: <\br>
	 * idx[0] = work characteristic weight <\br>
	 * idx[1] = multiplicator of the log curve <\br>
	 * idx[2] = base of the log <\br>
	 * idx[3] = marginal decrement (if equal 0, vitamine, if superior to 0, nutriment)
	 * TODO: see in WARR's model to init work characteristic evaluation 
	 */
	list<float> work_eval(characteristic work_char, float weight <- WC_WEIGHT_CONSTANT) {
		switch work_char {
			match SALARY {return warr_mode(weight,0.0);} // Pure vitamine
			match WORKING_TIME {return warr_mode(weight,rnd(WARR_NUTRIMENT_DEFAULT.x,WARR_NUTRIMENT_DEFAULT.y));} // Nutriment
			match CONTRACT {return warr_mode(weight,0.0);}  // Pure vitamine
			match eijqi_autonomy {return warr_mode(weight,nutriment::rnd(WARR_NUTRIMENT_DEFAULT.x,WARR_NUTRIMENT_DEFAULT.y));} // Nutriment
			match eijqi_interaction {return warr_mode(weight,0.0);} // Pure vitamine
			match eijqi_intensity {return warr_mode(weight,nutriment::rnd(WARR_NUTRIMENT_DEFAULT.x,WARR_NUTRIMENT_DEFAULT.y));} // Nutriment
			match eijqi_meaningful {return warr_mode(weight,0.0);} // Pure vitamine
			default {return warr_mode(weight,flip(0.5)?0:rnd(WARR_NUTRIMENT_DEFAULT.x,WARR_NUTRIMENT_DEFAULT.y));} // Coin flip on vitamine or nutriment
		}
	}
	
	// Default nutriment behavior
	list<float> warr_mode(float weight,  float nutriment, float multiplicator <- -1, int base_log <- -1) {
		return [
			weight<=0?rnd(WC_WEIGHT_DEFAULT.x,WC_WEIGHT_DEFAULT.y):weight,
			multiplicator<=0?truncated_gauss(WARR_MULTI_DEFAULT.x,WARR_MULTI_DEFAULT.y):multiplicator,
			base_log<=0?truncated_gauss(WARR_BASELOG_DEFAULT.x,WARR_BASELOG_DEFAULT.y):base_log,
			nutriment
		];
	}
		
	// ---------------- //
	// PERSONALITY INIT //
	
	// Personality trait profile with BFI
	point minmax_o <- {10,50};
	point minmax_c <- {9,45};
	point minmax_e <- {8,40};
	point minmax_a <- {9,45};
	point minmax_n <- {8,40};
	
	/*
	 * TODO: find data to init personnality
	 */
	worker random_gaussian_personality(worker w) {
		w._o <- int(truncated_gauss(point(minmax_o.x+(minmax_o.y-minmax_o.x)/2.0,(minmax_o.y-minmax_o.x)/2.0)));
		w._c <- int(truncated_gauss(point(minmax_c.x+(minmax_c.y-minmax_c.x)/2.0,(minmax_c.y-minmax_c.x)/2.0)));
		w._e <- int(truncated_gauss(point(minmax_e.x+(minmax_e.y-minmax_e.x)/2.0,(minmax_e.y-minmax_e.x)/2.0)));
		w._a <- int(truncated_gauss(point(minmax_a.x+(minmax_a.y-minmax_a.x)/2.0,(minmax_a.y-minmax_a.x)/2.0)));
		w._n <- int(truncated_gauss(point(minmax_n.x+(minmax_n.y-minmax_n.x)/2.0,(minmax_n.y-minmax_n.x)/2.0)));
		return w;	
	}
	
	// --------------------------- //
	
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
	
	map<characteristic,string> _demographics;
	
	list<individual> relatives;
	list<individual> friends;
	
	// PERSONALITY //
	
	int _o; // openess = inventive/curious vs. consistent/cautious - 10 items
	int _c; // consienciousness = efficient/organized vs. extravagant/careless - 9 items
	int _e; // extraversion = outgoing/energetic vs. solitary/reserved - 8 items 
	int _a; // agreeableness = friendly/compassionate vs. critical/rational - 9 items 
	int _n; // neuroticism = sensitive/nervous vs. resilient/confident - 8 items
	
	// MODERATOR
	
	// -----------
	// Neuroticism
	
	// weight of neuroticism on cognitive aspect of negative evaluation
	float _rho_neuroticism_weight <- default_neu_rho;
	float rho_agg(float neuroticism_weight <- _rho_neuroticism_weight) { return _n / minmax_n.y * neuroticism_weight; } // How much bad experience is weighted upon work characteristic evaluation
	
	// ------------
	// Extraversion
	
	// TODO : put _e as an antecedent of the number of social references
	// Trying to compare with people with poor or high satisfaction
	float _ext_mode <- extra_selection;
	float extravert_social_ref(float extra <- _ext_mode) { return 1 - _ext_mode + (_e / minmax_e.y * extra); } // Threshold of difference to connect with people
	
	// ------------
	// Openess and agreeableness
	
	float _new_mode <- default_new_mode;
	float open_to_new_ideas(float open_agreeableness <- _new_mode) { return (_o / minmax_o.y + _a / minmax_a.y) / 2 * open_agreeableness; }
	
	// --------------------------- //
	// DEMOGRAPHIC CHARACTERISTICS //
	
	/*
	 * Get the corresponding demographic value
	 */
	string get(characteristic char) {return (_demographics contains_key char) ? _demographics[char] : "";}
	
	/*
	 * Get the corresponding demographic numerical value
	 */
	float numerical(characteristic char) { 
		if not(_demographics contains_key char) {error sample(self)+" call a wrong demographic variable: "+char.name;}
		return char.get_numerical_value(_demographics[char]);
	}
}

/*
 * Main active entity of the model, inherit from individual in all aspect related to personnality, demographics and social references
 */
species worker parent:individual {
	
	init {
		
		// HOMOGENEOUS INIT
		// --
		
		// length of memery
		__memory_length <- default_agent_memory;
		// Does agent update social referents?
		__update_social_references <- default_update_social_referents;
		// Number of social contacts for each work evaluation
		__nb_interactions_with_social_references <- default_nb_contacts;
		
		// HETEROGENEOUS INIT
	}
	
	// ----------------------------- //
	
	work my_work;
	map<characteristic, string> _work_aspects; // work aspect with there subjective value
	
	// ----------------------------- //
	
	// ATTITUDE TOWARD THE JOB //
	
	float _job_satisfaction;
	
	// PEAK-END KANHEMAN HEURISTICS
	int __memory_length; // how long do we recall freezed satisfaction
	list<float> __sat_memory;
	
	// Should be event based
	bool __update_work_eval -> every(step);
	
	/*
	 * First update the perception of work characteristics <br>
	 * ------------------- <br>
	 * For a perception to be updated, the perception should be removed from work aspects, e.g. during an emotional_event or organizational_event 
	 * <p>
	 */
	reflex update_work_characteristic_perception {
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		loop c over:WORK_CHARACTERISTICS where not(_work_aspects contains_key each) {
			switch c {
				match SALARY { _work_aspects[SALARY] <- string(my_work=nil?0:my_work.salary);}
				match WORKING_TIME { _work_aspects[WORKING_TIME] <- string(with_precision(my_work.working_time_per_week,2)); }
				match CONTRACT { _work_aspects[CONTRACT] <- string(my_work.contract); }
				default { _work_aspects[c] <- intrinsic_work_characteristics(c,my_work); }
			}
		}
		if DEBUG_WORKER and t != 0 {ask world { do syso(sample(myself._work_aspects),machine_time-t,myself,"update_work_characteristic_perception",debug_level(0)); }}
	}
	
	/*
	 * Secondly assess one emotional reaction to recent work events
	 */
	reflex update_emotion_resp {
		if not(empty(work_event)) { do emotional_response_to_work_events(); }
		else { if emotional_resp=0 { disable_emotion <- true; } else { emotional_resp <- emotional_resp * emotional_decay_ratio; } }
		work_event <- [];
	}
	
	/*
	 * Third evaluate work characteristics
	 */
	reflex update_cognitive_resp when:__update_work_eval {
		do socio_cognitive_evaluation_of_work(__update_social_references, __nb_interactions_with_social_references);
	}
	
	/*
	 * Finally assess one's attitude toward job
	 */
	reflex update_attitude_toward_job {
		if disable_emotion { emotional_balance <- 0.0; } else { emotional_balance <- emotional_balance_weigth();}
		_job_satisfaction <- (__sat_memory max_of (abs(each)) + _cognitive_resp) / 2 * (1 - emotional_balance) + emotional_resp * emotional_balance;
		if length(__sat_memory) = __memory_length { __sat_memory >- first(__sat_memory); }
		__sat_memory <+ _job_satisfaction;
	}

	// ---------- //
	// PERCEPTION //
	// ---------- //
	
	string intrinsic_work_characteristics(characteristic job_characteristic, work the_work) {
		string perception;
		switch job_characteristic {
			// match EIJQI_AUTONOMY { }
			// match EIJQI_INTENSITY { }
			// match EIJQI_INTERACTION { }
			// match EIJQI_MEANINGFUL { }
			default { 
				perception <- job_characteristic.perceived_as(rnd(PERCEPTION_VALENCE.key,PERCEPTION_VALENCE.value));
			}
		}
		return perception;
	}
	
	// --------- //
	// COGNITION //
	// --------- //
	
	float _cognitive_resp;
	
	/*
	 * Update cognitive work aspect
	 */
	action socio_cognitive_evaluation_of_work(bool update_social_ref <- true, int nb_interactions <- -1) {
		
		// Update evaluation criterion
		if empty(__social_references) or update_social_ref {__social_references <- update_social_references();}
		if nb_interactions <= 0 {do contrast_and_assimilation;}
		else {do contrast_and_assimilation(nb_interactions among __social_references);}
		
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		// Assess one job's characteristics impact based on warr's model
		map<characteristic, pair<float,float>> val_weights <- [];
		loop wc over:WORK_CHARACTERISTICS {
			val_weights[wc] <- get_warr_factor(wc, _work_aspects[wc], work_evaluator[wc][1], work_evaluator[wc][2], work_evaluator[wc][3])::work_evaluator[wc][0];
		}
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso(sample(val_weights),machine_time-t,myself,"warr's vitamin model",debug_level(0)); }
			t <- machine_time;
		}
		
		// Aggregate characteristics' impact vector into a single job impact
		_cognitive_resp <- wowa(val_weights);
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso(sample(myself._cognitive_resp),machine_time-t,myself,"WOWA",debug_level(0)); }
		}
	}
	
	// ---------------------------
	// Social relations
	
		
	// How to manage social referents to compare to
	bool __update_social_references; // upd_soc_ref
	int __nb_interactions_with_social_references;
	
	list<worker> __social_references;
	float org_kindship_factor <- 1.0; // How close from my point i evaluate people from my own organization - 1 is neutral value (linear over network distance)
	
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
		
		sr <- sr where (abs(_job_satisfaction - each._job_satisfaction) / (_job_satisfaction+EPSILON) <= extravert_social_ref());
		
		if DEBUG_WORKER and t != 0 { ask world { do syso(sample(sr),machine_time-t,myself,"update_social_references",debug_level(0)); } }
		return sr;
	}
	
	/*
	 * Compute how close current worker is in relation with another individual
	 * -
	 * Default implementation provide organization based relationship,i.e. how close jobs of the two individuals are
	 */
	float get_organizational_kinship(individual i) {
		float k;
		if relatives contains i or friends contains i {
			k <- 1.0;
		} else if type_of(i) is worker and my_work.org = worker(i).my_work.org {
			k <- 1 / ((my_work.org.get_distance(my_work,worker(i).my_work) + DEFAULT_HORIZONTAL_DISTANCE_UNIT=0?1:0 )^org_kindship_factor);
		} else {
			k <- 0.0;
		}
		return k;
	}
	
	// ---------------------
	// WARR's vitamine model
	
		
	// key is work carac and value is :
	// index 0 = weights of work caracteristic
	// index 1 = 'a' parameter of warr's vitamine/nutriment model
	// index 2 = 'b' parameter of warr's vitamine/nutriment model
	// index 3 = 'd' parameter of warr's vitamine/nutriment model
	map<characteristic, list<float>> work_evaluator;
	
	// slope of the log
	float a <- default_a;
	// base of the log 
	float b <- default_b;
	// decreasing factor
	float d <- default_d;
	
	/*
	 * basic function is: a * log_b(x + 1) * (tanh(-x*d) + e)
	 * <p>
	 * when d = 0 pure vitamine (no decrement) </br>
	 * when d > 0 nutriment behavior </br>
	 * when a [0;+inf] increase the curve's amplitude increase (sharper up and down)
	 * when 0 > d > 1 the tipping point increase (the more it is closer to 0)
	 * when d > 1 the tipping point decrease (for d=1, the equation returns 0 for x=1)
	 * 
	 */
	float get_warr_factor(characteristic wc, string value, float slope <- a, float base <- b, float decrease <- d){
		return slope * log(wc.get_numerical_value(value) + 1) / log(base) * (tanh(-wc.get_numerical_value(value)*decrease) + #e);
	}
	
	// ------------------------------------
	// ANCHOR FOR CONTRAST AND ASSIMILATION
	
	int nb_wc_exchange <- default_nb_wchar_ex;
	
	/*
	 * Get closer to evaluation criteria from assimilated individuals and away from contrasting ones
	 */
	action contrast_and_assimilation(list<worker> interact_with <- __social_references) {
		float t <- 0.0; map<worker,float> interact_magnitude;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		loop sr over:interact_with { 
			
			float assim <- get_anchor(sr, (kinship?get_organizational_kinship(sr):1.0));
			if 0.0 > assim or assim > 1.0 {
				ask world { do syso("Assimilation should be between 0 and 1", caller::myself, 
					action_name::"contrast_and_assimilation", level::last(debug_levels));
				}
			}
			
			list<characteristic> wcs <- _work_aspects.keys union sr._work_aspects.keys;
			wcs <- nb_wc_exchange=0 ? wcs : nb_wc_exchange among wcs;
			
			if t != 0.0 {interact_magnitude[sr] <- assim;}
			loop wc over:wcs {	
				list<float> mine <- work_evaluator[wc];
				list<float> yours <- sr.work_evaluator[wc];
				
				loop i from:1 to:length(mine)-1 {
					float diff <- mine[i] - yours[i];
					
					// TODO : discard if work evaluator dimension is too different
					if abs(diff) < mine[i]*assim*open_to_new_ideas() { mine[i] <- mine[i] + diff * (2 * assim - 1); } 
					
				}	
			}
		}
		
		if DEBUG_WORKER and t != 0.0 { 
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
		int nb_anchors <- length(_demographics);
		loop dc over:compare_to._demographics.keys {
			if close > dc.ambiguity and dc.compare_values(compare_to._demographics[dc],_demographics[dc])=0 {similarities <- similarities+1;}
		}
		list<characteristic> anchors <- compare_to._work_aspects.keys where (_work_aspects contains_key each and each.ambiguity < close );
		nb_anchors <- nb_anchors + length(anchors); 
		loop anchor over:anchors {
			  if anchor.compare_values(_work_aspects[anchor],compare_to._work_aspects[anchor]) = 0 {similarities <- similarities+1;}
		}
		return similarities / nb_anchors;
	}
	
	// -----------------------
	// W-OWA
	//
	// see : https://github.com/sorend/fuzzy4j/blob/master/src/main/java/fuzzy4j/aggregation/weighted/WeightedOWA.java
	
	//float rho; // [0,1] = 1 means 'at least one', while 0 means 'everything count' 
	float gamma <- default_gamma; // [0,1] the weights of weights
	
	/*
	 * 
	 */
	float wowa(map<characteristic, pair<float,float>> val_weights, float andness <- rho_agg(), float ww <- gamma) {
		
		if 0 > andness or andness > 1 {
			error sample(andness)+" should be "+(0>andness?"higher than 0":"lower than 1")
				+" -- See var "+sample(_n)+sample(minmax_n.y)+sample(_rho_neuroticism_weight);
		}
		
		// Weights given by value ordering, with andness parameter
		list<float> oWeights <- list_with(length(val_weights),0.0);
		if (andness = 1.0) { // handle border-case, rho = 1.0
            oWeights[length(oWeights)-1] <- 1.0;
        } else {
        	// calculate the two roots for rho
        	float t_m <- (-(andness - 0.5) - sqrt(((andness - 0.5) * (andness - 0.5)) - (4 * (andness - 1) * andness))) / (2 * (andness - 1));
        	float t_p <- (-(andness - 0.5) + sqrt(((andness - 0.5) * (andness - 0.5)) - (4 * (andness - 1) * andness))) / (2 * (andness - 1));
        	float t <- max(t_m, t_p);

        	float s <- 0.0;
        	loop i from:0 to:length(val_weights)-1 {
        		oWeights[i] <- t^i;
        		s <- s + oWeights[i]; 
        	}
        	loop i from:0 to:length(val_weights)-1 {
            	oWeights[i] <- oWeights[i] / s;
        	}
        }
        
        if abs(1.0 - sum(oWeights)) > EPSILON { ask world {do syso("Weights = "+oWeights,caller::myself,action_name::"wowa",level::last(debug_levels));} }
		
		list<characteristic> sorted_carac <- val_weights.keys sort_by (val_weights[each].key);
		sorted_carac <- reverse(sorted_carac);
		
		// The user defined weights
		list<float> uWeights <- sorted_carac collect (val_weights[each].value);
		uWeights <- uWeights collect (each/sum(uWeights));
		
		// TODO : Original library seems to work with [0;1] values (and weights)
		float max_val <- val_weights max_of (each.key);
		
		float s <- 0.0;
		if (andness >= 0.5) {
            float R_exp <- ww * ((2 * andness) - 1);
            loop v over:sorted_carac {
            	float I_R <- max_val - uWeights[sorted_carac index_of v] ^ R_exp * (max_val - val_weights[v].key);
                s <- s + oWeights[sorted_carac index_of v] * I_R;
            }
        }
        else {
            float R_exp <- ww * ((2 * (1 - andness)) - 1);
            loop v over:sorted_carac {
                float I_R <- max_val - uWeights[sorted_carac index_of v] ^ R_exp * val_weights[v].key;
                s <- s + oWeights[sorted_carac index_of v] * I_R;
            }
        }
        return s;
	}
	
	// ------- //
	// EMOTION //
	// ------- //
	
	float emotional_resp;
	bool disable_emotion;
	float emotional_decay_ratio <- 0.95;
	
	list<float> work_event;
	
	action emotional_response_to_work_events {
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		// TODO
		
		// See : http://koreascience.or.kr/article/JAKO201915658234951.page
		// * Extraversion will increase positive response
		// * Neuroticism will increase negative response
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso("empty emotional process",machine_time-t,myself,debug_level(0)); }
		}
	}
	
	float emotional_balance;
	
	/*
	 * How much emotional response is important in job satisfaction
	 */
	float emotional_balance_weigth(int openness_weight <- 3, float randomness <- rnd(1.0)) {
		return openness_weight=0?0.5:(_o / minmax_o.y * openness_weight + randomness) / (openness_weight+1); 
	}
	
}
