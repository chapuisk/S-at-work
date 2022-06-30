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
	
	// --------------------------- //
	// Work characteristic weights //
	
	point WC_WEIGHT_DEFAULT <- {0,1}; // uniform draw min (x) and max (y)
	
	
	float salary_weight <- 4.363e-6;
	float working_time_weight <- 0.01;
	float contract_weight <- EPSILON;
	float autonomy_weight <- 0.0714;
	float interaction_weight <- 0.1622;
	float intensity_weight <- 0.0636;
	float meaningful_weight <- 0.2048;
	
	// ----------------- //
	// WARR'S MODEL INIT //
	
	int WARR_MULTI_DEFAULT <- 5; // truncated gaussian mean (x) and deviation (y)
	float BASELOG_DEFAULT <- #e;
	int NUTRIMENT_BETA <- 5; // How much high value can arose from lognormal nutriment
	
	float _working_time_warr_mod <- 35.0; // Expected time for warr's model overshoot 
	float _autonomy_warr_mod <- 10.0; // Expected level of autonomy for warr's model overshoot
	float _intensity_warr_mod <- 8.0;
	
	/*
	 * list of warr's parameter model: <\br>
	 * idx[0] = work characteristic weight <\br>
	 * idx[1] = multiplicator of the log curve - i.e. 'a' in ODD <\br>
	 * idx[2] = base of the log - default e based log <\br>
	 * idx[3] = marginal decrement (if equal 0, vitamine, if superior to 0, nutriment) - i.e. 'd' in ODD
	 * TODO: see in WARR's model to init work characteristic evaluation 
	 */
	list<float> work_eval(characteristic work_char, float weight <- -1) {
		switch work_char {
			match SALARY {return warr_mode(salary_weight,0.0);} // Pure vitamine
			match WORKING_TIME {return warr_mode(working_time_weight,warr_nutriment(_working_time_warr_mod));} // Nutriment
			match CONTRACT {return warr_mode(contract_weight,0.0);}  // Pure vitamine
			match eijqi_autonomy {return warr_mode(autonomy_weight,warr_nutriment(_autonomy_warr_mod));} // Nutriment
			match eijqi_interaction {return warr_mode(interaction_weight,0.0);} // Pure vitamine
			match eijqi_intensity {return warr_mode(intensity_weight,warr_nutriment(_intensity_warr_mod));} // Nutriment
			match eijqi_meaningful {return warr_mode(meaningful_weight,0.0);} // Pure vitamine
			default {return warr_mode(weight,flip(0.5)?0:warr_nutriment(#e));} // Coin flip on vitamine or nutriment
		}
	}
	
	// Way to format and compute warr parameters - 
	list<float> warr_mode(float weight,  float nutriment, float multiplicator <- -1, int base_log <- BASELOG_DEFAULT) {
		return [
			weight<=0?rnd(WC_WEIGHT_DEFAULT.x,WC_WEIGHT_DEFAULT.y):weight,
			multiplicator<=0?truncated_gauss(WARR_MULTI_DEFAULT,WARR_MULTI_DEFAULT):multiplicator,
			base_log, nutriment
		];
	}
	
	/*
	 * Compute mu accordinng to expected value from lognormal (esperance ou mod de la fonction de densité)
	 */
	float warr_nutriment(float mod, float sigma <- 1/NUTRIMENT_BETA) { 
		return lognormal_rnd(log(#e/(mod*1.5+#e)),sigma);
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
	bool DEBUG_WORKER <- false;
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
species worker parent:individual schedules:shuffle(worker) {
	
	init {
		
		// HOMOGENEOUS INIT
		// --
		
		// SUB-MODELS
		__update_work_eval_criterias <- warrMod?socCompMod:false;
		
		// length of memery
		__memory_length <- default_agent_memory;
		// Forget probability
		__refresh_memory_proba <- default_probability_to_forget;
		// Does agent update social referents?
		__update_social_references <- default_update_social_referents;
		// Number of social contacts for each work evaluation
		__nb_interactions_with_social_references <- default_nb_contacts;
		// Fake job satisfaction
		_job_satisfaction <- 1.0;
		
		// HETEROGENEOUS INIT
	}
	
	// ----------------------------- //
	
	work my_work;
	map<characteristic, string> _work_aspects; // work aspect with there subjective value
	
	// ----------------------------- //
	
	// Satisfaction from Data
	int __declared_sat;
	
	// ATTITUDE TOWARD THE JOB //
	
	float _job_satisfaction;
	
	// PEAK-END KANHEMAN HEURISTICS
	int __memory_length; // how long do we recall freezed satisfaction
	float __refresh_memory_proba;
	list<float> __sat_memory;
	float peak_memory <- -1.0;
	
	// Should be event based
	bool __update_work_eval -> every(step);
	bool __update_work_eval_criterias;
	
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
		do socio_cognitive_evaluation_of_work(__update_work_eval_criterias, __update_social_references, __nb_interactions_with_social_references);
	}
	
	/*
	 * Finally assess one's attitude toward job
	 */
	reflex update_attitude_toward_job {
		// elicit emotions
		if disable_emotion { emotional_balance <- 0.0; } else { emotional_balance <- emotional_balance_weigth();}
		
		// Elaborate on peak end heuristic or not
		float cog <- peakEndMod ? ((peak_memory<0?_cognitive_resp:peak_memory) + _cognitive_resp) / 2 : _cognitive_resp;
		
		// compute attitude
		_job_satisfaction <- cog * (1 - emotional_balance) + emotional_resp * emotional_balance;
	}
	
	/*
	 * Last update memory
	 */
	reflex update_memory {
		// Memory process
		if length(__sat_memory) = __memory_length and flip(__refresh_memory_proba) { 
			__sat_memory >- first(__sat_memory);
		}
		__sat_memory <+ _job_satisfaction;
		
		// Revise peak memory
		float current_peak_memory <- __sat_memory max_of (abs(each - _job_satisfaction));
		if abs(current_peak_memory-_job_satisfaction)>abs(peak_memory-_job_satisfaction) {peak_memory <- current_peak_memory;}
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
	action socio_cognitive_evaluation_of_work(bool update_eval_criterias <- true, bool update_social_ref <- true, int nb_interactions <- -1) {
		if (update_eval_criterias) {do update_evaluation_criterias(update_social_ref,nb_interactions);}
		float t;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
		
		// Assess one job's characteristics impact based on warr's model
		map<characteristic, pair<float,float>> val_weights <- [];
		loop wc over:WORK_CHARACTERISTICS {
			if warrMod {
				val_weights[wc] <- get_warr_factor(wc, _work_aspects[wc], work_evaluator[wc][1], work_evaluator[wc][2], work_evaluator[wc][3])::work_evaluator[wc][0];
			} else {
				val_weights[wc] <- wc.get_numerical_value(_work_aspects[wc])::work_evaluator[wc][0];
			}
		}
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso(sample(val_weights),machine_time-t,myself,"warr's vitamin model",debug_level(0)); }
			t <- machine_time;
		}
		
		// Aggregate characteristics' impact vector into a single job impact
		_cognitive_resp <- wowaMod ? wowa(val_weights) : wa(val_weights);
		
		if DEBUG_WORKER and t != 0 { 
			ask world { do syso(sample(myself._cognitive_resp),machine_time-t,myself,"WOWA",debug_level(0)); }
		}
	}
		
	action update_evaluation_criterias(bool update_social_ref, int nb_interactions) {
		if empty(__social_references) or update_social_ref {__social_references <- update_social_references();}
		if nb_interactions <= 0 {do contrast_and_assimilation;}
		else {do contrast_and_assimilation(nb_interactions among __social_references);}
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
	 * basic function is: a * log_b(x + 1) * (tanh(-x*d) + b)
	 * <p>
	 * when d = 0 pure vitamine (no decrement) </br>
	 * when d > 0 nutriment behavior </br>
	 * when a [0;+inf] increase the curve's amplitude increase (sharper up and down)
	 * when 0 > d > 1 the tipping point increase (the more it is closer to 0)
	 * when d > 1 the tipping point decrease (for d=1, the equation returns 0 for x=1)
	 * 
	 */
	float get_warr_factor(characteristic wc, string value, float slope <- a, float base <- b, float decrease <- d){
		return slope * log(wc.get_numerical_value(value) + 1) / log(base) * (tanh(-wc.get_numerical_value(value)*decrease) + base);
	}
	
	// ------------------------------------
	// ANCHOR FOR CONTRAST AND ASSIMILATION
	
	int nb_wc_exchange <- default_nb_wchar_ex;
	
	float contrast <- default_contrast_effect;
	
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
			
			float t1 <- machine_time;
			if t != 0.0 {interact_magnitude[sr] <- assim;}
			loop wc over:(nb_wc_exchange=0 ? _work_aspects.keys : nb_wc_exchange among _work_aspects.keys) {	
				list<float> mine <- copy(work_evaluator[wc]);
				list<float> yours <- sr.work_evaluator[wc];
				
				float t2 <- machine_time;
				loop i from:1 to:length(mine)-1 {
					float val <- mine[i]; float diff <- val - yours[i];
					
						// Bound confidence
						
						if val=0?abs(diff)<open_to_new_ideas():abs(diff)/val<open_to_new_ideas(){
							work_evaluator[wc][i] <- val + diff * ((1+contrast) * assim - contrast);
						}
					
				}
				if DEBUG_WORKER {ask world { do syso("BC",machine_time-t2,myself,"BC",debug_level(0));}}	
			}
			if DEBUG_WORKER {ask world {do syso("WC",machine_time-t1,myself,"WC",debug_level(0));}}
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
		float t <- 0.0;
		if DEBUG_WORKER and DEBUG_TARGET contains self {t <- machine_time;}
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
		if DEBUG_WORKER and t != 0.0 { 
			ask world { do syso("Achor process",machine_time-t,myself,"get_anchor",debug_level(0)); }
		}
		return similarities / nb_anchors;
	}
	
	// -----------------------
	// W-OWA
	//
	
	//float rho; // [0,1] = 1 means 'at least one', while 0 means 'everything count' 
	float gamma <- default_gamma; // [0,1] the weights of weights
	
	/*
	 * Weighted ordered weighted average based on implementation:
	 * https://github.com/sorend/fuzzy4j/blob/master/src/main/java/fuzzy4j/aggregation/weighted/WeightedOWA.java
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
	
	/*
	 * Simple weighted average
	 */
	float wa(map<characteristic, pair<float,float>> val_weights) {
		return mean(val_weights.values collect (each.key * each.value));
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
