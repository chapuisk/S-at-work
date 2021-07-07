/**
* Name: Demography
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags:
* 
* All metadata have been pull from Eurostat: https://ec.europa.eu/eurostat/ramon/index.cfm?TargetUrl=DSP_PUB_WELC
*  
*/


model Characteristic

global {
	
	pair<float,float> PERCEPTION_VALENCE <- -1.0::1.0;
	
	float LOW_AMBIGUITY <- 0.1;
	float MIDDLE_AMBIGUITY <- 0.3;
	float HIGH_AMBIGUITY <- 0.6;
	
	characteristic AGE;
	characteristic GENDER;
	characteristic EDUCATION;
	characteristic FAMILY;
	
	action init_demographic_characteristic {
		create num_characteristic returns:ca with:[name::"age",gama_type::"int", ambiguity::MIDDLE_AMBIGUITY, min::0, max::120]; AGE <- first(ca);
		create nominal_characteristic returns:cg with:[name::"gender",gama_type::"string", ambiguity::LOW_AMBIGUITY, values::["M","W","O"]]; GENDER <- first(cg);
		create ordered_characteristic returns:ce with:[name::"education",gama_type::"string", ambiguity::MIDDLE_AMBIGUITY, values::[
																				"No education completed",
																				"Primary or lower secondary education",
																				"Upper secondary or post-secondary education",
																				"Tertiary education"]]; EDUCATION <- first(ce);
		create nominal_characteristic returns:cf with:[name::"family",gama_type::"string", ambiguity::MIDDLE_AMBIGUITY, values::[
																				"Single adult without children",
																				"Single adult with children",
																				"Couple without children",
																				"Couple with children",
																				"Other type of household with children",
																				"Other type of household without children"
																				]]; FAMILY <- first(cf);
		
	}
	
}

/*
 * Abstract representation of a caracteristic, most important aspect is comparison between caracteristic
 */
species characteristic virtual:true {
	string gama_type among:["string", "int", "float", "date", "bool"];
	float ambiguity;
	list get_space virtual:true;
	
	// Share a characteristic value considering a given information level and ambiguity level of the characteristic
	string share_value(string val, float information_level <- 1.0) virtual:true;
	// Get a numerical representation of the value
	float get_numerical_value(string val) virtual:true;
	// Compare two values
	int compare_values(string val1, string val2) virtual:true;
	// Gives back a value for a perceived valence with PERCEIVED_VALENCE range
	string perceived_as(float valence, list frame <- PERCEPTION_VALENCE) virtual:true;
}

// Numerical caracteristic
species num_characteristic parent:characteristic {
	
	float min;
	float max;
	list get_space {return [min,max];}
	
	float equality_range;
	
	float get_numerical_value(string val) {
		switch gama_type {
			match "int" {return int(val);}
			match "float" {return float(val);}
			default {error "Numerical work caracteristic should be int or float rather than "+gama_type;}
		}
	}
	
	string share_value(string val, float information_level <- 1.0) {
		float il <- (information_level + 1 - ambiguity)/2;
		if (information_level=1.0) or flip(il) {return val;}
		float num_val <- get_numerical_value(val);
		string res;
		if gama_type = "int" {
			pair<int,int> lh <- round(num_val - num_val * il) :: round(num_val + num_val * il);
			res <- string(rnd(lh.key,lh.value));
		}
		else {
			pair<float,float> lh <- num_val - num_val * il :: num_val + num_val * il;
			res <- string(with_precision(rnd(lh.key,lh.value),2));
		} 
		
		return res; 
	}
	
	int compare_values(string val1, string val2) { 
		float diff <- get_numerical_value(val1) - get_numerical_value(val2);
		if abs(diff) < equality_range {return 0;}
		return diff;
	}
	
	string perceived_as(float valence, list frame <- PERCEPTION_VALENCE) {
		if length(frame) = 2 and frame all_match (is_number(each)) {
			float min_valence <- float(first(frame));
			float max_valence <- float(last(frame));
			return string( (max - min) * valence / (max_valence - min_valence) );
		}
		error "Perception with frame "+frame+" is not yet implemented";
	}
}

// Ordered caracteristic
species ordered_characteristic parent:characteristic {
	
	list<string> values;
	list get_space {return values;}
	
	float get_numerical_value(string val) {
		if not(values contains val) {error "Ask for an unknown value "+val+" : "+values;}
		return values index_of val;
	}
	
	string share_value(string val, float information_level <- 1.0) {
		if (information_level=1.0 and values contains val) or flip((information_level + 1 - ambiguity)/2) {return val;}
		int idx <- values index_of val;
		list range <- [val];
		if idx=0 {range <+ values[idx+1]; if length(values)>2 {range <+ values[idx+2];}}
		else if val=last(values) {range <+ values[idx-1]; if length(values)>2 {range <+ values[idx-2];}}
		return any(range);
	}
	
	int compare_values(string val1, string val2) {
		float diff <- get_numerical_value(val1) - get_numerical_value(val2);
		return diff < 0.0 ? -1 : (diff > 0.0 ? 1 : 0);
	}
	
	string perceived_as(float valence, list frame <- PERCEPTION_VALENCE) {
		if empty(frame) {error "Cannot ask a perceived value without a frame";}
		if length(frame) = 2 and is_number(string(first(frame))) and is_number(string(last(frame))) {
			float min_valence <- float(first(frame));
			float max_valence <- float(last(frame));
			return values[ round(length(values) * (valence - min_valence) / (max_valence - min_valence)) ];
		}
		if values contains_all frame {
			return string(frame[
				round((valence - PERCEPTION_VALENCE.key) / (PERCEPTION_VALENCE.value - PERCEPTION_VALENCE.key) * length(frame))
			]);
		}
		error "Perception with frame "+frame+" is not yet implemented";
	}
}

// Nominal caracteristic
species nominal_characteristic parent:characteristic {
	
	list<string> values;
	list get_space {return values;}
	
	float get_numerical_value(string val) {
		if not(values contains val) {error "Ask for an unknown value "+val+" : "+values;}
		return 1;
	}
	
	string share_value(string val, float information_level <- 1.0) {
		float il <- (information_level + 1 - ambiguity)/2;
		if (information_level=1.0 and values contains val) or flip(il) {return val;}
		float knowledge <- length(values)*il;
		int range <- int(knowledge) + (flip(knowledge-int(knowledge))?1:0);
		return any( (range-1) among (values-val) + val );
	}
	
	int compare_values(string val1, string val2) { return val1=val2?0:1; }
	
	string perceived_as(float valence, list frame <- PERCEPTION_VALENCE) {
		if frame all_match (is_number(each)) {error "Should not be asked for a perceived value based on numerical valence with nominal characteristics";}
		if values contains_all frame {
			return string(frame[
				round((valence - PERCEPTION_VALENCE.key) / (PERCEPTION_VALENCE.value - PERCEPTION_VALENCE.key) * length(frame))
			]);
		}
		error "Perception with frame "+frame+" is not yet implemented";
	}
}


