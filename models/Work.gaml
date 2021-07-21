/**
* Name: Work
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Work

import "Characteristic.gaml"
import "Organization.gaml"

global {
	
	// ---------------------------------
	list<characteristic> WORK_CHARACTERISTICS;
	// ---------------------------------
	
	num_characteristic SALARY; // per month
	string salary <- "salary";
	point default_salary_range <- point(1200,10000);
	
	num_characteristic WORKING_TIME; // per week
	string working_time <- "working time";
	point default_working_time_range <- point(8,42);
	float avr_working_hours <- 35#h;
	float std_working_hours <- 5#h;
	
	nominal_characteristic CONTRACT;
	string contract <- "contract time";
	list<string> default_contract_types <- ["short term","long term"];
	map<string,float> contract_types_weights <- ["short term"::0.1,"long term"::0.9];
	
	// Job characteristic model : Hackman & Oldam
	// https://www.sciencedirect.com/science/article/pii/S1877042814028286/pdf?md5=d22856a61e08683d63419dfdf7ba5483&pid=1-s2.0-S1877042814028286-main.pdf
	characteristic task_identity;
	characteristic task_significance;
	characteristic skill_variety;
	characteristic autonomy;
	characteristic feedback;
	
	// Work-role output model : Souza & Souza
	// https://www.sciencedirect.com/science/article/pii/S1053535700000858?casa_token=uCZyaZ8OFBkAAAAA:mZB8NtKKg34_PjKBjqIRN_B-3W-ITuWwouowRPAwcsLJFU9n17tI5pe_khiDCMQ_OKYv13u60kA#BIB30
	characteristic job_security;
	characteristic advancement_op;
	characteristic interesting_job;
	characteristic work_independently;
	characteristic help_people;
	characteristic useful_to_society;
	characteristic rl_with_manager;
	characteristic rl_with_colleague;
	
	list<string> likert3 <- ["low","medium","high"];
	list<string> likert5 <- ["very low","low","medium","high","very high"];
	list<string> likert7 <- ["lowest","very low","low","medium","high","very high","highest"];
	
	// -------------------------------------- //
	// WORK CREATION
	
	// CHARACTERISTIC OF WORK
	
	action init_basic_work_carac(point salary_range <- default_salary_range, float salary_equi_range <- 500.0, 
		point working_time_range <- default_working_time_range, list<string> contract_type <- default_contract_types) {
			SALARY <- num_characteristic(create_num_work_charac("int",salary,salary_range,equi_range::salary_equi_range));
			WORKING_TIME <- num_characteristic(create_num_work_charac("float",working_time,default_working_time_range));
			CONTRACT <- nominal_characteristic(create_nom_work_charac(contract,contract_type,false,0.0));
			WORK_CHARACTERISTICS <- [SALARY,WORKING_TIME,CONTRACT];
	}
	
	action init_jc_model_work_carac(int scale_size, pair<float,float> jc_ambiguity <- 1.0::1.0){
		task_identity <- create_likert_work_charac("taks identitiy",scale_size,rnd(jc_ambiguity.key,jc_ambiguity.value));
		task_significance <- create_likert_work_charac("taks significance",scale_size,rnd(jc_ambiguity.key,jc_ambiguity.value));
		skill_variety <- create_likert_work_charac("skill variety",scale_size,rnd(jc_ambiguity.key,jc_ambiguity.value));
		autonomy <- create_likert_work_charac("Autonomy",scale_size,rnd(jc_ambiguity.key,jc_ambiguity.value));
		feedback <- create_likert_work_charac("Feedback",scale_size,rnd(jc_ambiguity.key,jc_ambiguity.value));
		if empty(WORK_CHARACTERISTICS) {WORK_CHARACTERISTICS <- [];}
		WORK_CHARACTERISTICS <<+ [task_identity,task_significance,skill_variety,autonomy,feedback];
	}
	
	// ACTUAL WORK
	
	// Create the simplest work comprising 'nb_tasks' inner tasks and basic work characteristics (i.e. salary, working time and contract)
	work create_simple_work(int nb_tasks <- 0, pair<int,int> salary_range <- nil, 
		pair<float,float> working_hours <- avr_working_hours::std_working_hours, map<string,float> contract_weight <- contract_types_weights 
	) {
		create task number:nb_tasks returns:t;
		int s <- int(SALARY.get_numerical_value(first(SALARY.get_space())));
		if salary_range=nil { s <- rnd(s,int(SALARY.get_numerical_value(last(SALARY.get_space())))); }
		else { s <- rnd(salary_range.key,salary_range.value);}
		create work with:[tasks::t,
			salary::s,
			working_time_per_week::gauss(working_hours.key,working_hours.value),
			contract::rnd_choice(contract_weight)
		] returns:w;
		return first(w);
	}
	
	// Create work based on estimated distribution <p>
	// see Global.gaml read_default_data method
	work create_random_work(worker w, list<task> ts <- nil, float working_time_sigma <- 1) {
		if lnorm_earning_map=nil or empty(lnorm_earning_map) {
			do syso("Earning distribution map should not be null or empty",action_name::"create_random_work",level::last(debug_levels));
		}
		if bimod_workingtime_map=nil or empty(bimod_workingtime_map) {
			do syso("Working time distribution map should not be null or empty",action_name::"create_random_work",level::last(debug_levels));
		}
		
		// Choose betwee part or full time (i.e. any statement), then use corresponding mean value in a gaussian
		float wt <- gauss(bimod_workingtime_map[any(bimod_workingtime_map.keys where (each contains w.demographics[GENDER]))],working_time_sigma);
		// Choose salary according to working hours
		pair<float,float> param <- lnorm_earning_map[[w.demographics[GENDER],w.demographics[AGE]]];
		int s <- round(lognormal_rnd(param.key,param.value) * wt);
		// If no tasks are given create only one
		if ts=nil {create task returns:t; ts <- t;}
		
		
		create work with:[tasks::ts,salary::s,working_time_per_week::wt,contract::any(CONTRACT.get_space())] returns:res;
		work the_work <- first(res);
		w.my_work <- the_work;
		return first(res);
	}
	
	// -------------------------------------- //
	// WORK CHARACTERISTIC CREATION
	
	// Numerical work characteristic creation
	characteristic create_num_work_charac(string num_type, string c_name, point range, float subjectivity <- 0.0, float equi_range <- 0.0) {
		if not(["int", "float"] contains num_type) {error "wrong type of characteristic";}
		create num_characteristic with:[name::c_name,ambiguity::subjectivity,equality_range::equi_range,
			gama_type::num_type,min::range.x,max::range.y
		] returns:ca;
		return first(ca);
	}
	
	// Ordinal work characteristic creation
	characteristic create_nom_work_charac(string c_name, list<string> vals, bool ordered, float subjectivity) {
		characteristic inner;
		if ordered {create ordered_characteristic with:[name::c_name,ambiguity::subjectivity,gama_type::"string",values::vals] returns:co; inner <- first(co);}
		else {create nominal_characteristic with:[name::c_name,ambiguity::subjectivity,gama_type::"string",values::vals] returns:cn; inner <- first(cn);}
		return inner;
	}
	
	// Likert based subjective work characteristic
	characteristic create_likert_work_charac(string c_name, int scale, float subjectivity <- 0.5) {
		if subjectivity=0.0 {error "A likert based work characteristic should not be objective by nature";}
		list<string> vals; 
		if scale = 3 {vals <- likert3;}
		else if scale = 5 {vals <- likert5;}
		else if scale = 7 {vals <- likert7;}
		else {loop s from:1 to:scale {vals <+ string(s);}}
		create ordered_characteristic with:[name::c_name,ambiguity::subjectivity,gama_type::"string",values::vals] returns:co;
		return first(co);
	}
	
}

/*
 * A work
 */
species work { 
	// the organization that provide that job
	organization org;
	
	//  Job basic characteristics
	int salary;
	string contract;
	float working_time_per_week;
	
	list<task> tasks;
}

species task { }


