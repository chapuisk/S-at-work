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
	float salary_equivalence <- 0.2;
	
	num_characteristic WORKING_TIME; // per week
	string working_time <- "working time";
	point default_working_time_range <- point(8,42);
	float working_time_equivalence <- 0.1;
	float avr_working_hours <- 35#h;
	float std_working_hours <- 5#h;
	
	nominal_characteristic CONTRACT;
	string contract <- "contract time";
	list<string> default_contract_types <- ["short term","long term"];
	map<string,float> contract_types_weights <- [default_contract_types[0]::0.105,default_contract_types[1]::0.895]; // temporary employement Eurostats
	
	point ewcs_earning_span <- nil;
	
	float EIJQI_EQUI <- 0.1;
	float EIJQI_AMBI <- 0.0; 
	
	characteristic eijqi_autonomy; // The highest >> the more autonomous
	int EIJQI_AUTONOMY_MIN <- 4; int EIJQI_AUTONOMY_MAX <- 15;
	string EIJQI_AUTONOMY <- "autonomy"; 
	float EIJQI_AUTONOMY_EQUI <- EIJQI_AUTONOMY_MAX*EIJQI_EQUI; float EIJQI_AUTONOMY_AMBI <- EIJQI_AMBI;
	
	characteristic eijqi_interaction; // The highest >> the better is interactions
	int EIJQI_INTERACTION_MIN <- 4; int EIJQI_INTERACTION_MAX <- 15;
	string EIJQI_INTERACTION <- "interaction"; 
	float EIJQI_INTERACTION_EQUI <- EIJQI_INTERACTION_MAX*EIJQI_EQUI; float EIJQI_INTERACTION_AMBI <- EIJQI_AMBI;
	
	characteristic eijqi_intensity; // The highest << the lowest intensity
	int EIJQI_INTENSITY_MIN <- 2; int EIJQI_INTENSITY_MAX <- 14;
	string EIJQI_INTENSITY <- "intensity"; 
	float EIJQI_INTENSITY_EQUI <- EIJQI_INTENSITY_MAX*EIJQI_EQUI; float EIJQI_INTENSITY_AMBI <- EIJQI_AMBI;
	
	characteristic eijqi_meaningful; // The highest >> the meaningful
	int EIJQI_MEANINGFUL_MIN <- 2; int EIJQI_MEANINGFUL_MAX <- 10;
	string EIJQI_MEANINGFUL <- "meaningful"; 
	float EIJQI_MEANINGFUL_EQUI <- EIJQI_MEANINGFUL_MAX*EIJQI_EQUI; float EIJQI_MEANINGFUL_AMBI <- EIJQI_AMBI;
	
	
	// Pre-defined likert scales
	list<string> likert3 <- ["low","medium","high"];
	list<string> likert5 <- ["very low","low","medium","high","very high"];
	list<string> likert7 <- ["lowest","very low","low","medium","high","very high","highest"];
	
	// -------------------------------------- //
	// WORK CREATION
	
	// ====== BUILDER
	// CHARACTERISTIC OF WORK
	
	action init_basic_work_carac(point salary_range <- default_salary_range, 
		point working_time_range <- default_working_time_range, 
		list<string> contract_type <- default_contract_types
	) {
			SALARY <- num_characteristic(create_num_work_charac("int",salary,salary_range,equi_range::salary_equivalence));
			WORKING_TIME <- num_characteristic(create_num_work_charac("float",working_time,default_working_time_range,equi_range::working_time_equivalence));
			CONTRACT <- nominal_characteristic(create_nom_work_charac(contract,contract_type,false,0.0));
			WORK_CHARACTERISTICS <- [SALARY,WORKING_TIME,CONTRACT];
	}
	
	/*
	 * European intrinsic job quality index (built from EWCS)
	 */
	action init_eijqi_work_carac(pair<float,float> jc_ambiguity <- 1.0::1.0) {
		eijqi_autonomy <- num_characteristic(create_num_work_charac("int",EIJQI_AUTONOMY,
			{EIJQI_AUTONOMY_MIN,EIJQI_AUTONOMY_MAX},EIJQI_AUTONOMY_AMBI,EIJQI_AUTONOMY_EQUI
		));
		eijqi_interaction <- num_characteristic(create_num_work_charac("int",EIJQI_INTERACTION,
			{EIJQI_INTERACTION_MIN,EIJQI_INTERACTION_MAX},EIJQI_INTERACTION_AMBI,EIJQI_INTERACTION_EQUI
		));
		eijqi_intensity <- num_characteristic(create_num_work_charac("int",EIJQI_INTENSITY,
			{EIJQI_INTENSITY_MIN,EIJQI_INTENSITY_MAX},EIJQI_INTENSITY_AMBI,EIJQI_INTENSITY_EQUI
		));
		eijqi_meaningful <- num_characteristic(create_num_work_charac("int",EIJQI_MEANINGFUL,
			{EIJQI_MEANINGFUL_MIN,EIJQI_MEANINGFUL_MAX},EIJQI_MEANINGFUL_AMBI,EIJQI_MEANINGFUL_EQUI
		));
		if WORK_CHARACTERISTICS!=nil and not(empty(WORK_CHARACTERISTICS)) {
			WORK_CHARACTERISTICS <<+ [eijqi_autonomy,eijqi_intensity,eijqi_interaction,eijqi_meaningful];
		}
	}
	
	// ====== FACTORY
	// ACTUAL WORK
	
	// Create the simplest work comprising 'nb_tasks' inner tasks and basic work characteristics (i.e. salary, working time and contract)
	work create_simple_work(int nb_tasks <- 0, pair<int,int> salary_range <- nil, 
		pair<float,float> working_hours <- avr_working_hours::std_working_hours, 
		map<string,float> c_weight <- contract_types_weights 
	) {
		create task number:nb_tasks returns:t;
		int s <- int(SALARY.get_numerical_value(first(SALARY.get_space())));
		if salary_range=nil { s <- rnd(s,int(SALARY.get_numerical_value(last(SALARY.get_space())))); }
		else { s <- rnd(salary_range.key,salary_range.value);}
		create work with:[tasks::t,
			salary::s,
			working_time_per_week::gauss(working_hours.key,working_hours.value),
			contract::rnd_choice(c_weight)
		] returns:w;
		return first(w);
	}
	
	
	
	// -------------------------------------- //
	// WORK CHARACTERISTIC CREATION
	
	// Numerical work characteristic creation
	characteristic create_num_work_charac(string num_type, string c_name, point range, float subjectivity <- 0.0, float equi_range <- EPSILON) {
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


