/**
* Name: SynthPop
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model SynthPop

import "../Characteristic.gaml"
import "../Work.gaml"


global {
	
	string model_source_folder <- "../";
	int nb_agent <- 10000; 
	
	string output_file <- model_source_folder+"../includes/synthpop.csv";
	
	init {
		float t <- machine_time;
		
		write("Start init");
		
		write("- Create characteristics");
		do init_demographic_characteristic;
		do init_basic_work_carac;
		do init_eijqi_work_carac;
		write("-- demographics: "
			+"\n\t"+AGE.name+": "+AGE.get_space()
			+"\n\t"+GENDER.name+": "+GENDER.get_space()
			+"\n\t"+EDUCATION.name+": "+EDUCATION.get_space()
			+"\n\t"+FAMILY.name+": "+FAMILY.get_space()
			+"\n\tWork characteristics: "+WORK_CHARACTERISTICS collect (each.name)
		);
		
		write("- Create synthetic work force");
		do read_default_data;
		do workforce_synthesis(nb_agent, demo_distribution);
		write("-- "+length(worker)+" workers: "
			+"\n\t"+worker count (each._demographics[GENDER]="M")+" male | "
				+worker count (each._demographics[GENDER]="W")+" female"
			+"\n\t"+worker min_of (AGE.get_numerical_value(each._demographics[AGE]))+" min | "
				+worker max_of (AGE.get_numerical_value(each._demographics[AGE]))+" max | "
				+mean(worker collect (AGE.get_numerical_value(each._demographics[AGE])))+" avrg " 
		);
		
		write("- Create works");
		ask worker { 
			ask world {myself.my_work <- create_eijqi_work(myself);}
			_work_aspects[SALARY] <- string(my_work.salary);
			_work_aspects[WORKING_TIME] <- string(with_precision(my_work.working_time_per_week,2)); 
			_work_aspects[CONTRACT] <- string(my_work.contract);
		}
		write("-- works:\n"+
			"\tsalary: min="+min(work collect (each.salary))
				+" | max="+max(work collect (each.salary))
				+" | avr="+mean(work collect (each.salary))+
			"\n\tworking hours: min="+min(work collect (each.working_time_per_week))
				+" | max="+max(work collect (each.working_time_per_week))
				+" | avr="+mean(work collect (each.working_time_per_week))
		);
		
		write("End init - total time equals "
			+with_precision(((machine_time-t)/1000),2)+"s"
		);
		
		save [AGE.name,GENDER.name,EDUCATION.name,FAMILY.name] + WORK_CHARACTERISTICS collect (each.name)  
			to:output_file type:csv header:false rewrite:true;
		loop w over:worker {
			save [w.get(AGE),w.get(GENDER),w.get(EDUCATION),w.get(FAMILY)]
				+ WORK_CHARACTERISTICS collect (w._work_aspects[each])
				to:output_file type:csv rewrite:false;
		}
		
	}
	
	// -------------------------------------------- //
	// 				INPUT DATA MANAGEMENT			//
	
	// EUROSTATS
	// ----
	// TODO: move to synth pop
	
	string eurostat_folder <-  model_source_folder+"../includes/EUROSTATS/";
	string scripts_folder <- model_source_folder+"../scripts/";
	// EIJQI : Mira 2021
	// https://doi.org/10.1007/s11205-021-02615-9
	string ewcs_transformed <- model_source_folder+"../includes/ewcs2015_short_fr.csv";
	map<map<characteristic,string>,float> demo_distribution;
	matrix ewcs_data;
	
	// Demogrphics
	string es_educ_path <- eurostat_folder+"Age_sex_educ_employees.csv";
	
	// Work characteristics
	string es_earnings_path <- scripts_folder+"fitdist/fit_earning_eurostats.csv";
	map<list<string>,pair<float,float>> lnorm_earning_map <- [];
	
	string es_time_path <- eurostat_folder+"Sex_partfulltime_employement.csv";
	list<string> time_regimes <- ["Part-time","Full-time"];
	map<list<string>,float> bimod_workingtime_map <- [];
	
	/*
	 * Read, clean and prepare default data 
	 * TODO : create a clean log of the method activities
	 */
	action read_default_data {
		
		demo_distribution <- [];
		
		// Read data for educ x gender x age
		matrix medu <- matrix(csv_file(es_educ_path));
		map<string,list<string>> gender_map <- ["Males"::["M"],"Females"::["W"]];
		map<string,list<string>> edu_map <- [
			"Less than primary; primary and lower secondary education (levels 0-2)"::["No education completed","Primary or lower secondary education"],
			"Upper secondary and post-secondary non-tertiary education (levels 3 and 4)"::["Upper secondary or post-secondary education"],
			"Tertiary education (levels 5-8)"::["Tertiary education"]];
		map<string,list<int>> age_map <- ["From 15 to 24 years"::range(15,24),"From 25 to 49 years"::range(25,49),"From 50 to 74 years"::range(50,74)];
		map<map<characteristic,string>,float> dist_medu <- []; 
		loop rm over:rows_list(medu) {
			if(gender_map contains_key first(rm)){
				loop ak over:age_map.keys {
					loop rgk over:gender_map[string(rm[0])] {
						loop rek over:edu_map[string(rm[1])] {
							loop rak over:age_map[ak] {
								dist_medu[[GENDER::rgk,EDUCATION::rek,AGE::rak]] <- float(rm[2+age_map.keys index_of ak]);
							}
						}
					}
				}
			}
		}
		
		// TODO: Find data about age x gender x family status
		// For now uniform
		loop ck over:dist_medu.keys {
			loop fv over:FAMILY.get_space() {
				map<characteristic,string> actual_k <- copy(ck);
				actual_k <+ FAMILY::fv;
				demo_distribution[actual_k] <- dist_medu[ck];
			}
		}
		
		matrix mearn <- matrix(csv_file(es_earnings_path));
		loop row over:rows_list(mearn){
			list<string> key <- [first(row)="F"?"W":first(row)];
			pair<float,float> params <- float(row[3])::float(row[4]);
			int age_low_bound; int age_upper_bound;
			switch row[1] { 
				match "<30" { age_low_bound <- 15; age_upper_bound <- 29; }
				match "30-39" { age_low_bound <- 30; age_upper_bound <- 39; }
				match "40-49" { age_low_bound <- 40; age_upper_bound <- 49; }
				match "50-59" { age_low_bound <- 50; age_upper_bound <- 59; }
				match ">60" { age_low_bound <- 60; age_upper_bound <- 74; }
			}
			loop a from:age_low_bound to:age_upper_bound { lnorm_earning_map[key+[string(a)]] <- params; }
		}
		
		do syso(sample(lnorm_earning_map),level::first(debug_levels));
		
		matrix mtime <- matrix(csv_file(es_time_path));
		int midx; int widx;
		loop row over:rows_list(mtime){
			if row contains "Males" and row contains "Females"{ midx <- row index_of "Males"; widx <- row index_of "Females"; }
			if time_regimes contains first(row) {
				list<string> key <- [first(row)];
				bimod_workingtime_map[key+["M"]] <- float(row[midx]);
				bimod_workingtime_map[key+["W"]] <- float(row[widx]);
			}
		}
		
		do syso(sample(bimod_workingtime_map),level::first(debug_levels));
		
		// Load ewcs data
		ewcs_data <- matrix(csv_file(ewcs_transformed));
	}
	
	// ------------------------------------ //
	// ------------------------------------ //
	// 				DEMOGRAPHICS			//
	// ------------------------------------ //
	// ------------------------------------ //
		
	/*
	 * workforce synthesis based on joint distribution of demographics
	 */
	list<worker> workforce_synthesis(int nb, map<map<characteristic,string>,float> joint_distribution) {
		if empty(WORK_CHARACTERISTICS) {error "Work characteristic should be initialze first";}
		if empty(joint_distribution) {error "Cannot initialize workforce with empty distribution";}
		create worker number:nb returns:work_force {
			map<characteristic,string> profile <- rnd_choice(joint_distribution);
			loop c over:profile.keys {
				string v <- profile[c];
				switch c.gama_type {
					match_one ["int","float"] {
						if float(c.get_space()[0]) > float(v) or float(c.get_space()[1]) < float(c) {
							ask world {do syso("Wrong numerical value "+v+" for "+c,action_name::"workforce_synthesis",level::last(debug_levels));}
						} 
					}
					default { 
						if not(c.get_space() contains v) { ask world {do syso("Unmatching value "+v+" for characteristic "+c.name+" [val="+c.get_space()+"]",
							action_name::"workforce_synthesis",level::last(debug_levels)
						);} }
					}
				}
				_demographics[c] <- v;
			}
		}
		return work_force;
	}
	
	
	// ------------------------------------ //
	// ------------------------------------ //
	// 					WORKS				//
	// ------------------------------------ //
	// ------------------------------------ //
	
	
	
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
		float wt <- gauss(bimod_workingtime_map[any(bimod_workingtime_map.keys where (each contains w._demographics[GENDER]))],working_time_sigma);
		// Choose salary according to working hours
		pair<float,float> param <- lnorm_earning_map[[w._demographics[GENDER],w._demographics[AGE]]];
		int s <- round(lognormal_rnd(param.key,param.value) * wt * 4);
		// If no tasks are given create only one
		if ts=nil {create task returns:t; ts <- t;}
		
		
		create work with:[tasks::ts,salary::s,working_time_per_week::wt,contract::rnd_choice(contract_types_weights)] returns:res;
		work the_work <- first(res);
		w.my_work <- the_work;
		return first(res);
	}
	
	// Create works based on data from EWCS, following EIJQI that summs up intrisic work characteristics from EWCS <p>
	// #1 Build a random work based on demographics <\br>
	// #2 Use demographic and builded work characteristics to choose a EIJQI perception that fit 
	work create_eijqi_work(worker w) {
		work my_work <- create_random_work(w);
		
		list<float> dist;
		loop rec over:rows_list(ewcs_data) {
			float fit <- 0.0; 
			// Family
			int family_rec <- int(rec[1]);
			switch w.get(FAMILY) { 
				match "Single adult without children" { if family_rec=1 {fit <- fit + 1;} }
				match "Single adult with children" { if family_rec>=2 {fit <- fit + 1;}}
				match "Couple without children" { if family_rec=2 {fit <- fit + 1;}}
				match "Couple with children" { if family_rec>2 {fit <- fit + 1;}}
			}
			// Gender
			fit <- fit + 1 - GENDER.compare_values(w.get(GENDER),rec[2]);
			// Age
			fit <- fit + 1 / (1 + abs(AGE.compare_values(w.get(AGE),rec[3])) / 5); 
			// Type of contract
			fit <- fit + (rec[5] = my_work.contract ? 1 : 0);
			// Working time
			fit <- fit + 1 / (1 + abs(int(rec[6]) - my_work.working_time_per_week) / my_work.working_time_per_week * working_time_equivalence);
			// Education
			fit <- fit + 1 - EDUCATION.compare_values(w.get(EDUCATION),rec[7]);
			// Salary
			fit <- fit + 1 / (1 + abs(int(rec[8]) - my_work.salary) / my_work.salary * salary_equivalence);
			
			dist <+ fit;
		}
		int drawn_rec <- rnd_choice(dist);
		
		// TODO : ideally we should define jobs characteristics that fit perceptions rather than force perceptions
		w._work_aspects[eijqi_autonomy] <- ewcs_data[10,drawn_rec];
		w._work_aspects[eijqi_interaction] <- ewcs_data[11,drawn_rec];
		w._work_aspects[eijqi_intensity] <- ewcs_data[12,drawn_rec];
		w._work_aspects[eijqi_meaningful] <- ewcs_data[13,drawn_rec];
		
		// Change working time for a real one
		my_work.working_time_per_week <- float(ewcs_data[6,drawn_rec]); 
		
		return my_work;
	}
	
	// Do not know if usefull
	//
//	if ewcs_transformed!=nil and ewcs_transformed!="" { ewcs_data <- matrix(csv_file(ewcs_transformed)); }
//		ewcs_earning_span <- {#infinity,0};
//		loop r from:0 to: ewcs_data.rows - 1 {
//			int val <- int(float(ewcs_data[8,r]));
//			if val < ewcs_earning_span.x {ewcs_earning_span <- {val,ewcs_earning_span.y};}
//			if val > ewcs_earning_span.y {ewcs_earning_span <- {ewcs_earning_span.x,val};}
//		}
	
}

experiment synthpop type:gui {}

