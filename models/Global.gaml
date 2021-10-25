/**
* Name: Global
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Global

import "Work.gaml"
import "Characteristic.gaml"
import "Observer.gaml"

global {
	
	// #####################
	// MODEL VAR
	
	// GLOBAL VARIABLES
	observer main_observer;
	float s_index -> sat_dist_index(main_observer);
	float g_index -> gender_index(main_observer);
	float a_index -> age_index(main_observer);
	
	// CONSTANTS
	float EPSILON <- 1e-6 const:true;
	
	// BATCH MODE
	bool batch_mode <- false;
	
	// #####################
	// PARAMETERS
	
	// Debug
	string LEVEL init:"DEBUG" parameter:true among:["TRACE","DEBUG","WARNING","ERROR"] category:"Utils";
	
	// Input
	int nb_agent init:50 parameter:true category:"Agent init";
	
	// Observer
	int windows init:5 parameter:true category:"Observer"; // PARAMETER
	int q_number init:10 min:4 max:10 parameter:true category:"Observer"; // PARAMETER
	
	// *************** //
	
	// Cog
	int default_agent_memory <- 5; // length of sat memory (for peak-end)
	
	// W-OWA
	float default_neu_rho <- 0.0 parameter:true min:0.0 max:1.0 category:"W-OWA"; // strenght of neuroticism on wowa: over weight the least satisfiying aspects of job
	float default_gamma <- 1.0 parameter:true min:0.001 max:1.0 category:"W-OWA"; // weight of weights in wowa
	
	// Warr's function
	float default_a <- 1.0 parameter:true min:1.0 max:100.0 category:"Warr's model";
	float default_b <- #e parameter:true min:0.001 max:20.0 category:"Warr's model";
	float default_d <- 0.0 parameter:true min:0.0 max:100.0 category:"Warr's model"; 
	
	// Soc
	bool default_update_social_referents <- false;
	int default_nb_social_contacts <- 0;
	
	int default_work_charac_exchange_nb <- 1;
	
	// #####################
	// GLOBAL INITIALIZATION
	 
	init {
		do first_init;
		do init_characteristics;
		do init_workers;
		do init_organization;
		do init_observer;
		do last_init;
	}
	
	// INIT of CHARACTERISTICS
	action init_characteristics {
		do init_demographic_characteristic;
		do init_basic_work_carac;
	}
	
	// INIT OF WORKERS
	action init_workers { /* TODO : propose default init */ }
	// INIT OF ORGANIZATION
	action init_organization { /* TODO : propose default init */ }
	
	// Can add something before global init overloading the method
	action first_init {}
	// Can add something after global init overloading the method
	action last_init {}
	
	// -------------------------------------------- //
	// 				INPUT DATA MANAGEMENT			//
	
	// EUROSTATS
	// ----
	
	string eurostat_folder <-  "../includes/EUROSTATS/";
	string scripts_folder <- "../scripts/";
	map<map<characteristic,string>,float> demo_distribution;
	
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
		
		do syso(sample(lnorm_earning_map));
		
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
	}
	
	// -------------------------------------------- //
	// ------------------- LOGS ------------------- //
	// 
	// TODO : make it possible to print log in files
	// 
	
	//========//
	// LOGGER //
	//========//
	
	bool DEBUG_MOD <- false;
	list<string> debug_levels <- ["TRACE","DEBUG","WARNING","ERROR"];
	
	action syso(string msg, float benchmark_time <- -1.0, agent caller <- self, string action_name <- nil, string level <- LEVEL) {
		if DEBUG_MOD and (debug_levels index_of level >= debug_levels index_of LEVEL) {
			string m <- "["+caller.name+(action_name=nil?"":"|"+action_name)+"]"
				+(benchmark_time>0.0?"[TM="+with_precision(benchmark_time/1000,3)+"s] ":" ")+msg;
			switch level {
				match_one ["TRACE","DEBUG"] {write m;}
				match "WARNING" { write m color:#orange; }
				match "ERROR" { do err(msg,caller,action_name); }
			}
		}
	}
	
	string debug_level(int level <- 1) {return level < length(debug_levels) ? debug_levels[level] : LEVEL;}
	action err(string msg, agent caller <- self, string action_name <- nil) { error "["+caller.name+(action_name=nil?"":"|"+action_name)+"] "+msg; }
	
}

/*
 * ABSTRACT XP - TO BE OVERLOADED
 */
experiment "abstract_xp" virtual:true type:gui {
	output {
		display "satisfaction" {
			chart "satisfaction" type: series legend_font:font(0.0) series_label_position:none style:line {
				loop w over:worker {
					data "sat"+int(w) value:w._job_satisfaction color:rnd_color(int(w)/length(worker)*255);
				}
			}
		}
		monitor "sat distribution" value:s_index;
		monitor "sat gender" value:g_index;
		monitor "sat age" value:a_index;
	}
}

