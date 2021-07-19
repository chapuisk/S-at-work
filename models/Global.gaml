/**
* Name: Global
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Global

import "Work.gaml"
import "Characteristic.gaml"

global {
	
	/*
	 * GLOBAL INITIALIZATION
	 */
	init {
		do first_init;
		do init_characteristics;
		do init_workers;
		do init_organization;
		do last_init;
	}
	
	// INIT of CHARACTERISTICS
	action init_characteristics {
		do init_demographic_characteristic;
		do init_basic_work_carac;
	}
	
	// INIT OF WORKERS
	action init_workers {}
	// INIT OF ORGANIZATION
	action init_organization {}
	
	// Can add something before global init overloading the method
	action first_init {}
	// Can add something after global init overloading the method
	action last_init {}
	
	// -------------------------------------------- //
	// 				AD HOC POP SYNTHESIS			//
	
	// EUROSTATS
	// ----
	
	string eurostat_folder <-  "../includes/EUROSTATS/";
	map<map<characteristic,string>,float> demo_distribution;
	
	// Demogrphics
	file es_educ <- csv_file(eurostat_folder+"Age_sex_educ_employees.csv");
	
	// Work characteristics
	file es_earnings <- csv_file(eurostat_folder+"Age_sex_earningperh_2014.csv");
	map<list<string>,list<float>> lnorm_map <- [];
	
	file es_time <- csv_file(eurostat_folder+"Sex_partfulltime_employement.csv");
	
	/*
	 * Read, clean and prepare default data 
	 */
	action read_default_data {
		
		demo_distribution <- [];
		
		// Read data for educ x gender x age
		matrix medu <- matrix(es_educ);
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
		
		matrix mtime <- matrix(es_time);
		
	}
	
	// -------------------------------------------- //
	// ------------------- LOGS ------------------- //
	// 
	// TODO : make it possible to print log in files
	// 
	
	bool DEBUG_MOD <- false;
	string LEVEL <- "DEBUG";
	list<string> debug_levels <- ["TRACE","DEBUG","WARNING","ERROR"];
	
	action syso(string msg, float benchmark_time <- -1.0, agent caller <- self, string action_name <- nil, string level <- LEVEL) {
		if DEBUG_MOD and debug_levels index_of level >= debug_levels index_of LEVEL {
			string m <- "["+caller.name+(action_name=nil?"":"|"+action_name)+"]"
				+(benchmark_time>0.0?"[TM="+with_precision(benchmark_time/1000,3)+"s] ":" ")+msg;
			switch level {
				match_one ["TRACE","DEBUG"] {write m;}
				match "WARNING" { write m color:#orange; }
				match "ERROR" { do err(msg,caller,action_name); }
			}
		}
	}
	
	action debug_level(int level <- 1) {return level < length(debug_levels) ? debug_levels[level] : LEVEL;}
	action err(string msg, agent caller <- self, string action_name <- nil) { error "["+caller.name+(action_name=nil?"":"|"+action_name)+"] "+msg; }
	
}

/*
 * ABSTRACT XP - TO BE OVERLOADED
 */
experiment "abstract_xp" virtual:true type:gui {
	output {
		display "satisfaction" type: opengl {
			chart "satisfaction" type: series legend_font:font(0.0) series_label_position:none style:line {
				loop w over:worker {
					data "sat"+int(w) value:w.job_satisfaction color:rnd_color(int(w)/length(worker)*255);
				}
			}
		}
	}
}

