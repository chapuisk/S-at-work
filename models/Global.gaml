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
	float s_index_batch;
	float g_index -> gender_pearson(main_observer);
	float g_index_batch;
	float a_index -> age_pseudo_two_lines_index(main_observer);
	float a_index_batch;
	
	float avr_sat_batch;
	int end_cycle_batch;
	
	// CONSTANTS
	float EPSILON <- 1e-6 const:true;
	
	// #####################
	// PARAMETERS
	
	// Input
	int nb_agent init:1000 parameter:true category:"Agent init";
	
	// Observer
	int windows init:5 parameter:true category:"Observer"; // PARAMETER
	int q_number init:10 min:4 max:10 parameter:true category:"Observer"; // PARAMETER
	
	// *************** //
	
	// Cog
	int default_agent_memory <- 5; // length of sat memory (for peak-end)
	
	// 5-traits
	float extra_selection <- 0.0 parameter:true min:0.0 max:1.0 category:"Personality"; // Tendency to reject middle judgement (average satisfaction)
	float consceint_somthing; // TODO find correlations between _c and job evaluation
	
	// W-OWA
	float default_neu_rho <- 0.0 parameter:true min:0.0 max:1.0 category:"W-OWA"; // strenght of neuroticism on wowa: over weight the least satisfiying aspects of job
	float default_gamma <- 1.0 parameter:true min:0.001 max:1.0 category:"W-OWA"; // weight of weights in wowa
	
	// Warr's function
	float default_a <- 1.0 parameter:true min:1.0 max:100.0 category:"Warr's model";
	float default_b <- #e parameter:true min:0.001 max:20.0 category:"Warr's model";
	float default_d <- 0.0 parameter:true min:0.0 max:100.0 category:"Warr's model"; 
	
	// Soc
	bool default_update_social_referents <- false;
	int default_nb_contacts <- 0;
	
	int default_nb_wchar_ex <- 1;
	
	// *************** //
	
	// Network
	graph sn;
	string net_type <- "Random" among:["Random","ScaleFree","SmallWorld"];
	int net_type_sobol <- -1;
	
	int erdos_renyi_k <- 4 min:1 max:20;
	
	int barabasi_start <- 10 min:1 max:int(nb_agent/4);
	int barabasi_m <- 4 min:1 max:nb_agent;
	
	float watts_strogatz_p <- 0.05 min:0.001 max:0.1;
	int watts_strogatz_k <- 4 min:2 max:10; // Must be even
	
	// #####################
	// GLOBAL INITIALIZATION
	// PROCESS
	 
	init {
		float t <- machine_time;
		do syso("Start init");
		do first_init;
		do syso("- Create characteristics");
		do init_characteristics;
		do syso("-- demographics: "
			+"\n\t"+AGE.name+": "+AGE.get_space()
			+"\n\t"+GENDER.name+": "+GENDER.get_space()
			+"\n\t"+EDUCATION.name+": "+EDUCATION.get_space()
			+"\n\t"+FAMILY.name+": "+FAMILY.get_space()
			+"\n\tWork characteristics: "+WORK_CHARACTERISTICS collect (each.name)
		);
		do syso("- Create synthetic work force");
		do init_workers;
		do syso("-- "+length(worker)+" workers: "
			+"\n\t"+worker count (each._demographics[GENDER]="M")+" male | "
				+worker count (each._demographics[GENDER]="W")+" female"
			+"\n\t"+worker min_of (AGE.get_numerical_value(each._demographics[AGE]))+" min | "
				+worker max_of (AGE.get_numerical_value(each._demographics[AGE]))+" max | "
				+mean(worker collect (AGE.get_numerical_value(each._demographics[AGE])))+" avrg " 
		);
		do syso("- Create work organizations");
		do init_organization;
		do syso("-- "+length(organization)+" organizations"
			+"\n\t"+organization min_of length(each.workers)+" min | "
			+"\n\t"+organization max_of length(each.workers)+" max | "
			+"\n\t"+mean(organization collect length(each.workers))+" avrg"
		);
		do syso("- Create social network");
		do init_network;
		do syso("-- "+length(sn.vertices)+" nodes and "+length(sn.edges)+" edges"
			+"\n\t average degree: "+mean(noeud collect (sn degree_of each))
			+"\n\t alpha (prop of cycles): "+alpha_index(sn)
			+"\n\t gamma (prop of links): "+gamma_index(sn)
		);
		// Delete network because unspecified network does not work in Gama
		sn <- nil; ask lien {do die;} ask noeud {do die;}
		do syso("- Init observer and outcomes");
		do init_observer;
		do last_init;
		do syso("End init - total time equals "
			+with_precision(((machine_time-t)/1000),2)+"s"
		);
	}
	
	// #####################
	// STEP INITIALIZATION
	
	// INIT of CHARACTERISTICS
	action init_characteristics {
		do init_demographic_characteristic;
		do init_basic_work_carac;
	}
	
	// INIT OF WORKERS
	action init_workers {
		do read_default_data();
		do workforce_synthesis(nb_agent, demo_distribution); 
	}
	
	// INIT OF SOCIAL CONNECTIONS
	action init_network {
		float t <- machine_time;
		if net_type = nil and net_type_sobol != -1 {net_type <- ["Random","ScaleFree","SmallWorld"][net_type_sobol];}
		switch net_type {
			match "Random" { 
				sn <- generate_random_graph(length(worker),length(worker)*erdos_renyi_k,false,noeud,lien);
			}
			match "ScaleFree" {
				sn <- generate_barabasi_albert(barabasi_start,length(worker),barabasi_m,false,noeud,lien);
			}
			match "SmallWorld" {
				sn <- generate_watts_strogatz(length(worker),watts_strogatz_p,watts_strogatz_k,false,noeud,lien);
			}
			match "Default" {}
		} 
		do syso("Network generated",benchmark_time::machine_time-t);
		map<noeud,worker> nw <- []; map<worker,noeud> wn; list n <- list(noeud);
		loop w over:worker { noeud cn <- any(n); n >- cn; nw[cn] <- w; }
		ask worker { friends <- (sn neighbors_of wn[self]) collect (nw[noeud(each)]); }
		do syso("Assign workers in network",benchmark_time::machine_time-t);
 	}
	
	// INIT OF ORGANIZATION
	action init_organization {
		list a <- list(worker); 
		loop while:not(empty(a)) {
			// Draw a size for the organization and as many worker
			pair<int,int> p <- rnd_choice(orga_sizes);
			int n <- rnd(p.key,p.value);
			list sub_a <- n>length(a) ? a : n among a;
			
			// Create works according to agent demographics
			map<worker,work> sub_w <- sub_a as_map (each::create_random_work(each));
			loop w over:sub_w.keys {w.my_work <- sub_w[w];} 
			
			// Create a simple organization
			organization o <- build_single_position_orga(sub_w.values);
			o.workers <- sub_a;
			
			// Remove workers that have been assigned a job and an organization
			a >>- o.workers;
		}
		ask organization where (empty(each.workers)) {do die;} // TODO why I have to do that ?
	}
	
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
		
		// Orga stuff
		// INSEE Data on the distribution of french firms according to the number of employees
		orga_sizes <- [(1::9)::1028.1,(10::49)::172.6,(50::99)::18.1,
			(100::249)::10.8,(250::nb_agent>250?nb_agent:250)::6.3
		];
	}
	
	// ------------------------------------------- //
	// ------------------- SIM ------------------- //

	// BATCH MODE
	bool batch_mode <- false;
	string output_file <- "batch_output/results.csv";

	/*
	 * Trigger output computation and stop simulation if not batch mode
	 */
	reflex observ when:not(batch_mode) and stop_sim() {}
	
	// -------------------------------------------- //
	// ------------------- LOGS ------------------- //
	// 
	// TODO : make it possible to print log in files
	// 
	
	// Debug
	string LEVEL init:"DEBUG" parameter:true among:["TRACE","DEBUG","WARNING","ERROR"] category:"Utils";
	
	//========//
	// LOGGER //
	//========//
	
	bool DEBUG_MOD <- false;
	list<string> debug_levels <- ["TRACE","DEBUG","WARNING","ERROR"];
	
	/*
	 * Print things in the console in a consistant way
	 * TODO : turn piece of console output into proper log file
	 */
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

species lien {} species noeud {}

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
		display "satisfaction distribution" {
			chart "satisfaction std" type:histogram {
				data "Q0" value:main_observer.qSat[0] color:#red;
				data "Q1" value:main_observer.qSat[1] color:blend(#red,#green,1-1.0/8);
				data "Q2" value:main_observer.qSat[2] color:blend(#red,#green,1-2.0/8);
				data "Q3" value:main_observer.qSat[3] color:blend(#red,#green,1-3.0/8);
				data "Q4" value:main_observer.qSat[4] color:blend(#red,#green,1-4.0/8);
				data "Q5" value:main_observer.qSat[5] color:blend(#red,#green,1-5.0/8);
				data "Q6" value:main_observer.qSat[6] color:blend(#red,#green,1-6.0/8);
				data "Q7" value:main_observer.qSat[7] color:blend(#red,#green,1-7.0/8);
				data "Q8" value:main_observer.qSat[8] color:#green;
			}
		}
		display "satisfaction x age" {
			chart "satisfaction with age" type:histogram {
				data "<25" value:main_observer.aSat[pair<int,int>(0::25)] color:#gold;
				data ">=25<35" value:main_observer.aSat[pair<int,int>(25::35)] color:blend(#gold,#brown,1-1/5);
				data ">=35<45" value:main_observer.aSat[pair<int,int>(35::45)] color:blend(#gold,#brown,1-2/5);
				data ">=45<55" value:main_observer.aSat[pair<int,int>(45::55)] color:blend(#gold,#brown,1-3/5);
				data ">=55<65" value:main_observer.aSat[pair<int,int>(55::65)] color:blend(#gold,#brown,1-4/5);
				data ">=65" value:main_observer.aSat[pair<int,int>(65::MAX_AGE)] color:#brown;
			}
		}
		display "satisfaction x gender" {
			chart "satisfaction with gender" type:histogram {
				data "Women MIN" value:main_observer.gSat["W"][MOMENTS index_of MIN];
				data "Women AVR" value:main_observer.gSat["W"][MOMENTS index_of AVR];
				data "Women MAX" value:main_observer.gSat["W"][MOMENTS index_of MAX];
				data "Men MIN" value:main_observer.gSat["M"][MOMENTS index_of MIN];
				data "Men AVR" value:main_observer.gSat["M"][MOMENTS index_of AVR];
				data "Men MAX" value:main_observer.gSat["M"][MOMENTS index_of MAX];
			} 
		}
 		monitor "sat distribution" value:s_index;
		monitor "sat gender" value:g_index;
		monitor "sat age" value:a_index;
	}
}

experiment abstract_batch virtual:true type:batch until:world.stop_sim() {
	
	init { batch_mode <- true; } 
	
	/*
	 * Main output of the simulation without parameters
	 */
	reflex end_batch {
		
		save ["Sim id","end cycle","avr sat","s index","g index","a index",
			"sq1","sq2","sq3","sq4","sq5","sq6","sq7","sq8","sq9","sq10",
			"wmin","wavr","wmax","mmin","mavr","mmax",
			"a25","a35","a45","a55","a65","a"+MAX_AGE] 
		type:csv to:output_file rewrite:true header:false;
		
		ask simulations {
			save [int(self),self.end_cycle_batch,self.avr_sat_batch,
				self.s_index_batch,self.g_index_batch,self.a_index_batch]
				 +self.main_observer.qSat
				+[self.main_observer.gSat["W"][MOMENTS index_of MIN],
					self.main_observer.gSat["W"][MOMENTS index_of AVR],
					self.main_observer.gSat["W"][MOMENTS index_of MAX]
				]
				+[self.main_observer.gSat["M"][MOMENTS index_of MIN],
					self.main_observer.gSat["M"][MOMENTS index_of AVR],
					self.main_observer.gSat["M"][MOMENTS index_of MAX]
				]
				+self.main_observer.aSat 
			type:csv to:output_file rewrite:false;
		}
		
	}
	
}

