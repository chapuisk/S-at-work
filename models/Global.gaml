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
	float g_index -> gender_pearson(main_observer);
	float a_index -> age_pseudo_two_lines_index(main_observer);
	
	// CONSTANTS
	float EPSILON <- 1e-6 const:true;
	
	// BATCH MODE
	bool batch_mode <- false;
	
	// #####################
	// PARAMETERS
	
	// Debug
	string LEVEL init:"DEBUG" parameter:true among:["TRACE","DEBUG","WARNING","ERROR"] category:"Utils";
	
	// Input
	int nb_agent init:1000 parameter:true category:"Agent init";
	
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
	int default_nb_contacts <- 0;
	
	int default_nb_wchar_ex <- 1;
	
	// *************** //
	
	// Network
	graph sn;
	string net_type <- "SmallWorld" among:["Random","ScaleFree","SmallWorld"];
	
	float erdos_renyi_p <- 0.05 min:0.01 max:1.0;
	
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
			+"\n\t betweeness: "+betweenness_centrality(sn)
			+"\n\t alpha (prop of cycles): "+alpha_index(sn)
			+"\n\t gamma (prop of links): "+gamma_index(sn)
		);
		do syso("- Init observer and outcomes");
		do init_observer;
		do last_init;
		do syso("End init - total time equals "
			+with_precision((machine_time-t/1000),2)+"s"
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
		switch net_type {
			match "Random" { 
				sn <- generate_complete_graph(noeud,lien,length(worker),false);
				loop e over:sn.edges { if(flip(1-erdos_renyi_p)) {remove edge(e) from:sn;}}
			}
			match "ScaleFree" {
				sn <- generate_barabasi_albert(noeud,lien,length(worker),barabasi_m,false);
			}
			match "SmallWorld" {
				sn <- generate_watts_strogatz(noeud,lien,length(worker),
						watts_strogatz_p,watts_strogatz_k,false);
			}
			match "Default" {}
		} 
		map<noeud,worker> nw <- []; map<worker,noeud> wn; list n <- list(noeud);
		loop w over:worker { noeud cn <- any(n); n >- cn; nw[cn] <- w; }
		ask worker { friends <- (sn neighbors_of wn[self]) collect (nw[noeud(each)]); }
 	}
	
	// INIT OF ORGANIZATION
	action init_organization {
		list a <- list(worker); 
		loop while:not(empty(a)) {
			// Adjust distribution of firm size to remaining number of worker
			if orga_sizes.keys one_matches (each.key > length(a)) { 
				orga_sizes <- (orga_sizes.keys where (each.key <= length(a))) 
					as_map (each::orga_sizes[each]>length(a)?length(a):orga_sizes[each]);
			}
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

// Fake species to build network
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
		monitor "sat distribution" value:s_index;
		monitor "sat gender" value:g_index;
		monitor "sat age" value:a_index;
	}
}

