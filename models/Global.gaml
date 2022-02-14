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
	
	// Experiment
	string model_source_folder <- "";
	// Synth pop
	string synth_pop <- model_source_folder+"../includes/synthpop.csv";
	
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
	int windows init:4 parameter:true category:"Observer"; // PARAMETER
	int q_number init:10 min:4 max:10 parameter:true category:"Observer"; // PARAMETER
	
	// *************** //
	
	// Cog
	int default_agent_memory <- 5; // length of sat memory (for peak-end)
	
	// 5-traits
	float extra_selection <- 0.0 parameter:true min:0.0 max:1.0 category:"Personality"; // Tendency to reject middle judgement (average satisfaction)
	float default_new_mode <- 0.2 parameter:true min:0.0 max:1.0 category:"Personality"; // Tendency to accept new ideas (openess) or to be empatic (agreableness)
	
	// W-OWA
	bool default_rnd_wc_weights <- false parameter:true category:"W-OWA";
	float default_neu_rho <- 0.0 parameter:true min:0.0 max:1.0 category:"W-OWA"; // strenght of neuroticism on wowa: over weight the least satisfiying aspects of job
	float default_gamma <- 1.0 parameter:true min:0.001 max:1.0 category:"W-OWA"; // weight of weights in wowa
	
	// Warr's function
	float default_a <- 1.0 parameter:true min:1.0 max:100.0 category:"Warr's model";
	float default_b <- #e parameter:true min:0.001 max:20.0 category:"Warr's model";
	float default_d <- 0.0 parameter:true min:0.0 max:100.0 category:"Warr's model"; 
	
	// Soc
	bool default_update_social_referents <- false;
	int default_nb_contacts <- 0;
	
	int default_nb_wchar_ex <- 0;
	
	bool kinship <- false;
	
	// *************** //
	
	// Network
	graph sn;
	string net_type <- "Complet" among:["Random","ScaleFree","SmallWorld","Complet"];
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
		do init_eijqi_work_carac;
	}
	
	// INIT OF WORKERS
	action init_workers {
		loop wl over:sample(rows_list(csv_file(synth_pop).contents),nb_agent,false) {
			create worker {
				_demographics[AGE] <- wl[0];
				_demographics[GENDER] <- wl[1];
				_demographics[EDUCATION] <- wl[2];
				_demographics[FAMILY] <- wl[3];
				ask world {do random_gaussian_personality(myself);}
				int idx <- 4;
				loop c over:WORK_CHARACTERISTICS { 
					work_evaluator[c] <- world.work_eval(c);
					_work_aspects[c] <- wl[idx]; idx <- idx+1; 
				}
			}
		}
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
			default { sn <- generate_complete_graph(length(worker),false); }
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
			
			list<work> sub_w;
			loop wer over:sub_a {
				// Create works according to agent 
				create work with:[
					salary::SALARY.get_numerical_value(wer._work_aspects[SALARY]),
					working_time_per_week::WORKING_TIME.get_numerical_value(wer._work_aspects[WORKING_TIME]),
					contract::wer._work_aspects[CONTRACT]
				] returns:w;
			
				sub_w <+ first(w);
				wer.my_work <- first(w);
			
			} 
			
			// Create a simple organization
			organization o <- build_single_position_orga(sub_w);
			o.workers <- copy(sub_a);
			
			// Remove workers that have been assigned an organization
			a >>- sub_a;
		}
	}
	
	// Can add something before global init overloading the method
	action first_init {}
	// Can add something after global init overloading the method
	action last_init {}
	
	
	
	// ------------------------------------------- //
	// ------------------- SIM ------------------- //

	// BATCH MODE
	bool batch_mode <- false;
	
	string output_file <- model_source_folder+"../batch_output/"+res_file;
	string res_file <- "results.csv";
	string sobol_report <- model_source_folder+"../batch_output/"+sobol_file;
	string sobol_file <- "Sobol.txt";
	
	/*
	 * Trigger output computation and stop simulation if not batch mode
	 */
	reflex observ when:not(batch_mode) and stop_sim() {if not(empty(action_benchmark)) {do syso(sample(action_benchmark));} do pause;}
	
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
	
	map<string,float> action_benchmark <- [];
	
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
		if action_name != nil and benchmark_time > 0 { 
			action_benchmark[action_name] <- with_precision(benchmark_time/1000,2) 
				+ (action_benchmark contains_key action_name ? action_benchmark[action_name] : 0.0); 
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
		display "satisfaction" type:java2D {
			chart "satisfaction" type: series legend_font:font(0.0) series_label_position:none style:line {
				loop w over:worker {
					data "sat"+int(w) value:w._job_satisfaction color:rnd_color(int(w)/length(worker)*255);
				}
			}
		}
		display "satisfaction distribution" type:java2D {
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
		display "satisfaction x age" type:java2D {
			chart "satisfaction with age" type:histogram {
				data "<25" value:main_observer.aSat[pair<int,int>(0::25)] color:#gold;
				data ">=25<35" value:main_observer.aSat[pair<int,int>(25::35)] color:blend(#gold,#brown,1-1/5);
				data ">=35<45" value:main_observer.aSat[pair<int,int>(35::45)] color:blend(#gold,#brown,1-2/5);
				data ">=45<55" value:main_observer.aSat[pair<int,int>(45::55)] color:blend(#gold,#brown,1-3/5);
				data ">=55<65" value:main_observer.aSat[pair<int,int>(55::65)] color:blend(#gold,#brown,1-4/5);
				data ">=65" value:main_observer.aSat[pair<int,int>(65::MAX_AGE)] color:#brown;
			}
		}
		display "satisfaction x gender" type:java2D {
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
	 * TODO : bugged relfex when replication is disabled
	 */
	reflex end_batch {
		
		if int(first(simulations))=0 {
			save ["Sim id","end cycle","avr sat","s index","g index","a index",
				"sq1","sq2","sq3","sq4","sq5","sq6","sq7","sq8","sq9","sq10",
				"wmin","wavr","wmax","mmin","mavr","mmax",
				"a25","a35","a45","a55","a65","a"+MAX_AGE] 
			type:csv to:output_file rewrite:true header:false;
		}
		
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

