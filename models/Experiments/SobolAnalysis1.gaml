/**
* Name: SobolAnalysis
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/

model SobolAnalysis

import "../Global.gaml"

global {
	string output_file <- "../../batch_output/Sobol_zero.csv";
	bool DEBUG_MOD <- true;
	
	action init_organization {
		organization o <- build_single_position_orga(worker collect (create_random_work(each)));
		o.workers <- list(worker);
	}
	
}

experiment sobol_xplo_1 parent:abstract_batch type:batch until:world.stop_sim(){
	parameter "gamma" var:default_gamma min:0.0 max:1.0;
	parameter "number of agent" var:nb_agent min:1000 max:5000;
	parameter "agent memory length" var:default_agent_memory min:1 max:10;
	method sobol sample:2000 outputs:["s_index_batch","a_index_batch","g_index_batch"];
}



