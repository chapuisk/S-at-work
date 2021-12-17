/**
* Name: SobolAnalysis2
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model SobolAnalysis2

import "../Global.gaml"

global {
	string output_file <- "../../batch_output/Sobol_orga.csv";
}

experiment sobol_xplo_2 parent:abstract_batch type:batch until:world.stop_sim(){
	init {
		net_type <- nil;
	}
	
	parameter "gamma" var:default_gamma min:0.0 max:1.0;
	parameter "number of agent" var:nb_agent min:1000 max:5000;
	parameter "agent memory length" var:default_agent_memory min:1 max:10;
	
	parameter "number of contacts" var:default_nb_contacts min:1 max:20;
	parameter "network type" var:net_type_sobol min:1 max:3;
	
	method sobol sample:2000 outputs:["s_index_batch","a_index_batch","g_index_batch"];
}