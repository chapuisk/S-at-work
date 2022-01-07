/**
* Name: Stochasticityxp
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Stochasticityxp

import "../Global.gaml"

global {
	string output_file <- "../../batch_output/stochastic_xp.csv";
	bool DEBUG_MOD <- true;
	
	int dummy_var <- 0;
}

experiment stochastic_xp parent:abstract_batch type:batch 
	repeat:50 until:world.stop_sim(){
	method explicit parameter_sets:list(range(0,39) collect (["dummy_var"::each]));	
}