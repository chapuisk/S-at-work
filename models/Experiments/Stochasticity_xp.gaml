/**
* Name: Stochasticityxp
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Stochasticityxp

import "../Global.gaml"

global {
	
	string model_source_folder <- "../";
	string res_file <- "stochastic_xp3.csv";
	bool DEBUG_MOD <- true;
	
	// Random weights of work characteristics
	float WC_WEIGHT_CONSTANT <- 0.0;
	
	int dummy_var;
}

experiment stochastic_xp parent:abstract_batch type:batch 
	repeat:50 until:world.stop_sim(){
	method explicit parameter_sets:list(range(0,39) collect (["dummy_var"::each]));	
}