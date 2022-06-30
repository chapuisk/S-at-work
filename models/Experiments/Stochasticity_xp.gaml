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
	string res_file <- "stochastic_xp_fixedWCWeights.csv";
	bool DEBUG_MOD <- true;
	
	int end_cycle_criterion <- 100;
	
	// Random weights of work characteristics
	float WC_WEIGHT_CONSTANT <- 1.0;
	
	int dummy_var;
}

experiment stochastic_xp parent:abstract_batch type:batch 
	repeat:1 until:world.stop_sim(){
	method explicit parameter_sets:list(range(0,39) collect (["dummy_var"::each]));	
}