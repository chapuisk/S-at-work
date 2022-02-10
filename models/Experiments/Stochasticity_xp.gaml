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
	string output_file <- "../../batch_output/stochastic_xp.csv";
	bool DEBUG_MOD <- true;
	
	int dummy_var <- 0;
}

experiment stochastic_xp parent:abstract_batch type:batch 
	repeat:2000 until:world.stop_sim(){	}