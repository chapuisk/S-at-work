/**
* Name: Stochasticityxp
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Stochasticityxp

import "../Global.gaml"

global {
	string output_file <- "../../batch_output/results.csv";
	bool DEBUG_MOD <- true;
}

experiment stochastic_xp parent:abstract_batch type:batch 
	repeat:4 until:world.stop_sim(){}