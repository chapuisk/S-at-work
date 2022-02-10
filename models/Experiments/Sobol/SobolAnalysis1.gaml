/**
* Name: SobolAnalysis
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/

model SobolAnalysis

import "../../Global.gaml"

global {
	
	string model_source_folder <- "../../";	
	bool default_rnd_wc_weights <- true; // Activate random weigths of work characteristics
	
	action init_organization {
		loop wer over:worker {
			// Create works according to agent 
			create work with:[
				salary::SALARY.get_numerical_value(wer._work_aspects[SALARY]),
				working_time_per_week::WORKING_TIME.get_numerical_value(wer._work_aspects[WORKING_TIME]),
				contract::wer._work_aspects[CONTRACT]
			] returns:w;
			wer.my_work <- first(w);
		} 
		// Create a simple organization
		organization o <- build_single_position_orga(list(work));
	}
	
}

experiment sobol_xplo_1 parent:abstract_batch type:batch until:world.stop_sim(){
	parameter "gamma" var:default_gamma min:0.0 max:1.0;
	parameter "number of agent" var:nb_agent min:1000 max:5000;
	parameter "agent memory length" var:default_agent_memory min:1 max:10;
	method sobol sample:4 outputs:["s_index_batch","a_index_batch","g_index_batch"] report:sobol_report results:output_file;
}



