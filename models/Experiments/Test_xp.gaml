/**
* Name: Experiment
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Experiment

import "../Global.gaml"
import "../Characteristic.gaml"
import "../Worker.gaml"

global {
	
	string model_source_folder <- "../";
	bool DEBUG_MOD <- true;
	//bool DEBUG_WORKER <- true;
	string LEVEL <- debug_levels[1];
	
	int end_cycle_criterion <- 100;

	/*
	 * Bind workers and jobs withtin an organization
	 * ==UNUSED==
	 */
	action bind_worker_and_job(organization org, list<worker> workers,
		map<characteristic,int> age_dist,
		map<characteristic,int> educ_dist,
		map<characteristic,int> male_dist
	) {
		list<work> jobs <- org.orga accumulate (each.pWorks);
		map<work,float> job_salary <- jobs as_map (each::each.salary/SALARY.max);
		map<work,float> job_time <- jobs as_map (each::each.working_time_per_week/WORKING_TIME.max);
		map<work,float> job_contract <- jobs as_map (each::contract_types_weights[each.contract]);
		
		loop w over:workers {
			map<characteristic,float> weights;
			loop c over:[SALARY,WORKING_TIME,CONTRACT] {
				weights[c] <- AGE.get_numerical_value(w._demographics[AGE]) * age_dist[c] +
					(w._demographics[GENDER]="M"?1:-1) * male_dist[c] + 
					EDUCATION.get_numerical_value(w._demographics[EDUCATION]) * educ_dist[c];
			} 
			w.my_work <- rnd_choice(jobs as_map (each::
				job_salary[each]*weights[SALARY]+job_time[each]*weights[WORKING_TIME]+job_contract[each]*weights[CONTRACT]
			));
			org.workers <+ w;
		}
	}
	
	bool peakEndMod <- true;
	bool socCompMod <- true;
	bool warrMod <- true;
	bool wowaMod <- true;
	
	//int observer_window <- 1;
	
	// Can add something after global init overloading the method
	action last_init {
		ask worker {
			do socio_cognitive_evaluation_of_work(__update_work_eval_criterias, __update_social_references, __nb_interactions_with_social_references);
			_job_satisfaction <- peakEndMod ? ((peak_memory<0?_cognitive_resp:peak_memory) + _cognitive_resp) / 2 : _cognitive_resp;
		}
	}
	
}

experiment test parent:abstract_xp type:gui { }

experiment random_xplrt type:batch repeat:10 until:world.stop_sim() { 
	
	init {
		batch_mode <- true;
	}
	
	parameter default_agent_memory var:default_agent_memory init:5 min:5 max:20 step:5;
	parameter default_neu_rho var:default_neu_rho init:0.0 min:0.0 max:1.0 step:0.1;
	
	parameter default_nb_contacts var:default_nb_contacts init:0 min:0 max:50 step:5;
	parameter default_nb_wchar_ex var:default_nb_wchar_ex init:1 min:1 max:3;
	
	method exhaustive;
	
	permanent {
		display main {
			chart "outputs" type:series {
				data "s index" legend:string(default_agent_memory)+"|"+default_neu_rho value:mean(simulations collect each.s_index) 
					y_err_values:[simulations min_of each.s_index,simulations max_of each.s_index] color:#blue;
				data "g index" legend:string(default_agent_memory)+"|"+default_neu_rho value:mean(simulations collect each.g_index) 
					y_err_values:standard_deviation(simulations collect each.g_index) color:#orange;
				data "a index" legend:string(default_agent_memory)+"|"+default_neu_rho value:mean(simulations collect each.a_index) 
					y_err_values:standard_deviation(simulations collect each.a_index) color:#green;
			}
		}
	}
	
}

experiment cali parent:abstract_batch type:batch repeat:1 keep_seed:true until:world.stop_sim() {
	
	init { batch_mode <- true; }
	
	//method tabu maximize: avr_sat_batch iter_max: 10 tabu_list_size: 5;
	method genetic pop_dim: 6 crossover_prob: 0.7 mutation_prob: 0.1 improve_sol: true stochastic_sel: true
	nb_prelim_gen: 1 max_gen: 50  maximize: fit_batch  aggregation: "avr";
	
}