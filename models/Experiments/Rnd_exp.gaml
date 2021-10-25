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
	
	bool DEBUG_MOD <- true;
	
	action init_workers {
		do read_default_data();
		do workforce_synthesis(nb_agent, demo_distribution);
	}
	
	action init_organization {
		organization o <- build_single_position_orga(worker collect (create_random_work(each)));
		o.workers <- list(worker);
	}
	
	/*
	 * Bind workers and jobs withtin an organization
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
	
	reflex observ when:not(batch_mode) { if stop_sim() {main_observer.triggered <- true; do pause;} }
	
}

experiment test parent:abstract_xp type:gui { }

experiment random_xplrt type:batch repeat:10 until:world.stop_sim() { 
	
	init {
		batch_mode <- true;
	}
	
	parameter default_agent_memory var:default_agent_memory init:5 min:5 max:20 step:5;
	parameter default_neu_rho var:default_neu_rho init:0.0 min:0.0 max:1.0 step:0.1;
	
	method exhaustive;
	
	permanent {
		display main {
			chart "outputs" type:series {
				data "s index" value:mean(simulations collect world.sat_dist_index(each.main_observer)) 
					y_err_values:standard_deviation(simulations collect world.sat_dist_index(each.main_observer)) color:#blue;
				data "g index" value:mean(simulations collect world.gender_index(each.main_observer)) 
					y_err_values:standard_deviation(simulations collect world.gender_index(each.main_observer)) color:#orange;
				data "a index" value:mean(simulations collect world.age_index(each.main_observer)) 
					y_err_values:standard_deviation(simulations collect world.age_index(each.main_observer)) color:#green;
			}
		}
	}
	
}