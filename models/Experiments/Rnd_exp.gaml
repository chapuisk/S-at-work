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
	string LEVEL <- debug_level(0);
	int nb_agent init:5 parameter:true;
	
	action init_workers {
		do read_default_data();
		do workforce_synthesis(nb_agent, demo_distribution);
	}
	
	action init_organization {
		do build_single_position_orga(10);
		do bind_worker_and_job(first(organization),list(worker),
			[SALARY::4,WORKING_TIME::1,CONTRACT::2], // age distribution
			[SALARY::2,WORKING_TIME::1,CONTRACT::1], // education distribution
			[SALARY::1,WORKING_TIME::-1,CONTRACT::0] // gender distribution
		);
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
				weights[c] <- AGE.get_numerical_value(w.demographics[AGE]) * age_dist[c] +
				(w.demographics[GENDER]="M"?1:-1) * male_dist[c] + 
				EDUCATION.get_numerical_value(w.demographics[EDUCATION]) * educ_dist[c];
			} 
			w.my_work <- rnd_choice(jobs as_map (each::
				job_salary[each]*weights[SALARY]+job_time[each]*weights[WORKING_TIME]+job_contract[each]*weights[CONTRACT]
			));
			org.workers <+ w;
		}
	}
	
}

experiment test parent:abstract_xp type:gui { }