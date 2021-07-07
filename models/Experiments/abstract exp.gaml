/**
* Name: abstractexp
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model abstractexp

import "../Worker.gaml"

global {
	
	init {
		do first_init;
		do init_characteristics;
		do init_workers;
		do init_organization;
		do last_init;
	}
	
	action init_characteristics {
		do init_demographic_characteristic;
		do init_basic_work_carac;
	}
	
	action init_workers virtual:true;
	
	action init_organization virtual:true;
	
}

experiment "abstract_xp" virtual:true type:gui {
	output {
		display "satisfaction" type: opengl {
			chart "satisfaction" type: series legend_font:font(0.0) series_label_position:none style:line {
				loop w over:worker {
					data "sat"+int(w) value:w.job_satisfaction color:rnd_color(int(w)/length(worker)*255);
				}
			}
		}
	}
}
