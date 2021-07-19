/**
* Name: Observer
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Observer

import "Global.gaml"

global {
	
	float EPSILON <- 1e-05;
	
	list<list<float>> sats;
	int windows <- 5;
	
	reflex observe {
		if sats=nil { sats <- list_with(length(worker),list<float>([])); }
		ask worker { 
			sats[int(self)] <+ job_satisfaction;
			if length(sats[int(self)]) > windows { sats[] >- 0; }
		}
	}
	
	bool stop_sim(float epsilon <- EPSILON) {
		return sats none_matches (abs(min(each) - max(each)) > epsilon);
	}
	
}
