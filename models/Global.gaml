/**
* Name: Global
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Global

global {
	
	// CONSTANT
	float EPSILON <- 1e-6;
	
	// INIT
	action first_init {}
	action last_init {}
	
	//========//
	// LOGGER //
	//========//
	
	bool DEBUG_MOD <- false;
	string LEVEL <- "DEBUG";
	list<string> debug_levels <- ["TRACE","DEBUG","WARNING","ERROR"];
	
	action syso(string msg, float benchmark_time <- -1.0, agent caller <- self, string action_name <- nil, string level <- LEVEL) {
		if DEBUG_MOD and (debug_levels index_of level >= debug_levels index_of LEVEL) {
			string m <- "["+caller.name+(action_name=nil?"":"|"+action_name)+"]"
				+(benchmark_time>0.0?"[TM="+with_precision(benchmark_time/1000,3)+"s] ":" ")+msg;
			switch level {
				match_one ["TRACE","DEBUG"] {write m;}
				match "WARNING" { write m color:#orange; }
				match "ERROR" { do err(msg,caller,action_name); }
			}
		}
	}
	
	string debug_level(int level <- 1) {return level < length(debug_levels) ? debug_levels[level] : LEVEL;}
	action err(string msg, agent caller <- self, string action_name <- nil) { error "["+caller.name+(action_name=nil?"":"|"+action_name)+"] "+msg; }
	
}

