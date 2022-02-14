/**
* Name: Organization
* Based on the internal empty template. 
* Author: kevinchapuis
* Tags: 
*/


model Organization

import "Work.gaml"
import "Worker.gaml"

global {
	
	string VERTICAL <- "vertical";
	string HORIZONTAL <- "horizontal";
	int DEFAULT_VERTICAL_DISTANCE_UNIT <- 2;
	int DEFAULT_HORIZONTAL_DISTANCE_UNIT <- 1;
	
	// SOME BASIC INIT STUFF
	// INSEE Data on the distribution of french firms according to the number of employees
	map<pair<int,int>,float> orga_sizes <- [(1::9)::1028.1,(10::49)::172.6,(50::99)::18.1,
			(100::249)::10.8,(250::nb_agent>250?nb_agent:250)::6.3
	];
	
	/*
	 * Build an organization from its hierarchy
	 * <p>
	 * layers - list : the list of layers, made of list of position, each described by a list of works </br>
	 * connections - list : the list of source::destination pair of organizational links </br>
	 */
	organization build_orga_hierarchy(list<list<list<work>>> layers, list<pair<point,point>> connections) {
		
		// Create the organization
		create organization with:[workers::[],orga::[],hierarchy::graph([])] returns:orgas;
		organization o <- first(orgas);
		
		// Store graph location vs orga position
		map<point,position> loc_to_pos;
		
		// Create layers
		loop l from:0 to:length(layers)-1 {
			list<list<work>> positions <- layers[l];
			loop p from:0 to: length(positions)-1 {
				list<work> works <- positions[p];
				create position with:[
					pWorks::works,
					pTasks::works accumulate (each.tasks),
					location::{l,p}
				] { ask o {do add_position(myself);} loc_to_pos[{l,p}] <- self;}
			}
		}
		
		// Create graph of organizational link
		loop c over:connections {
			position s <- loc_to_pos[c.key];
			position d <- loc_to_pos[c.value];
			if s.location.x < d.location.x { position tmp <- s; s <- d; d <- tmp;}
			string n <- s.location.x=d.location.x?HORIZONTAL:VERTICAL;
			create orga_link with:[
				source::s,destination::d, nature::n, 
				orga_distance::n=VERTICAL?DEFAULT_VERTICAL_DISTANCE_UNIT:DEFAULT_HORIZONTAL_DISTANCE_UNIT
			] { ask o {do add_connection(myself);}}	
		}
		
		return o;
	} 
	
	// Build the simplest possible oragnization made of a given list of works
	organization build_single_position_orga(list<work> works) {
		organization o <- build_orga_hierarchy([[works]],list<pair<point,point>>([]));
		loop w over:works { w.org <- o; }
		return o;
	}
	
	// Build the simplest possible organization made of 'n' works
	organization build_random_single_position_orga(int nb_work <- 1, int nb_tasks <- 0) {
		list<work> ws;
		loop times:nb_work { ws <+ create_simple_work(nb_tasks); } 
		return build_single_position_orga(ws);
	}
	
	/*
	 * Build a pyramidal organization based on simple parameters: <p>
	 * layers - int : the number of hierarchical layers </br>
	 * ground_layer_size - int : the number of position for the lowest layer </br>
	 * sharpness - int : how sharp hierarchical slope is (i.e. how thin is each upper layer knowing the previous one) </br>
	 * horizontal_connectivity - float : the probability to connect adjacent position in a layer </br>
	 * vertical_connectivity - float : the probability to connect positions of two consecutive layers </br>
	 */
	organization build_pyramidal_organization(int layers, int ground_layer_size, 
		int sharpness, float horizontal_connectivity, float vertical_connectivity
	) {
		// Layers
		list<list<list<work>>> orga_layers <- [];
		// Construct layers
		int previous_layer_size <- layers;
		orga_layers <+ list_with(ground_layer_size,[create_simple_work(0)]);
		loop l from:1 to:layers-1 {
			int l_size <- round((1 - (1.0 * layers / l)^sharpness) * previous_layer_size);
			if l_size = 0 { orga_layers <+ [[create_simple_work(0)]]; }
			else { orga_layers <+ list_with(l_size,[create_simple_work(0)]);}
		}
		
		// Connections
		list<pair<point,point>> orga_connections <- [];
		// Connections within layers
		loop l over:orga_layers {
			int il <- orga_layers index_of l;
			if length(l) > 1 {
				// If there is more than 1 position for curret layer, build horizontal connections
				loop ip from:0 to:length(l)-2 {
					if flip(horizontal_connectivity) { orga_connections <+ point(il,ip)::point(il,ip+1); }
				}
			}
		}
		// Connections between layers
		list<list<work>> current_l <- last(orga_layers);
		int hl <- length(orga_layers)-1;
		if hl > 1 {
	 		loop il from:2 to:hl {
				list<list<work>> sub_layer <- orga_layers[hl-il];
				loop up from:0 to:length(current_l)-1 { 
					loop down from:0 to:length(sub_layer)-1 { 
						if flip(il=2?1.0:vertical_connectivity) {orga_connections <+ point(hl-1,up) :: point(hl-il,down);}
					}
				} 
				current_l <- sub_layer;
			}
		}
		
		// Build orga
		return build_orga_hierarchy(orga_layers,orga_connections);
	}
	
}

species organization {
	
	list<worker> workers;
	list<position> orga;
	
	graph hierarchy;
	
	// --------------------------------- //
	
	// REFLEXES
	
	/*
	 * Whenever something happen in/out the organization, it revent things on employees
	 */
	reflex oranizational_events {  }
	
	// --------------------------------- //
	
	// ACTIONS
	
	// The hierarchical level of worker 'w'
	int get_hierarchy_level(worker w) {
		return int(get_position(w.my_work).shape.location.y);
	}
	
	// The organizational distance between job 'w1' and job 'w2'
	int get_distance(work w1, work w2) {
		if length(orga)=1 {return DEFAULT_HORIZONTAL_DISTANCE_UNIT;}
		position p1 <- get_position(w1);
		position p2 <- get_position(w2);
		if p1 = p2 {return DEFAULT_HORIZONTAL_DISTANCE_UNIT;} 
		list<orga_link> p <- ( hierarchy path_between (p1.location,p2.location) )
			.edges collect (get_link_from_points(first(each.points),last(each.points))); 
		return sum(p accumulate (each.orga_distance));
	}
	
	// Get the organizational position of the given job 'w'
	position get_position(work w) { 
		return orga first_with (each.pWorks contains_key w);
	}
	
	orga_link get_link(position p1, position p2) {
		return hierarchy contains_edge (p1.location::p2.location) ? nil : (orga_link first_with (each.source=p1 and each.destination=p2));
	}
	
	orga_link get_link_from_points(point p1, point p2) {
		return hierarchy contains_edge (p1::p2) ? nil : (orga_link first_with (each.source.location=p1 and each.destination.location=p2));
	}
	
	// Builder
	action add_position(position p) { orga <+ p; hierarchy <- hierarchy add_node p.location; }
	action add_connection(orga_link l) {
		hierarchy <- hierarchy add_edge (l.source::l.destination); 
		if l.nature = HORIZONTAL {hierarchy <- hierarchy add_edge (l.destination::l.source);}
	}
	
}

species position {
	
	point location <- {0,0};
	list<task> pTasks;
	list<work> pWorks;
	
}

species orga_link {
	position source;
	position destination;
	
	string nature;
	int orga_distance;
}