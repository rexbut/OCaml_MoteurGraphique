open Segment

type t = E | N of Segment.t * t * t 

(* Copie un BSP *)
let clone_bsp bsp =
		let rec sub bsp =
  	match bsp with
  	| E -> E
  	| N (bsp_seg, bsp_g, bsp_d) -> N((Segment.clone_segment bsp_seg), (sub bsp_g), (sub bsp_d))
	in sub bsp

let parse f bsp p =
	let rec parse_sub bsp =
  	match bsp with
  	| E -> ()
  	| N (bsp_seg, bsp_g, bsp_d) -> begin
  		if Segment.get_position p bsp_seg = L then 
				begin
    			parse_sub bsp_g;
					f bsp_seg;
  				parse_sub bsp_d
				end 
			else 
				begin
    			parse_sub bsp_d;
					f bsp_seg;
  				parse_sub bsp_g
  			end
		end
	in parse_sub bsp

let rev_parse f bsp p =
		let rec parse_sub bsp =
  	match bsp with
  	| E -> ()
  	| N (bsp_seg, bsp_g, bsp_d) -> begin
  		if Segment.get_position p bsp_seg = L then 
				begin
    			parse_sub bsp_d;
					f bsp_seg;
  				parse_sub bsp_g
  			end
			else 
				begin
    			parse_sub bsp_g;
					f bsp_seg;
  				parse_sub bsp_d
				end 
		end
	in parse_sub bsp

let iter f bsp = 
	 let rec iter_sub bsp =
  	match bsp with
  	| E -> ()
  	| N (bsp_seg, bsp_g, bsp_d) -> begin
				f bsp_seg;
  			iter_sub bsp_d;
				iter_sub bsp_g
		end
	in iter_sub bsp

let build_bsp sl =
	let rec build_sub s lgd =
  	match lgd with
  	| ([], []) -> N (s, E, E)
		| (g::lg, []) -> N (s, (build_sub g (Segment.split g lg)), E)
		| ([], d::ld) -> N (s, E, (build_sub d (Segment.split d ld)))
		| (g::lg, d::ld) -> N (s, (build_sub g (Segment.split g lg)), (build_sub d (Segment.split d ld)))
	in 
	match sl with
	| [] -> E
	| s::l -> build_sub s (Segment.split s l)
