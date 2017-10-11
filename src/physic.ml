open Segment
open Point

exception Collision
	
(* Collision de 2 segments *)
let detect_collision_segment s bsp =
	try 
		Bsp.iter (fun seg ->
  		(* d = ((xb - xa) * (yd - yc) - (yb - ya) * (xd - xc)) *)
  		let d = (seg.pdest.x-seg.porig.x)*(s.pdest.y-s.porig.y)-(seg.pdest.y-seg.porig.y)*(s.pdest.x-s.porig.x) in
  		if d <> 0 then begin
    		(* r = ((ya - yc) * (xd - xc) - (xa - xc) * (yd - yc)) / d *)
				let r = float_of_int ((seg.porig.y-s.porig.y)*(s.pdest.x-s.porig.x)-(seg.porig.x-s.porig.x)*(s.pdest.y-s.porig.y))/.float_of_int d in
				if 0. <= r  && r <= 1. then begin
					(* d = ((xb - xa) * (yd - yc) - (yb - ya) * (xd - xc)) *)
  				let d = (s.pdest.x-s.porig.x)*(seg.pdest.y-seg.porig.y)-(s.pdest.y-s.porig.y)*(seg.pdest.x-seg.porig.x) in
      		if d <> 0 then begin
        		(* r = ((ya - yc) * (xd - xc) - (xa - xc) * (yd - yc)) / d *)
    				let r = float_of_int ((s.porig.y-seg.porig.y)*(seg.pdest.x-seg.porig.x)-(s.porig.x-seg.porig.x)*(seg.pdest.y-seg.porig.y))/.float_of_int d in
    				if 0. <= r  && r <= 1. then begin
        			raise Collision
        		end else 
        			()
    			end else
    				()
    		end else 
    			()
			end else
				()
			
  	) bsp;
		false
	with
	| Collision -> true
