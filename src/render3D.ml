open Player
open Point
open Segment
open Bsp
open Graphics

let translation_rotation bsp p =
	let pcos = Trigo.dcos (-p.pa) in
	let psin = Trigo.dsin (-p.pa) in
		let rec sub bsp =
    	match bsp with
    	| E -> E
    	| N (bsp_seg, bsp_g, bsp_d) ->
				bsp_seg.porig <- (Point.new_point
    						(int_of_float ((float_of_int (bsp_seg.porig.x-p.pos.x)*.pcos)-.(float_of_int(bsp_seg.porig.y-p.pos.y)*.psin)))
      					(int_of_float ((float_of_int (bsp_seg.porig.y-p.pos.y)*.pcos)+.(float_of_int(bsp_seg.porig.x-p.pos.x)*.psin))));
				bsp_seg.pdest <- (Point.new_point
      					(int_of_float ((float_of_int (bsp_seg.pdest.x-p.pos.x)*.pcos)-.(float_of_int(bsp_seg.pdest.y-p.pos.y)*.psin)))
      					(int_of_float ((float_of_int (bsp_seg.pdest.y-p.pos.y)*.pcos)+.(float_of_int(bsp_seg.pdest.x-p.pos.x)*.psin))));
					N (bsp_seg, sub bsp_g, sub bsp_d)
	in sub bsp
	
let clipping bsp =
		let rec sub bsp =
    	match bsp with
    	| E -> E
    	| N (bsp_seg, bsp_g, bsp_d) ->
					let xo = Segment.get_xo bsp_seg in
					let yo = Segment.get_yo bsp_seg in
					let xd = Segment.get_xd bsp_seg in
					let yd = Segment.get_yd bsp_seg in
					(* Le segment est entierement à l'extérieur du champ de vision *)
					if (xo < Options.xmin && xd < Options.xmin) || (xo > Options.xmax && xd > Options.xmax) then begin
						bsp_seg.ci <- 0.;
						bsp_seg.ce <- 0.;
						N (bsp_seg, sub bsp_g, sub bsp_d)
					(* Le segment est à l'intérieur du champ de vision *)
					end else begin
						let ta = float_of_int(yd-yo) /. float_of_int(xd-xo) in
						(* Le point d'origine du segment est à l'extérieur du champ de vision *)
						if xo < Options.xmin then begin
						 	let xi = Options.xmin in
							let yi = yo + int_of_float (float_of_int(xi-xo) *. ta) in
							N ((Segment.seg_ci bsp_seg xi yi) , sub bsp_g, sub bsp_d)
						(* Le point de destination du segment est à l'extérieur du champ de vision *)
						end else if xd < Options.xmin then begin
							let xi = Options.xmin in
							let yi = yd + int_of_float (float_of_int(xi-xd) *. ta) in
							N ((Segment.seg_ce bsp_seg xi yi) , sub bsp_g, sub bsp_d)
						(* Le point d'origine du segment est trop loin *)
						end else if xo > Options.xmax then begin
						 	let xi = Options.xmax in
							let yi = yo + int_of_float (float_of_int(xi-xo) *. ta) in
							N ((Segment.seg_ci bsp_seg xi yi) , sub bsp_g, sub bsp_d)
						(* Le point de destination du segment  est trop loin *)
						end else if xd > Options.xmax then begin
							let xi = Options.xmax in
							let yi = yd + int_of_float (float_of_int(xi-xd) *. ta) in
							N ((Segment.seg_ce bsp_seg xi yi) , sub bsp_g, sub bsp_d)
						(* Le segment est entierement à l'intérieur du champ de vision *)
						end else begin
							N (bsp_seg, sub bsp_g, sub bsp_d)
						end
					end
	in sub bsp

(* Render 3D *)
let display_background () =
	Graphics.set_color Graphics.cyan;
	Graphics.fill_rect 0 ((Graphics.size_y ())/2) (Graphics.size_x ()) (Graphics.size_y ());
	
	Graphics.set_color (Graphics.rgb 123 123 123);
	Graphics.fill_rect 0 0 (Graphics.size_x ()) ((Graphics.size_y ())/2)
	

let display_map bsp player =
	
	let d = int_of_float ((float_of_int (Graphics.size_x ()) /. 2.) /. Trigo.dtan (Options.fov/2)) in
	Bsp.rev_parse (fun seg ->
			if seg.ci <> seg.ce then begin
				let fenetre = (float_of_int (Graphics.size_y ())/. 2.) in
				let ph = Player.get_view player in
				
				let vorig_x = int_of_float ((float_of_int (Graphics.size_x ()) /. 2.) -. (float_of_int((Segment.get_yo seg)*d) /. float_of_int (Segment.get_xo seg))) in
  			let	vdest_x = int_of_float ((float_of_int (Graphics.size_x ()) /. 2.) -. (float_of_int((Segment.get_yd seg)*d) /. float_of_int (Segment.get_xd seg))) in
				
				let vorig_ceiling = int_of_float (fenetre +. (float_of_int((Options.ceiling_h - ph)*d)/.float_of_int (Segment.get_xo seg))) in
				let vorig_floor = int_of_float (fenetre +. (float_of_int((Options.floor_h - ph)*d)/.float_of_int (Segment.get_xo seg))) in
				let vdest_ceiling = int_of_float (fenetre +. (float_of_int((Options.ceiling_h - ph)*d)/.float_of_int (Segment.get_xd seg))) in
				let vdest_floor = int_of_float (fenetre +. (float_of_int((Options.floor_h - ph)*d)/.float_of_int (Segment.get_xd seg))) in
				
				Graphics.set_color Graphics.black;
				Graphics.fill_poly [|
					vorig_x,vorig_floor;
					vorig_x,vorig_ceiling;
					vdest_x,vdest_ceiling;
					vdest_x,vdest_floor|];
  			
  			Graphics.set_color Graphics.red;
  			Graphics.moveto vorig_x vorig_floor;
  			Graphics.lineto vorig_x vorig_ceiling;
  			Graphics.lineto vdest_x vdest_ceiling;
  			Graphics.lineto vdest_x vdest_floor;
  			Graphics.lineto vorig_x vorig_floor;
				
				if Options.debug then begin
    			print_endline "####################### Render 3D : Segment #######################";
    			print_string "id : "; print_int seg.id; print_endline "";
    			
    			print_string "xo : "; print_int seg.porig.x; print_endline "";
      		print_string "yo : "; print_int seg.porig.y; print_endline "";
      		print_string "xd : "; print_int seg.pdest.x; print_endline "";
      		print_string "yd : "; print_int seg.pdest.y; print_endline "";
    			
    			print_string "ci : "; print_float seg.ci; print_endline "";
      		print_string "ce : "; print_float seg.ce; print_endline "";
    			
    			print_string "get_xo : "; print_int (Segment.get_xo seg); print_endline "";
      		print_string "get_yo : "; print_int (Segment.get_yo seg); print_endline "";
      		print_string "get_xd : "; print_int (Segment.get_xd seg); print_endline "";
      		print_string "get_yd : "; print_int (Segment.get_yd seg); print_endline "";
    			
    			print_string "seg.vorig_x : "; print_int vorig_x; print_endline "";
    			print_string "seg.vorig_floor : "; print_int vorig_floor; print_endline "";
    			print_string "seg.vdest_ceiling : "; print_int vdest_ceiling; print_endline "";
    			
    			print_string "seg.vdest_x : "; print_int vdest_x; print_endline "";
    			print_string "seg.vdest_floor : "; print_int vdest_floor; print_endline "";
    			print_string "seg.vdest_ceiling : "; print_int vdest_ceiling; print_endline ""
				end else ()
			end else begin
				if Options.debug then begin
					print_endline "####################### Render 3D : Supprimer #######################";
  				print_string "Segment ID : "; print_int seg.id; print_endline ""
				end else ()
			end
	) bsp (Point.new_point 0 0)

let display bsp player =
	let rotation = translation_rotation (Bsp.clone_bsp bsp) player in
	let clip = clipping rotation in
	
	display_background ();
	display_map clip player