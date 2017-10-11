open Player
open Point

(* Affiche le joueur *)
let render_player scale player border win_w win_h x y =
	let win_w = (Graphics.size_x ())/scale in
	let l = win_w/8 in
	let d = int_of_float ((float_of_int l /. 2.) /. Trigo.dtan (Options.fov/2)) in
	let pcos = Trigo.dcos (player.pa) in
	let psin = Trigo.dsin (player.pa) in
	
	let player_x = (player.pos.x / scale) + border - x in
	let player_y = (player.pos.y / scale) + border - y in
  let x = player_x + d in
  let gauche_y = player_y + (l/2) in
	let droite_y = player_y - (l/2) in
  let gauche_point = (Point.new_point
    						(player_x + int_of_float ((float_of_int (x-player_x)*.pcos)-.(float_of_int(gauche_y-player_y)*.psin)))
      					(player_y + int_of_float ((float_of_int (gauche_y-player_y)*.pcos)+.(float_of_int(x-player_x)*.psin)))) in
  let droite_point = (Point.new_point
    						(player_x + int_of_float ((float_of_int (x-player_x)*.pcos)-.(float_of_int(droite_y-player_y)*.psin)))
      					(player_y + int_of_float ((float_of_int (droite_y-player_y)*.pcos)+.(float_of_int(x-player_x)*.psin)))) in
	let arc_x = player_x + d in
	let arc_y = player_y in
	let arc_point = (Point.new_point
    						(player_x + int_of_float ((float_of_int (arc_x-player_x)*.pcos)-.(float_of_int(arc_y-player_y)*.psin)))
      					(player_y + int_of_float ((float_of_int (arc_y-player_y)*.pcos)+.(float_of_int(arc_x-player_x)*.psin)))) in
								
	Graphics.set_color Graphics.red;
	
	if player_x > 0 && player_y > 0 && player_x < win_w && player_y < win_h then begin
  	Graphics.moveto player_x player_y;
  	Graphics.lineto gauche_point.x gauche_point.y;
  	
  	Graphics.moveto player_x player_y;
  	Graphics.lineto droite_point.x droite_point.y;
  	
  	Graphics.draw_arc arc_point.x arc_point.y (l/2) (l/2) (270+player.pa) (90+player.pa);
	end;

	if Options.debug then begin
  	print_endline "##################### Render_player_2D ###################";
  	print_string "pcos : "; print_float pcos; print_endline "";
  	print_string "psin : "; print_float psin; print_endline "";
  	print_string "x : "; print_int x; print_endline "";
  	print_string "gauche_y : "; print_int gauche_y; print_endline "";
  	print_string "droite_y : "; print_int droite_y; print_endline "";
  	
  	print_string "gauche_point_x : "; print_int gauche_point.x; print_endline "";
  	print_string "gauche_point_y : "; print_int gauche_point.y; print_endline "";
  	print_string "droite_point_x : "; print_int droite_point.x; print_endline "";
  	print_string "droite_point_y : "; print_int droite_point.y; print_endline ""
	end else 
		()

(* Affiche les murs *)
let render_map bsp scale border win_w win_h x y =	
	Graphics.set_color Graphics.black;
	Bsp.iter (fun seg ->
		let orig_x = (Segment.get_xo seg)/scale + border - x in
		let orig_y = (Segment.get_yo seg)/scale + border - y in
		
		let dest_x = (Segment.get_xd seg)/scale + border - x in
		let dest_y = (Segment.get_yd seg)/scale + border - y in
		
		
		Graphics.moveto (Pervasives.min orig_x win_w) (Pervasives.min orig_y win_h);
		Graphics.lineto (Pervasives.min dest_x win_w) (Pervasives.min dest_y win_h);
		
		if Options.debug then begin
			print_endline "####################### Display_2D : Segment #######################";
			print_string "xo : "; print_int ((Segment.get_xo seg)/scale); print_endline "";
			print_string "yo : "; print_int ((Segment.get_yo seg)/scale); print_endline "";
			print_string "xd : "; print_int ((Segment.get_xd seg)/scale); print_endline "";
			print_string "yd : "; print_int ((Segment.get_yd seg)/scale); print_endline ""
		end else ()
	) bsp

(* Affiche la map en 2D *)
let display bsp p scale border =
	let win_w = (Options.win_w/scale) in
	let win_h = (Options.win_h/scale) in
	let max_x = win_w + border in
	let max_y = win_h + border in
	
	Graphics.set_color Graphics.red;
	Graphics.fill_rect 0 0 (max_x+border) (max_y+border);
	
	Graphics.set_color Graphics.white;
	Graphics.fill_rect border border win_w win_h;
	
	let x = int_of_float (floor(float_of_int p.pos.x /. float_of_int Options.win_w)) * win_w in
	let y = int_of_float (floor(float_of_int p.pos.y /. float_of_int Options.win_h)) * win_h in
	
	render_map bsp scale border max_x max_y x y;
	render_player scale p border win_w win_h x y