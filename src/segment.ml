open Point

type t = 
	{
		id : int; 
    mutable porig : Point.t; 
    mutable pdest : Point.t;
		mutable ci : float;
		mutable ce : float;
	}

type tpos = L | R | C

let id = ref 0

(* Création d'un nouveau segment *)
let new_segment xo yo xd yd = 
	id := !id + 1;
	{
		id = !id;
		porig = Point.new_point xo yo;
		pdest = Point.new_point xd yd;
		ci = 0.;
		ce = 1.;
	}

(* Copie un segment *)
let clone_segment seg = 
	{
		id = seg.id;
		porig = seg.porig;
		pdest = seg.pdest;
		ci = seg.ci;
		ce = seg.ce;
	}

(* Retourne une liste de segment à partir d'une liste de valeur *)
let new_segment_list list = 
	let rec sub list ls =  
		match list with
		| [] -> ls
		| (xo, yo, xd, yd):: l -> sub l ((new_segment xo yo xd yd)::ls)
	in sub list []

(* Renvoie la position du point p par rapport au segment s *)
let get_position point seg = 
	let z = ((seg.pdest.x-seg.porig.x)*(point.y-seg.porig.y))-((seg.pdest.y-seg.porig.y)*(point.x-seg.porig.x)) in
	(* Si une valeur est égale *)
	if z = 0 then
		(* Si la valeur 'x' est égale *)
		if (seg.pdest.x-seg.porig.x) = 0 then
			if point.x > seg.porig.x then
				R
			else if point.x < seg.porig.x then
				L 
			else
				C
		(* Si la valeur 'y' est égale *)
		else if (seg.pdest.y - seg.porig.y) = 0 then
			if point.y > seg.porig.y then
				R
			else  if point.y < seg.porig.y then
				L
			else 
				C
		else 
			C
	(* Si les valeurs sont différentes *)
	else
		if z > 0 then
			L
		else 
			R

let split_segment droite seg =	
	(* d = ((xb - xa) * (yd - yc) - (yb - ya) * (xd - xc)) *)
	let d = (seg.pdest.x-seg.porig.x)*(droite.pdest.y-droite.porig.y)-(seg.pdest.y-seg.porig.y)*(droite.pdest.x-droite.porig.x) in
	(* Le segment S et la droite D sont colinéaires *)
	if d = 0 then
		(* Le segment S est à droite de la droite D *)
		if get_position seg.porig droite = R then begin
			(None, Some seg)
		(* Le segment S est à gauche de la droite D *)
		end else begin
			(Some seg, None)
		end
	(* Il existe un point d'intersection des droites D et S *)
	else begin
  	(* r = ((ya - yc) * (xd - xc) - (xa - xc) * (yd - yc)) / d *)
  	let r = float_of_int ((seg.porig.y-droite.porig.y)*(droite.pdest.x-droite.porig.x)-(seg.porig.x-droite.porig.x)*(droite.pdest.y-droite.porig.y))/.float_of_int d in
		let segorig_tpos = get_position seg.porig droite in
		let segdest_tpos = get_position seg.pdest droite in
  	(* Le segment S est à droite de la droite D *)
  	if r < 0.0 then begin
  		if segdest_tpos = R then begin
				(None, Some seg)
			(* La partie du segment S est à droite de la droite D *)
			end else if segdest_tpos = L then begin
				(Some seg, None)
			(* Erreur *)
			end else begin
				print_string "Erreur split_segment r<0 : "; print_int seg.id; print_endline "";
				(None, None)
			end
  	(* Le segment S est à gauche de la droite D *)
  	end else if r > 1.0 then begin
  		if segorig_tpos = R then begin
				(None, Some seg)
			(* La partie du segment S est à droite de la droite D *)
			end else if segorig_tpos = L then begin
				(Some seg, None)
			(* Erreur *)
			end else begin
				print_string "Erreur split_segment r>1 : "; print_int seg.id; print_endline "";
				(None, None)
			end
  	(* Le segment S est coupé par la droite D *)
  	end else begin
  		(* La partie du segment S d'un seul coté de la droite D *)
  		if seg.ci >= r then begin
				(* La partie du segment S est à gauche de la droite D *)
  			if segdest_tpos = R then begin
					(None, Some seg)
				(* La partie du segment S est à droite de la droite D *)
				end else if segdest_tpos = L then begin
					(Some seg, None)
				end else begin
					print_string "Erreur split_segment 0<=r<=1 && seg.ci >= r : "; print_int seg.id; print_endline "";
					(None, None)
				end
  		(* La partie du segment S d'un seul coté de la droite D *)
  		end else if seg.ce <= r then begin
				(* La partie du segment S est à gauche de la droite D *)
				if segorig_tpos = R then begin
					(None, Some seg)
				(* La partie du segment S est à droite de la droite D *)
				end else if segorig_tpos = L then begin
					(Some seg, None)
				end else begin
					print_string "Erreur split_segment 0<=r<=1 && seg.ce <= r : "; print_int seg.id; print_endline "";
					(None, None)
				end
  		(* La partie du segment S est coupé par la droite D *)
  		end else begin
				if segorig_tpos = L then begin
					let seg_1 = clone_segment seg in
					let seg_2 = clone_segment seg in
					seg_1.ci <- seg.ci;
					seg_1.ce <- r;
					seg_2.ci <- r;
					seg_2.ce <- seg.ce;
    			(Some seg_1, Some seg_2)
				end else begin
					let seg_1 = clone_segment seg in
					let seg_2 = clone_segment seg in
					seg_1.ci <- r;
					seg_1.ce <- seg.ce;
					seg_2.ci <- seg.ci;
					seg_2.ce <- r;
    			(Some seg_1, Some seg_2)
				end
			end
		end
	end

let rec insert elem list = 
	match list with
	| [] -> [elem]
	| x :: l -> x :: insert elem l

let split hd rest = 
  let rec split_couple reste_list (list_gauche, list_droite) =
  	match reste_list with
		| [] -> (list_gauche, list_droite)
		| elem::reste -> 
			let (couple_gauche, couple_droite) = split_couple reste (list_gauche,list_droite) in
  		let (split_gauche, split_droite) = split_segment hd elem in
  			let g = 
    			match split_gauche with
    			| None -> couple_gauche
  				| Some elem -> elem::couple_gauche in
  			let d =
  				match split_droite with
    			| None -> couple_droite
  				| Some elem -> elem::couple_droite in
			(g,d)
	in 
	let l = split_couple rest ([], []) in
	match l with
	| ([], []) -> l
	| (g, d) -> l

(* Retourne la position d'origine x *)
let get_xo seg = 
	if seg.ci = 0. then
		seg.porig.x
	else
		seg.porig.x + int_of_float (ceil(seg.ci*.float_of_int(seg.pdest.x-seg.porig.x)))

(* Retourne la position d'origine y *)
let get_yo seg = 
	if seg.ci = 0. then
		seg.porig.y
	else
		seg.porig.y + int_of_float (ceil(seg.ci*.float_of_int(seg.pdest.y-seg.porig.y)))

(* Retourne la position de destination x *)
let get_xd seg = 
	if seg.ce = 0. then
		seg.pdest.x
	else
		seg.porig.x + int_of_float ( ceil(seg.ce*.float_of_int(seg.pdest.x-seg.porig.x)))

(* Retourne la position de destination y *)
let get_yd seg = 
	if seg.ce = 0. then
		seg.pdest.y
	else
		seg.porig.y + int_of_float ( ceil(seg.ce*.float_of_int(seg.pdest.y-seg.porig.y)))
		
(* Modifie la position ci *)
let seg_ci seg x y =
	let r = float_of_int(x-seg.porig.x) /. float_of_int(seg.pdest.x-seg.porig.x) in
		if (seg.pdest.x-seg.porig.x) <> 0 then begin
			seg.ci <- r
		end else begin
			let r = float_of_int(y-seg.porig.y) /. float_of_int(seg.pdest.y-seg.porig.y) in
			seg.ci <- r
		end;
		seg

(* Modifie la position ce *)
let seg_ce seg x y =
	let r = float_of_int(x-seg.porig.x) /. float_of_int(seg.pdest.x-seg.porig.x) in
		if (seg.pdest.x-seg.porig.x) <> 0 then begin
			seg.ce <- r
		end else begin
			let r = float_of_int(y-seg.porig.x) /. float_of_int(seg.pdest.y-seg.porig.x) in
			seg.ce <- r
		end;
		seg