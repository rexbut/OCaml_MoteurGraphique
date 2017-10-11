open Bsp
open Player
open Point
open Segment

type t = 
	{
  	mutable minimap : bool;
  	mutable mode : Options.tmode;
		mutable info : bool;
	}

let new_render () = 
	{
		minimap = Options.minimap;
		mode = Options.mode;
		info = Options.info
	}

let display_info render bsp p =
	let sneak = "Sneak = " in
	let (sneak_x, sneak_y) = Graphics.text_size sneak in
	let sneak_valeur = 
		if p.sneak then
			"Activé"
		else
			"Désactivé" 
	in
	let (sneak_valeur_x, sneak_valeur_y) = Graphics.text_size sneak_valeur in
	
	let run = "Run = " in
	let (run_x, run_y) = Graphics.text_size run in
	let run_valeur = 
		if p.run then
			"Activé"
		else
			"Désactivé" 
	in
	let (run_valeur_x, run_valeur_y) = Graphics.text_size run_valeur in
	let margin = 10 in
	
	let color = 
		if render.mode = Options.TwoD then
			Graphics.black
		else 
			Graphics.white
	in
	
	Graphics.set_color Graphics.red;
	Graphics.set_font "Arial Black";
	
	(* Sneak *)
	Graphics.moveto ((Graphics.size_x ())-sneak_x-sneak_valeur_x-margin) margin;
	Graphics.set_color color;
	Graphics.draw_string sneak;
	if p.sneak then
		Graphics.set_color Graphics.green
	else
		Graphics.set_color Graphics.red;
	Graphics.moveto ((Graphics.size_x ())-sneak_valeur_x-margin) margin;
	Graphics.draw_string sneak_valeur;
	
	(* Run *)
	Graphics.moveto ((Graphics.size_x ())-run_x-run_valeur_x-margin) (margin+sneak_y);
	Graphics.set_color color;
	Graphics.draw_string run;
	if p.run then
		Graphics.set_color Graphics.green
	else
		Graphics.set_color Graphics.red;
	Graphics.moveto ((Graphics.size_x ())-run_valeur_x-margin) (margin+sneak_y);
	Graphics.draw_string run_valeur 

let display render bsp p =
	if render.mode = Options.TwoD then
		Render2D.display bsp p 1 0
	else begin
		Render3D.display bsp p;
		if render.minimap then
			Render2D.display bsp p Options.scale 4
		else
			()
	end;
	if render.info then
		display_info render bsp p
	else 
		()

(* Active ou désactive la minimap *)
let minimap render =
	if render.minimap then
		render.minimap <- false
	else 
		render.minimap <- true
	
(* Change le mode de vue *)	
let mode render =
	if render.mode = Options.TwoD then
		render.mode <- Options.ThreeD
	else 
		render.mode <- Options.TwoD
		
(* Active ou désactive les info *)
let info render =
	if render.info then
		render.info <- false
	else 
		render.info <- true