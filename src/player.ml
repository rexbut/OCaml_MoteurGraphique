open Options
open Physic
open Point

let get_view_init () = 
	try
	 (Graphics.size_y ())/9
	with
	| _ -> Options.win_h/9

type t = {
  mutable pos : Point.t;
	pa_init : int;
  mutable pa : int;
	mutable view : int;
	mutable sneak : bool;
	mutable jump : bool;
	mutable run : bool;
}

let get_rotate pa = 
	try 
  	match Graphics.mouse_pos () with 
  	| (x, y) -> ((int_of_float (float_of_int (-x + (Graphics.size_x ()/2)) *. Options.yaw_sensitivity) + pa) mod 360)
	with 
	| _ -> pa

let new_player pos pa = 
	{ 
		pos = pos; 
		pa_init = pa;
		view = (get_view_init ());
		pa = get_rotate pa;
		sneak = false;
		jump = false;
		run = false;
	};

type dir = Left | Right

let rotate p = 
	p.pa <- get_rotate p.pa_init

let rotate_dir dir p = 
	if dir = Left then
		p.pa <- ((p.pa + int_of_float (Options.yaw_sensitivity*.10.)) mod 360)
	else if dir = Right then
		p.pa <- ((p.pa - int_of_float (Options.yaw_sensitivity*.10.)) mod 360)

type mv = MFwd | MBwd | MLeft | MRight

let move_pos p bsp pos = 
	if detect_collision_segment (Segment.new_segment p.pos.x p.pos.y pos.x pos.y) bsp then
		()
	else 
		p.pos <- pos

let step_dist p = 
	if p.run then 
		int_of_float (Options.step_dist *. 2.)
	else
		int_of_float Options.step_dist

let move mv p bsp = 
	let pcos = Trigo.dcos (-p.pa) in
	let psin = Trigo.dsin (-p.pa) in
	if mv = MFwd then
		let x = 0 in
		let y =  -(step_dist p) in
		let xd = int_of_float ((float_of_int x) *. psin -. (float_of_int y) *. pcos) in
		let yd = int_of_float ((float_of_int y) *. psin -. (float_of_int x) *. pcos) in
		let pos = Point.new_point (p.pos.x+xd) (p.pos.y+yd) in
		move_pos p bsp pos
	else if mv = MBwd then
		let x = 0 in
		let y = step_dist p in
		let xd = int_of_float ((float_of_int x) *. psin -. (float_of_int y) *. pcos) in
		let yd = int_of_float ((float_of_int y) *. psin -. (float_of_int x) *. pcos) in
		let pos = Point.new_point (p.pos.x+xd) (p.pos.y+yd) in
		move_pos p bsp pos
	else if mv = MLeft then
		let x = 0 in
		let y = step_dist p in
		let xd = int_of_float ((float_of_int x) *. psin -. (float_of_int y) *. pcos) in
		let yd = int_of_float ((float_of_int y) *. psin -. (float_of_int x) *. pcos) in
		let pos = Point.new_point (p.pos.x+yd) (p.pos.y-xd) in
		move_pos p bsp pos
	else if mv = MRight then
		let x = 0 in
		let y = step_dist p in
		let xd = int_of_float ((float_of_int x) *. psin -. (float_of_int y) *. pcos) in
		let yd = int_of_float ((float_of_int y) *. psin -. (float_of_int x) *. pcos) in
		let pos = Point.new_point (p.pos.x-yd) (p.pos.y+xd) in
		move_pos p bsp pos

let sneak player = 
	if player.sneak then begin
		player.sneak <- false;
	end else 
		player.sneak <- true
		
let jump p = 
	if p.jump then
		p.jump <- false
	else 
		p.jump <- true
		
let run p = 
	if p.run then
		p.run <- false
	else 
		p.run <- true
		
let get_view p =
	let view = (Graphics.size_y ())/9 in
	if p.sneak then
		int_of_float (float_of_int view /. 1.5)
	else if p.jump then
		int_of_float (float_of_int view *. 1.5)
	else 
		view