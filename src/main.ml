open Options
open Graphics
open Player

let () = 
	match Parse_lab.read_lab Options.cin with
	| ((px, py, pangle), l) ->
			let render = Render.new_render () in
			let player = Player.new_player (Point.new_point px py) pangle in
			let ls = Segment.new_segment_list l in
			let bsp = Bsp.build_bsp ls in
			
			open_graph (Printf.sprintf " %dx%d" win_w win_h);
			try
				while true do
					Render.display render bsp player;
					synchronize();
					(* Bug redimensionnement *)
					auto_synchronize false;
					try
						Listener.listener ();
					with
					| Listener.PlayerRotateEvent -> Player.rotate player
					| Listener.PlayerRotateDirEvent dir -> Player.rotate_dir dir player
					| Listener.PlayerMoveEvent mv -> Player.move mv player bsp
					| Listener.PlayerSneakEvent -> Player.sneak player;
					| Listener.PlayerJumpEvent -> 
  						Player.jump player; 
  						Render.display render bsp player;
    					synchronize(); 
  						for i = 0 to 10000000 do () done;
    					Player.jump player;
					| Listener.PlayerRunEvent -> Player.run player
					| Listener.RenderMapEvent -> Render.minimap render
					| Listener.PlayerInfoEvent -> Render.info render
					| Listener.RenderModeEvent -> Render.mode render
				done;
			with Exit -> close_graph (); exit 0
