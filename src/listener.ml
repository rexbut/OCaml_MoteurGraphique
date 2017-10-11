exception PlayerMoveEvent of Player.mv
exception PlayerRotateDirEvent of Player.dir
exception PlayerRotateEvent
exception PlayerSneakEvent
exception PlayerJumpEvent
exception PlayerRunEvent
exception PlayerInfoEvent

exception RenderMapEvent
exception RenderModeEvent

let mouse_x = ref 1

let init () = 
	mouse_x := match Graphics.mouse_pos () with 
	| (x, y) -> x

(* Ecouter les touches et la souris *)
let listener () =
	init ();
	while true do
    let event = Graphics.wait_next_event [Graphics.Key_pressed; Graphics.Mouse_motion] in
    if event.Graphics.keypressed then
      begin
        let key = event.Graphics.key in
					match key with
					| 'z' -> raise (PlayerMoveEvent Player.MFwd) 
					| 's' -> raise (PlayerMoveEvent Player.MBwd)
					| 'q' -> raise (PlayerMoveEvent Player.MLeft)
					| 'd' -> raise (PlayerMoveEvent Player.MRight)
					| 'a' -> raise (PlayerRotateDirEvent Player.Left)
					| 'e' -> raise (PlayerRotateDirEvent Player.Right)
					| 'w' -> raise PlayerSneakEvent
					| ' ' -> raise PlayerJumpEvent
					| 'x' -> raise PlayerRunEvent
					| 'c' -> raise PlayerInfoEvent
					| '\027' -> raise Exit
					| 'r' -> raise RenderMapEvent
					| 'f' -> raise RenderModeEvent
					| _ -> ()
      end
		else 
			begin
				(* Si la souris a changé de position *)
				if !mouse_x <> event.Graphics.mouse_x then
					raise PlayerRotateEvent
			end;
	done