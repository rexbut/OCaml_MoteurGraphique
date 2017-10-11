type t = 
	{
  	mutable minimap : bool;
  	mutable mode : Options.tmode;
		mutable info : bool;
	}

val new_render : unit -> t

val minimap : t -> unit
val info : t -> unit
val mode : t -> unit

val display : t -> Bsp.t -> Player.t -> unit