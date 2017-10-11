type t = {
  mutable pos : Point.t;
	pa_init : int;
  mutable pa : int;
	mutable view : int;
	mutable sneak : bool;
	mutable jump : bool;
	mutable run : bool;
}

val new_player : Point.t -> int -> t

type dir = Left | Right

val rotate : t -> unit

val rotate_dir : dir -> t -> unit

type mv = MFwd | MBwd | MLeft | MRight

val move : mv -> t -> Bsp.t -> unit


val sneak : t -> unit
val jump : t -> unit
val run : t -> unit

val get_view : t -> int

