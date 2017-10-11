type t = 
	{
		id : int; 
    mutable porig : Point.t; 
    mutable pdest : Point.t;
		mutable ci : float;
		mutable ce : float
	}

type tpos = L | R | C

val new_segment : int -> int -> int -> int -> t

val clone_segment : t -> t

val new_segment_list : (int * int * int * int) list -> t list

val get_position : Point.t -> t -> tpos

val split_segment : t -> t -> t option * t option

val split : t -> t list -> t list * t list

val get_xo : t -> int
val get_yo : t -> int
val get_xd : t -> int
val get_yd : t -> int

val seg_ci : t -> int -> int -> t
val seg_ce : t -> int -> int -> t

