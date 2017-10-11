let read_lab_1 ci =
  let r = Str.regexp "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" in
  let rp = Str.regexp "P : \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" in
  let rc = Str.regexp "\\\\\\.*" in
  let p, l =
    let rec rr (p, acc) =
      try
        let l = input_line ci in
        if Str.string_match rp l 0 then
          let xp = int_of_string (Str.replace_first rp "\\1" l) in
          let yp = int_of_string (Str.replace_first rp "\\2" l) in
          let ap = int_of_string (Str.replace_first rp "\\3" l) in
          rr (Some (xp, yp, ap), acc)
        else if Str.string_match rc l 0 then
          rr (p, acc)
        else
          let xo = int_of_string (Str.replace_first r "\\1" l) in
          let yo = int_of_string (Str.replace_first r "\\2" l) in
          let xd = int_of_string (Str.replace_first r "\\3" l) in
          let yd = int_of_string (Str.replace_first r "\\4" l) in
          rr (p, (xo, yo, xd, yd) :: acc)
      with End_of_file -> close_in ci; (p, acc)
    in rr (None, []) in
  let p = match p with 
    | None -> Format.eprintf "No player provided@."; raise Exit
    | Some c -> c in
  p, l
	
let rec explode = function
    "" -> []
  | s  -> (String.get s 0) ::
          explode (String.sub s 1 ((String.length s) - 1))
	
let read_lab_2 ci =
	let length = Options.length in
	let length2 = length/2 in
	let rp = Str.regexp "P : \\([0-9]+\\)" in
  let p, l =
    let rec rr x ((px, py, pr), acc) =
      try
				let rec rrr y chars ((px, py, pr), acc) =
  				match chars with
  				| [] -> ((px, py, pr), acc) 
  				| c::reste ->
  					if c = '\032' then begin
  						rrr (y+length) reste ((px, py, pr), acc)
						end else if c = 'o' then begin
  						rrr (y+length) reste ((x+length2, y+length2, pr), acc)
  					end else if c = '\226' then begin
							match chars with
							| [] -> ((px, py, pr), acc) 
    					| c1::[] -> ((px, py, pr), acc) 
    					| c1::c2::[] -> ((px, py, pr), acc) 
      				| c1::c2::c3::reste -> 
    						if c2 = '\148' then begin
    							if c3 = '\140' then begin
      							(* ┌ *)
        						rrr (y+length) reste ((px, py, pr), (x+length2, y+length2, x+length, y+length2)::((x+length2, y+length2, x+length2, y+length) :: acc))
									end else if c3 = '\144' then begin
      							(* ┐ *)
        						rrr (y+length) reste ((px, py, pr), (x+length2, y+length2, x+length, y+length2)::((x+length2, y+length2, x+length2, y) :: acc))
									end else if c3 = '\148' then begin
      							(* └ *)
        						rrr (y+length) reste ((px, py, pr), (x+length2, y+length2, x, y+length2)::((x+length2, y+length2, x+length2, y+length) :: acc))
									end else if c3 = '\152' then begin
										(* ┘ *)
        						rrr (y+length) reste ((px, py, pr), (x+length2, y+length2, x, y+length2)::((x+length2, y+length2, x+length2, y) :: acc))
									end else if c3 = '\188' then begin
										(* ┼ *)
        						rrr (y+length) reste ((px, py, pr), (x, y+length2, x+length, y+length2)::((x+length2, y, x+length2, y+length) :: acc))
									end else if c3 = '\172' then begin
										(* ┬ *)
        						rrr (y+length) reste ((px, py, pr), (x+length2, y+length2, x+length, y+length2)::((x+length2, y+0, x+length2, y+length) :: acc))
									end else if c3 = '\180' then begin
										(* ┴ *)
        						rrr (y+length) reste ((px, py, pr), (x+length2, y+length2, x, y+length2)::((x+length2, y+0, x+length2, y+length) :: acc))
									end else if c3 = '\164' then begin
										(* ┤ *)
        						rrr (y+length) reste ((px, py, pr), (x, y+length2, x+length, y+length2)::((x+length2, y+length2, x+length2, y) :: acc))
									end else if c3 = '\156' then begin
										(* ├ *)
        						rrr (y+length) reste ((px, py, pr), (x, y+length2, x+length, y+length2)::((x+length2, y+length2, x+length2, y+length) :: acc))
        					end else if c3 = '\128' then begin
										(* ─ *)
        						rrr (y+length) reste ((px, py, pr), (x+length2, y, x+length2, y+length)::acc)
									end else if c3 = '\130' then begin
										(* │ *)
										rrr (y+length) reste ((px, py, pr), (x, y+length2, x+length, y+length2)::acc)
        					end else begin
        						rrr (y+length) reste ((px, py, pr), acc)
    							end
    						end else begin
      						rrr (y+length) reste ((px, py, pr), acc)
    						end
  					end else begin
  						rrr (y+length) reste ((px, py, pr), acc)
						end
        	in 
				let l = input_line ci in
				if Str.string_match rp l 0 then
          let pr = int_of_string (Str.replace_first rp "\\1" l) in
          rr x ((px, py, pr), acc)
				else 
					let chars = (explode l) in
					let ((px, py, pr), acc) = rrr 0 chars ((px, py, pr), acc) in
					rr (x+length) ((px, py, pr), acc)
      with End_of_file -> close_in ci; ((px, py, pr), acc)
    in rr 0 ((0, 0, 0), []) in
  p, l

let read_lab ci =
	try 
		read_lab_1 ci
	with
	| _ -> read_lab_2 ci
		