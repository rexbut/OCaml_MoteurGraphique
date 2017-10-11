type tmode = TwoD | ThreeD

let usage = "usage: ./bsp file.lab"
let file = ref ""

let mode = ref ThreeD

let win_w = ref 800
let win_h = ref 800

let fov = ref 60

let step_dist = ref 10

let xmin = ref 1
let xmax = ref 9000

let scale = ref 5
let minimap = ref false

let debug = ref false
let debug_bsp = ref false

let yaw_sensitivity = ref 0.5

let length = ref 100

let info = ref false

let set_mode = function
  | "2D" -> mode := TwoD
  | "3D" -> mode := ThreeD
  | _ -> raise (Arg.Bad "2D or 3D only")

let specs = 
  [ "-mode", Arg.String set_mode, "<2D | 3D> Permet de choisir entre la 2D et la 3D";
    "-fov", Arg.Set_int fov, " Permet de choisir angle de vision";
    "-dims", Arg.Tuple [Arg.Set_int win_w; Arg.Set_int win_h], 
    " Permet de choisir la taille de la fenêtre lors de l’ouverture du programme";
    "-scale", Arg.Set_int scale, " Permet de choisir l’échelle de la Mini-Map";
    "-map", Arg.Set minimap, " Permet d’activer la Mini-Map";
    "-step", Arg.Set_int step_dist, " Permet de choisir la distance entre deux pas";
    "-xmin", Arg.Set_int xmin, " Permet de choisir la distance minimale pour voir un objet";
		"-xmax", Arg.Set_int xmax, " Permet de choisir la distance maximale pour voir un objet";
    "-debug", Arg.Set debug, " Permet de faire afficher les informations dans la console";
		"-yaw", Arg.Set_float yaw_sensitivity, " Permet de choisir la sensibilité lors du déplacement de la souris";
		"-length", Arg.Set_int length, "La longueur des murs";
		"-info", Arg.Set info, "Afficher les informations";
  ]

let alspecs = Arg.align specs

let cin =
  let ofile = ref None in
  let set_file s =
    if Filename.check_suffix s ".lab" then ofile := Some s
    else raise (Arg.Bad "no .lab extension");
  in
  Arg.parse alspecs set_file usage;
  match !ofile with 
    | Some f -> file := f ; open_in f
    | None -> raise (Arg.Bad "no file provided")


let file = !file

let win_w = !win_w
let win_h = !win_h

let xmin = !xmin
let xmax = !xmax

let ceiling_h = win_h / 4
let floor_h = 0
let wall_h = ceiling_h - floor_h

let mode = !mode

let fov = !fov

let step_dist = float !step_dist

let scale = !scale
let minimap = !minimap

let debug = !debug
let debug_bsp = !debug_bsp

let yaw_sensitivity = !yaw_sensitivity

let length = !length

let info = !info
