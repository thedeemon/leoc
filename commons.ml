let (|>) x f = f x
let (>>) f g x = g (f x)
let flip f x y = f y x
external identity : 'a -> 'a = "%identity"

let last_uid = ref 0
let uid () = incr last_uid; !last_uid;;

type arg_size = ASByte | ASInt32 | ASInt (* int is 32 or 64 bit *)
type oper = Add | Mul | Mod | Div | Sub | Xor
let show_op = function Add->"+" | Mul->"*" | Mod->"%" | Div->"/" | Sub->"-" | Xor->"^"
let tab n s = (String.make n ' ') ^ s
let verbose = ref false
let log s = () (*if !verbose then (prerr_string (s^" "); flush stderr)*)

type source_loc = int (* line number *)
let no_source : source_loc = -1
let int32_is_int = ref false
let prog_lines = ref [|""|]

let prog_source sl = 
	try !prog_lines.(sl-1) with Invalid_argument _ -> "no source"
	

module M = Map.Make(String);;