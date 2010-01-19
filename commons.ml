let (|>) x f = f x;;
let (>>) f g x = g (f x);;
let flip f x y = f y x;;
external identity : 'a -> 'a = "%identity";;

let last_uid = ref 0;;
let uid () = incr last_uid; !last_uid;;

type oper = Add | Mul | Mod | Div | Sub | Xor;;
let show_op = function Add->"+" | Mul->"*" | Mod->"%" | Div->"/" | Sub->"-" | Xor->"^";;
let tab n s = (String.make n ' ') ^ s;;
let log s = () (*prerr_string (s^" "); flush stderr*);;

module M = Map.Make(String);;