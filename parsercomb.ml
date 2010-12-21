open ExtString
open Commons;;

type ('res, 'tok, 'src) parse_result = Parsed of ('res * 'src) * 'tok list | Failed;;

let return x s = Parsed((x, no_source), s)
let return_ x sl s = Parsed((x, sl), s)
let fail s = Failed

let set_source sl = function Failed -> Failed | Parsed((x,_), s) -> Parsed((x,sl), s)	

let p_pred p s =
	match s with    
	| [] -> Failed
	| h::t -> if p h then Parsed(h, t) else Failed

(*let p_char c = p_pred ((=) c)*)

let p_somechar s = 
	match s with
	| [] -> Failed
	| h::t -> Parsed(h, t) 

let p_manyf prs f v0 s =
	let rec loop v st sl =
		match prs st with
		| Parsed((x, sl'), s') -> loop (f v x) s' sl 
		| Failed -> Parsed((v, sl), st) in
	loop v0 s no_source

let p_many prs s =
	let rec loop st sl =
		match prs st with
		| Parsed((_, sl'), s') -> loop s' sl'
		| Failed -> Parsed(((), sl), st) in
	loop s no_source;;

let p_opt defval p s =
	match p s with
	| Parsed _ as ok -> ok
	| Failed -> Parsed(defval, s);;

let (|||) p1 p2 s =
	match p1 s with
	| Parsed _ as ok -> ok
	| Failed -> p2 s;; 

let (>>=) p1 f s =
	match p1 s with
	| Parsed((x, sl), s2) -> f x s2 |> set_source sl 
	| Failed -> Failed;;

let (>>>) p1 p2 s =
	match p1 s with
	| Parsed((_,sl), s2) -> p2 s2 |> set_source sl
	| Failed -> Failed;;

let add_sl = function
	| Parsed((x,sl),s) -> Parsed(((x,sl),sl),s)
	| Failed -> Failed
(*let (>>=@) p1 f s = (*f gets value, source_loc and rest of string*)
	match p1 s with
	| Parsed((x, sl), s2) -> f x sl s2  
	| Failed -> Failed;;

let (>>>@) p1 p2 s =
	match p1 s with
	| Parsed((_,sl), s2) -> (match p2 s2 with Parsed((x,_),s') -> Parsed(((x,sl),sl), s') | Failed -> Failed)
	| Failed -> Failed;;
*)

let p_many1f prs f =
	prs >>= fun v0 ->	p_manyf prs f v0;;

let p_plus prs = prs >>> p_many prs;;

let isdigit c = c>='0' && c<='9';;
(*let p_digit = p_pred isdigit;;*)

let mkInt v x = v * 10 + int_of_char x - 48;;

let rec p_seq prs = (* a+, sequence of something *)
	prs >>= fun x ->
	p_opt ([x], no_source) (p_seq prs >>= fun lst -> return (x::lst));;

let p_seq0 prs = p_opt ([], no_source) (p_seq prs);; (* a*, *)

let rec p_list prs psep = (* a+, list of something, separated by given separator parser *)
	prs >>= fun x ->
	p_opt ([x], no_source) (psep >>> p_list prs psep >>= fun lst -> return (x::lst));;

let p_list0 prs psep = p_opt ([], no_source) (p_list prs psep);; (* a* *)

(*let rec p_listch prs sep = (* a+, list of something, separated by given char *)
	prs >>>= fun (x,sl) ->
	p_opt ([x], sl) (p_char sep >>> p_listch prs sep >>= fun lst -> return ((x::lst),sl));;*)

(*let p_intlist = p_listch p_int;;*) 

let p_void prs s = 
	match prs s with
	| Parsed((_,sl), s') -> Parsed(((),sl), s')
	| Failed -> Failed;; 
