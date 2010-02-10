open ExtString;;

type ('a, 'b) parse_result = Parsed of 'a * 'b list | Failed;;

let return x s = Parsed(x, s);;
let fail s = Failed;;

let p_pred p s =
	match s with    
	| [] -> Failed
	| h::t -> if p h then Parsed(h, t) else Failed;;

let p_char c = p_pred ((=) c);;

let p_somechar s = 
	match s with
	| [] -> Failed
	| h::t -> Parsed(h, t);; 

let p_manyf prs f v0 s =
	let rec loop v st =
		match prs st with
		| Parsed(x, s') -> loop (f v x) s'
		| Failed -> Parsed(v, st) in
	loop v0 s;;

let p_many prs s =
	let rec loop st =
		match prs st with
		| Parsed(_, s') -> loop s'
		| Failed -> Parsed((), st) in
	loop s;;

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
	| Parsed(x, s2) -> f x s2
	| Failed -> Failed;;

let (>>>) p1 p2 s =
	match p1 s with
	| Parsed(_, s2) -> p2 s2
	| Failed -> Failed;;

let p_many1f prs f =
	prs >>= fun v0 ->	p_manyf prs f v0;;

let p_plus prs = prs >>> p_many prs;;

let isdigit c = c>='0' && c<='9';;
let p_digit = p_pred isdigit;;

let mkInt v x = v * 10 + int_of_char x - 48;;

let p_int s =
	match s with
	| [] -> Failed
	| '-'::t -> (match p_manyf p_digit mkInt 0 t with
							| Parsed(x, s') -> Parsed(-x, s')
							| Failed -> Failed)
  | '0'..'9' :: _ -> p_manyf p_digit mkInt 0 s
	| _ -> Failed;; 							
	
let p_str str =
	String.fold_left (fun p c -> p >>> p_char c) (return '!') str;;

let rec p_seq prs = (* a+, sequence of something *)
	prs >>= fun x ->
	p_opt [x] (p_seq prs >>= fun lst -> return (x::lst));;

let p_seq0 prs = p_opt [] (p_seq prs);; (* a*, *)

let rec p_list prs psep = (* a+, list of something, separated by given separator parser *)
	prs >>= fun x ->
	p_opt [x] (psep >>> p_list prs psep >>= fun lst -> return (x::lst));;

let p_list0 prs psep = p_opt [] (p_list prs psep);; (* a* *)

let rec p_listch prs sep = (* a+, list of something, separated by given char *)
	prs >>= fun x ->
	p_opt [x] (p_char sep >>> p_listch prs sep >>= fun lst -> return (x::lst));;

let p_intlist = p_listch p_int;; 

let p_void prs s = 
	match prs s with
	| Parsed(_, s') -> Parsed((), s')
	| Failed -> Failed;; 
