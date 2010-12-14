open ExtLib
open Commons
open Leoc

exception Badnum;;

let primes = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71; 
  73; 79; 83; 89; 97; 101; 103; 107; 109; 113; 127; 131; 137; 139; 149; 151; 157; 163; 
	167; 173; 179; 181; 191; 193; 197; 199; 211; 223; 227; 229; 233; 239; 241; 251];;

let gen_shape n = 
	let rec gen level count = 
		if count = 0 then [] else
		let pplus = count - (count-level)*3/4 in
		let t = Random.int count in
		(*Printf.printf "gen count=%d lev=%d P+=%d t=%d\n" count level pplus t;*)
		let delta = 
			if t < level then -1 else
			if t > pplus then 1 else 0 in
		level::(gen (level+delta) (count-1)) in
	gen 0 n
	
let test_shape n =
	let lst = gen_shape n in
	List.map string_of_int lst |> String.join " " |> print_endline

let as_sum x = 
	let a = Random.int ((abs x) + 1) in	a, x-a

let as_mul x =
	let factors = List.filter (fun p -> x mod p = 0) primes |> Array.of_list in
	let len = Array.length factors in
	if len > 0 then	let a = factors.(Random.int len) in	a, x/a else raise Badnum  

let as_xor x = 
	let a = Random.int 250 + 1 in	a, x lxor a

let as_mod x =
	if x <= 0 then raise Badnum else
	let rec loop n = if n > x then n else loop (n*2) in
	let a = loop 16 in x, a
	
let compute x =
	try match Random.int 4 with
		| 0 -> Add, as_sum x
		| 1 -> Mul, as_mul x
		| 2 -> Xor, as_xor x
		| 3 -> Mod, as_mod x
		| _ -> failwith "never happens"
	with Badnum -> Add, as_sum x	 
	
type noise_operation = Def of name | Set of name * int

let const_var x = Printf.sprintf "const_%d_%d" x (uid ())

let var_or_val x =
	if Random.int 4 = 1 then 
		let varname = const_var x in LV(Var varname), [Set(varname, x)]
	else Val x, []
	
let gen_cond tru =
	let a = Random.int 192 in let b = Random.int 63 + 1 + a in
	if tru then
		if Random.int 3 = 1 then Not(Less(Val b, Val a)) else Less(Val a, Val b)
	else
		if Random.int 3 = 1 then Not(Less(Val a, Val b)) else Less(Val b, Val a)
	
let add_cond con = 
	let pswap a b = if Random.int 2 = 1 then a,b else b,a in
	if Random.int 4 = 1 then 
		if Random.int 2 = 1 then
			let a, b = pswap con (gen_cond true) in	And(a, b) 
		else 
			let a, b = pswap con (gen_cond false) in Or(a, b)
	else con	

let rec complicate_stmt = function
	| DefVar _ | 	Break | Trash _ as x -> [], x	
	| Assign(sz, lv, rv) -> let lst1, lv' = complicate_lv lv and lst2, rv' = complicate_rv rv in lst1 @ lst2, Assign(sz, lv', rv') 
	| Call(name, rvs) -> let lsts, rvs' = List.map complicate_rv rvs |> List.split in	List.concat lsts, Call(name, rvs')   
	| Defun(name, params, code) -> [], Defun(name, params, add_noise code)
	| Ret rvs -> let lsts, rvs' = List.map complicate_rv rvs |> List.split in List.concat lsts, Ret rvs' 
	| If(con, code1, code2) -> 
			let lst1, con' = complicate_cond con  
			and lst2, code1' = complicate_code true code1
			and lst3, code2' = complicate_code true code2 in
			lst1 @ lst2 @ lst3, If(con', code1', code2')	
	| While(con, code) -> 
			let lst1, con' = complicate_cond con 
			and lst2, code' = complicate_code true code in
			lst1 @ lst2, While(con', code') 
	| Print _ | Prchar _ as x -> [], x  
	| Alloc(lv, rv) -> let lst1, lv' = complicate_lv lv and lst2, rv' = complicate_rv rv in lst1 @ lst2, Alloc(lv', rv')
	| Comp code -> let lst, code' = complicate_code true code in lst, Comp code'
	| PostMessage(msg, rv1, rv2) -> let lst1, rv1' = complicate_rv rv1 and lst2, rv2' = complicate_rv rv2 in 
																	lst1 @ lst2, PostMessage(msg, rv1', rv2')

and complicate_lv = function
	| Var _	| PVar _	| LReg _ as x -> [], x 
	| PArith(op, lv1, rv2) ->
			let lst1, lv' = complicate_lv lv1 in
			let lst2, rv' = complicate_rv rv2 in
			lst1 @ lst2, PArith(op, lv', rv')
			
and complicate_rv = function
	| LV lv -> let lst, lv' = complicate_lv lv in lst, LV lv'
	| Val i -> let varname = const_var i in [Set(varname, i)], LV(Var varname)
	| Arith(op, rv1, rv2) -> 
			let lst1, rv1' = complicate_rv rv1 and lst2, rv2' = complicate_rv rv2 in
			lst1 @ lst2, Arith(op, rv1', rv2') 
	| FCall(name, rvs) -> 
			let lsts, rvs' = List.map complicate_rv rvs |> List.split in
			List.concat lsts, FCall(name, rvs')
	| Byte rv -> let lst, rv' = complicate_rv rv in lst, Byte rv'

and complicate_cond con = con |> add_cond |> deconst_cond 
	
and deconst_cond = function 
	| Less(rv1, rv2) -> let lst1, rv1' = complicate_rv rv1 and lst2, rv2' = complicate_rv rv2 in lst1 @ lst2, Less(rv1', rv2')
	| Eq(rv1, rv2) -> let lst1, rv1' = complicate_rv rv1 and lst2, rv2' = complicate_rv rv2 in lst1 @ lst2, Eq(rv1', rv2')
	| And(con1, con2) -> let lst1, con1' = complicate_cond con1 and lst2, con2' = complicate_cond con2 in 
			lst1 @ lst2, And(con1', con2')
	| Or(con1, con2) -> let lst1, con1' = complicate_cond con1 and lst2, con2' = complicate_cond con2 in 
			lst1 @ lst2, Or(con1', con2')
	| Not con -> let lst, con' = complicate_cond con in lst, Not con'

and deconst_code on code =
	match on, code with
	| _, [] -> [], []
	| _, (Trash t)::tail -> let lst, code' =  deconst_code t tail in lst, (Trash t)::code'
	| false, st::tail -> let lst, code' = deconst_code false tail in lst, st::code'
	| true, st::tail ->
			let lst1, st' = complicate_stmt st 
			and lst2, tail_code = deconst_code true tail in
			let lst = lst1 @ lst2 and code' = st' :: tail_code in
			let lst', noise_code =   
				if lst <> [] && Random.int 100 < 40 then 
					let len = List.length lst in
					let idx = len/2 + (Random.int (len - len/2)) in
					let l1, l2 = List.split_nth idx lst in
					let op = List.hd l2 in
					let nlst, noise_st = gen_noise op in
					nlst @ l1 @ (List.tl l2), [noise_st]  
				else lst, [] in
			lst', noise_code @ code'

and complicate_code on code = deconst_code on code |> reshape_code 

and add_noise code = 
	let lst, code0 = deconst_code true code in
	let rec mknoise = function
		| [] -> []
		| oplist ->	let lst', ncode = List.map gen_noise oplist |> List.split in
								(List.concat lst' |> mknoise) @ ncode in
	let code1 = (mknoise lst) @ code0 in
	let lst2, code2 = reshape_code ([], code1) in
	(mknoise lst2) @ code2
		
and gen_noise = function
	| Def name -> [], DefVar name
	| Set(name, value) -> 
			let op, (a,b) = compute value in
			let rv1, op1 = var_or_val a and rv2, op2 = var_or_val b in						  
			[Def name] @ op1 @ op2, Assign(ASInt, Var name, Arith(op, rv1, rv2))	

and reshape_code (oplist, code) = 
	let rec reshape cur_level shape_code =
		match shape_code with
		| []  -> [], []
		| (level, st)::shape_code_tail ->
			  if level = cur_level then 
				  let lst, code' = reshape cur_level shape_code_tail in
					lst, st::code'
				else 
					if level > cur_level then
						let lifted_shape_code = List.takewhile (fun (x,_) -> x > cur_level) shape_code in
						let nlifted = List.length lifted_shape_code in
						let rest_shape_code = List.drop nlifted shape_code in
						let defs_sc, other_lifted_sc = List.partition (function (_,DefVar _) -> true | _ -> false) lifted_shape_code in
						let defs_code = List.map snd defs_sc in
						let chunk_oplist, shaped_code = reshape (cur_level+1) other_lifted_sc in
						let con_lst, ifst =
							if Random.int 2 = 0 then
								let ls, cond = gen_cond true |> complicate_cond in
								ls, If(cond, shaped_code, []) 
							else
								let ls, cond = gen_cond false |> complicate_cond in
								ls, If(cond, [], shaped_code) in
						let lst, code' = reshape cur_level rest_shape_code in
						con_lst @ chunk_oplist @ lst, defs_code @ (ifst :: code')
					else failwith "reshape: level < cur_level"   in
  let shape_code = List.combine (List.length code |> gen_shape) code |> flatten_notrash in
	(*List.iter (fun (lev, st) -> Printf.printf "~%d: %s\n" lev (show_stmt 0 st)) shape_code;*)  					
	let lst, code' = reshape 0 shape_code in
	lst @ oplist, code'					 
						 
and flatten_notrash shape_code =
	let rec flatten prevlev notr shcd = 
		match notr, shcd with
		| _, [] -> []
		| _, ((lev, Trash false) as x)::tail -> x::(flatten lev true tail)
		| _, (lev, Trash true)::tail -> (prevlev, Trash true)::(flatten lev false tail)
		| true, (lev, st)::tail -> (prevlev, st)::(flatten prevlev true tail)
		| false, x::tail -> x::(flatten prevlev false tail)  in   			
	flatten 0 false shape_code		
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			

