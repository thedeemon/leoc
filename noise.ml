open ExtLib
open Commons
open Leoc

type noise_operation = Def of name | Set of name * int

let rec complicate_stmt = function
	| DefVar _ | 	Break | Trash _ as x -> [], x	
	| Assign(lv, rv) -> let lst1, lv' = complicate_lv lv and lst2, rv' = complicate_rv rv in lst1 @ lst2, Assign(lv', rv') 
	| Assignb(lv, rv) -> let lst1, lv' = complicate_lv lv and lst2, rv' = complicate_rv rv in lst1 @ lst2, Assignb(lv', rv')
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
	| Print _ as x -> [], x  
	| Alloc(lv, rv) -> let lst1, lv' = complicate_lv lv and lst2, rv' = complicate_rv rv in lst1 @ lst2, Alloc(lv', rv')
	| Comp code -> let lst, code' = complicate_code true code in lst, Comp code'

and complicate_lv = function
	| Var _	| PVar _	| LReg _ as x -> [], x 
	| PArith(op, lv1, rv2) ->
			let lst1, lv' = complicate_lv lv1 in
			let lst2, rv' = complicate_rv rv2 in
			lst1 @ lst2, PArith(op, lv', rv')
			
and complicate_rv = function
	| LV lv -> let lst, lv' = complicate_lv lv in lst, LV lv'
	| Val i -> let varname = Printf.sprintf "const_%d_%d" i (uid ()) in [Set(varname, i)], LV(Var varname)
	| Arith(op, rv1, rv2) -> 
			let lst1, rv1' = complicate_rv rv1 and lst2, rv2' = complicate_rv rv2 in
			lst1 @ lst2, Arith(op, rv1', rv2') 
	| FCall(name, rvs) -> 
			let lsts, rvs' = List.map complicate_rv rvs |> List.split in
			List.concat lsts, FCall(name, rvs')
	| Byte rv -> let lst, rv' = complicate_rv rv in lst, Byte rv'

and complicate_cond = function (* todo: add more noise *)
	| Less(rv1, rv2) -> let lst1, rv1' = complicate_rv rv1 and lst2, rv2' = complicate_rv rv2 in lst1 @ lst2, Less(rv1', rv2')
	| Eq(rv1, rv2) -> let lst1, rv1' = complicate_rv rv1 and lst2, rv2' = complicate_rv rv2 in lst1 @ lst2, Eq(rv1', rv2')
	| And(con1, con2) -> let lst1, con1' = complicate_cond con1 and lst2, con2' = complicate_cond con2 in 
			lst1 @ lst2, And(con1', con2')
	| Or(con1, con2) -> let lst1, con1' = complicate_cond con1 and lst2, con2' = complicate_cond con2 in 
			lst1 @ lst2, Or(con1', con2')
	| Not con -> let lst, con' = complicate_cond con in lst, Not con'

and complicate_code on code =
	match on, code with
	| _, [] -> [], []
	| _, (Trash t)::tail -> complicate_code t tail
	| false, st::tail -> let lst, code' = complicate_code false tail in lst, st::code'
	| true, st::tail ->
			let lst1, st' = complicate_stmt st 
			and lst2, tail_code = complicate_code true tail in
			let lst = lst1 @ lst2 and code' = st' :: tail_code in
			let lst', noise_code =   
				if lst <> [] && Random.int 4 = 1 then 
					let len = List.length lst in
					let idx = len/2 + (Random.int (len - len/2)) in
					let l1, l2 = List.split_nth idx lst in
					let op = List.hd l2 in
					let nlst, noise_st = gen_noise op in
					nlst @ l1 @ (List.tl l2), [noise_st]  
				else lst, [] in
			lst', noise_code @ code'

and add_noise code = 
	let lst, code' = complicate_code true code in
	let rec mknoise = function
		| [] -> []
		| oplist ->	let lst', ncode = List.map gen_noise oplist |> List.split in
								(List.concat lst' |> mknoise) @ ncode in
	(mknoise lst) @ code' 
		
and gen_noise = function
	| Def name -> [], DefVar name
	| Set(name, value) -> [Def name], Assign(Var name, Val value)	