open ExtLib
open Commons
open Leoc

(**************** partial evaluation ***************)

type var_value =  Undefined | Copy of rvalue | Complex

module S = Set.Make(String)

let show_val = function
	| Undefined -> "undef" | Copy rv -> "copy of "^(Leoc.show_rvalue rv) | Complex -> "something"
	
let show_vars = M.iter (fun name value -> Printf.printf "[%s = %s] " name (show_val value)) 
		
let newctx = M.empty, S.empty
let defvar (vars, chg) var = M.add var Undefined vars, chg
let setvar (vars, chg) var value = 
	Printf.printf "#setvar %s = %s\n" var (show_val value);
	M.add var value vars, S.add var chg
let getvar (vars, _) var = try M.find var vars with Not_found -> (show_vars vars; failwith ("Optim: var not found: "^var)) 

let mark vars ch = M.mapi (fun vname vvalue -> if S.mem vname ch then Complex else vvalue) vars

let rec eval_stmt ctx = function
	| DefVar name as x -> defvar ctx name, x
	| Assign(lv, rv)  -> let ctx', rv' = eval_assgn ctx lv rv in ctx', Assign(lv, rv')
	| Assignb(lv, rv) -> let ctx', rv' = eval_assgn ctx lv rv in ctx', Assignb(lv, rv')			
	| Call(name, rvs) -> ctx, Call(name, List.map (eval_rvalue ctx) rvs)   
	| Defun(name, params, code) ->
			let ctx' = List.fold_left (fun cx par -> setvar cx par Complex) newctx params in  
			let _, code', _ = eval_code ctx' code in ctx, Defun(name, params, code')
	| Ret rvs -> ctx, Ret(List.map (eval_rvalue ctx) rvs)
	| If(con, code1, code2) -> 
			let con' = eval_cond ctx con in
			let ctx1, code1', ch1 = eval_code ctx code1 and ctx2, code2', ch2 = eval_code ctx code2 in
			let vars, chgd = ctx in 
			let ch = S.union ch1 ch2 in			
			let vars' = mark vars ch in
			let chgd' = M.fold (fun vname vvalue s -> if S.mem vname ch then S.add vname s else s) vars chgd in
			(vars', chgd'), If(con', code1', code2')			
	| While(con, code) -> 
			let ctx1, code1, ch1 = eval_code ctx code in
			let vars, chgd = ctx in 
			let vars' = mark vars ch1 in
			let (vars2, chgd2), code2, ch2 = eval_code (vars', chgd) code in
			let con' = eval_cond (vars', chgd) con in
			let vars'' = mark vars ch2 in
			(vars'', chgd2), While(con', code2) 
	| Print rv -> ctx, Print(eval_rvalue ctx rv) 
	| Alloc(lv, rv) -> 
			let ctx' = match lv with
				| Var nm ->	setvar ctx nm Complex
				| _ -> ctx in
			ctx', Alloc(lv, eval_rvalue ctx rv)
	| Comp code -> let ctx', code', _ = eval_code ctx code in ctx', Comp code'
	| Break as x -> ctx, x 

and eval_assgn ctx lv rv = 	
	let rv' = eval_rvalue ctx rv in
	let ctx' = match lv with
		| Var name ->
				(match rv' with
				| Val _ | LV(Var _) -> setvar ctx name (Copy rv')
				| _ -> setvar ctx name Complex)
		| _ -> ctx in 
	ctx', rv'
			
and eval_rvalue ctx = function
	| Val i as x -> x
	| LV (Var name) as x -> 
			(match getvar ctx name with
			| Undefined -> failwith ("evaluating undefined var " ^ name) 
			| Copy rv -> rv
			| Complex -> x)
	| LV(PArith(op, lv, rv)) ->	LV(PArith(op, lv, eval_rvalue ctx rv))  
	| LV lv as x -> x  
	| Arith(op, rv1, rv2) -> 
			let erv1 = eval_rvalue ctx rv1 and erv2 = eval_rvalue ctx rv2 in simp_rvalue (Arith(op, erv1, erv2))
	| FCall(name, rvs) -> FCall(name, List.map (eval_rvalue ctx) rvs)
	| Byte rv -> Byte (eval_rvalue ctx rv) 
									
and eval_cond ctx = function
	| Less(rv1, rv2) -> Less(eval_rvalue ctx rv1, eval_rvalue ctx rv2) 
	| Eq(rv1, rv2) -> Eq(eval_rvalue ctx rv1, eval_rvalue ctx rv2)
	| And(con1, con2) -> And(eval_cond ctx con1, eval_cond ctx con2)
	| Or(con1, con2) -> Or(eval_cond ctx con1, eval_cond ctx con2)
	| Not con -> Not (eval_cond ctx con)
	
and eval_code (vars, chgd) code =
	let (vs,ch), code' = List.fold_left (fun (ctx, ccode) stmt -> let ctx', stmt' = eval_stmt ctx stmt in ctx', stmt' :: ccode) 
		((vars, S.empty),[]) code in
	let chgd' = M.fold (fun vname vvalue s -> if S.mem vname ch then S.add vname s else s) vars chgd in
	let vars' = M.mapi (fun vname vvalue -> if S.mem vname ch then M.find vname vs else vvalue) vars in
	(vars', chgd'), List.rev code', ch  
	
let eval code = 
	let ctx = setvar newctx "args" Complex in
	let _, code', _ = eval_code ctx code in code' 	
	
(*************** dead code elimination ***********************)

let def (defs, uses) var = S.add var defs, uses
let use (defs, uses) var = Printf.printf "use %s\n" var; defs, S.add var uses
let show set = print_string "["; S.iter (Printf.printf "%s ") set; print_string "]\n"

let rec collect_stmt ctx = function
	| DefVar name -> def ctx name
	| Assign(lv, rv) -> collect_asgn ctx lv rv 
	| Assignb(lv, rv) -> collect_asgn ctx lv rv 
	| Call(name, rvs) -> List.fold_left collect_rvalue ctx rvs  
	| Defun(name, params, code) -> ctx
	| Ret rvs -> List.fold_left collect_rvalue ctx rvs
	| If(con, code1, code2) -> collect_code (collect_code (collect_cond ctx con) code1) code2
	| While(con, code) -> collect_code (collect_cond ctx con) code
	| Print rv -> collect_rvalue ctx rv
	| Alloc(lv, rv) -> collect_asgn ctx lv rv
	| Comp code -> collect_code ctx code 
	| Break -> ctx 
									
and collect_asgn ctx lv rv =									
	let ctx1 = collect_rvalue ctx rv in
	match lv with
	| Var _ | LReg _ -> ctx1
	| PVar nm -> use ctx1 nm
	| PArith(op, lv1, rv2) -> collect_lvalue (collect_rvalue ctx1 rv2) lv1
									
and collect_rvalue ctx = function
	| LV lv -> collect_lvalue ctx lv
	| Val _ -> ctx
	| Arith(op, rv1, rv2) -> collect_rvalue (collect_rvalue ctx rv1) rv2
	| FCall(name, rvs) -> List.fold_left collect_rvalue ctx rvs
	| Byte rv -> collect_rvalue ctx rv

and collect_lvalue ctx = function
	| Var name -> use ctx name 
	| PVar name	-> use ctx name
	| LReg _ -> ctx
	| PArith(op, lv1, rv2) -> collect_lvalue (collect_rvalue ctx rv2) lv1

and collect_cond ctx = function
	| Less(rv1, rv2) | Eq(rv1, rv2) -> collect_rvalue (collect_rvalue ctx rv1) rv2
	| And(con1, con2) | Or(con1, con2) -> collect_cond (collect_cond ctx con1) con2
	| Not con -> collect_cond ctx con 

and collect_code (defs, uses) code =
	List.fold_left collect_stmt (S.empty, uses) code 	
	
	
let rec clean_stmt uses st = match st with
	| DefVar name | Assign(Var name, _) | Assignb(Var name, _) | Alloc(Var name, _) -> if S.mem name uses then Some st else None 
	| Assign _	| Assignb _	| Call _	| Defun _ | Ret _	| Print _	| Break | Alloc _ -> Some st	
	| If(con, code1, code2) -> Some(If(con, clean_code uses code1, clean_code uses code2))
	| While(con, code) -> Some(While(con, clean_code uses code))
	| Comp code -> Some(Comp(clean_code uses code)) 

and clean_code uses code =
	let k = uid () in
	Printf.printf "#clean_code %d, uses = " k; show uses;
	let _, uses' = collect_code (S.empty, uses) code in
	Printf.printf "uses' %d = " k; show uses';
	List.enum code |> Enum.filter_map (clean_stmt uses') |> List.of_enum 
	
let optimize code = code |> eval |> clean_code S.empty	
	