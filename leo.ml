open ExtLib
open Commons

type name = string
type array_type = AInt | AByte
type num = NVal of int | NVar of name
type code = statement list 
and statement = 
	| Def of name * name list * expr
	| Write of lvalue * expr
	| Print of expr
	| Expr of expr
	| For of (name * seq) list * code
	| Ret of expr
	| Typedef of name * (name * field_type) list
	| Typing of name * name

and expr =
	| Val of int
	| LV of lvalue
	| Arith of oper * expr * expr
	| Call of name * expr list
	| Seq of seq
	| Length of expr
	| Head of expr
	| Tail of expr
	| New of array_type * expr 
	| If of condition * expr * expr option
	| Lambda of name list * expr
	| Comp of code 

and lvalue = 
	| Var of name
	| ArrElt of name * expr
	| SubArr of name * seq

and seq = 
	| SRange of expr * expr
	| SVal of name

and condition = 
	| Less of expr * expr
	| Eq of expr * expr
	| And of condition * condition
	| Or of condition * condition
	| Not of condition

and field_type = FInt | FArray of array_type * num | FStruct of name;;

(************************** pretty printing ********************************)

let show_args lst = 
	if lst = [] then "" else Printf.sprintf "(%s)" (String.concat ", " lst);;

let show_atype = function AInt -> "int" | AByte -> "byte";;

let rec show_code n code =
	code |> List.map (show_stmt n >> tab n) |> String.concat "\n"

and show_stmt n = function
	| Def(name, arglist, exp) -> Printf.sprintf "%s%s = %s" name (show_args arglist) (show_expr n exp) 
	| Write(lv, exp) -> Printf.sprintf "%s <- %s" (show_lvalue lv) (show_expr n exp)
	| Print e -> Printf.sprintf "print(%s)" (show_expr n e)
	| Expr e -> show_expr n e
	| For(name_seq_list, code) -> 
			Printf.sprintf "for %s\n%s\n%send" (List.map (fun (nm,sq) -> Printf.sprintf "%s in %s" nm (show_seq sq)) name_seq_list |> String.concat ", ") 
																	(show_code (n+1) code) (tab n "") 
	| Ret e -> "return " ^ (show_expr n e)
	| Typedef(name, fields) ->
			let show_field (nm, ft) = Printf.sprintf "%s : %s;\n" nm (show_ftype ft) |> tab (n+1) in
			let delim = tab n "" in			 
			Printf.sprintf "type %s = {\n%s%s" name (List.map show_field fields |> String.concat delim) (tab n "}")
	| Typing(varname, typename) -> Printf.sprintf "%s : %s" varname typename

and show_expr n = function
	| Val i -> string_of_int i
	| LV lv -> show_lvalue lv
	| Arith(op, e1, e2) -> Printf.sprintf "(%s %s %s)" (show_expr n e1) (show_op op) (show_expr n e2)
	| Call(name, elist) -> Printf.sprintf "%s(%s)" name (List.map (show_expr n) elist |> String.concat ", ") 
	| Seq seq -> show_seq seq
	| Length e -> Printf.sprintf "|%s|" (show_expr n e)
	| Head e -> (show_expr n e) ^ ".head"
	| Tail e -> (show_expr n e) ^ ".tail"
	| New(arrtype, e) -> Printf.sprintf "new %s[%s]" (show_atype arrtype) (show_expr n e) 
	| If(con, e1, e2o) -> Printf.sprintf "if %s then %s %s" (show_cond n con) (show_expr n e1) 
													(Option.map_default (show_expr n >> Printf.sprintf "else %s") "" e2o)
	| Lambda(name_list, e) -> Printf.sprintf "(%s) => %s" (String.concat ", " name_list) (show_expr n e) 
	| Comp code -> if List.length code > 1 then Printf.sprintf "{\n%s\n%s}" (show_code (n+1) code) (tab n "")
									else show_code (n+1) code 

and show_seq = function
	| SRange(e1, e2) -> Printf.sprintf "%s..%s" (show_expr 0 e1) (show_expr 0 e2)
	| SVal name -> name

and show_cond n = function
	| Less(e1, e2) -> Printf.sprintf "%s < %s" (show_expr n e1) (show_expr n e2)
	| Eq(e1, e2) -> Printf.sprintf "%s = %s" (show_expr n e1) (show_expr n e2)
	| And(con1, con2) -> Printf.sprintf "(%s && %s)" (show_cond n con1) (show_cond n con2)
	| Or(con1, con2) -> Printf.sprintf "(%s || %s)" (show_cond n con1) (show_cond n con2)
	| Not con -> Printf.sprintf "not (%s)" (show_cond n con)

and show_lvalue = function
	| Var name -> name
	| ArrElt(name, e) -> Printf.sprintf "%s[%s]" name (show_expr 0 e)
	| SubArr(name, seq) -> Printf.sprintf "%s[%s]" name (show_seq seq)

and show_ftype = function
	| FInt -> "int" | FStruct nm -> nm | FArray(aty, n) -> Printf.sprintf "%s[%s]" (show_atype aty) (show_num n)
	
and show_num = function NVal i -> string_of_int i | NVar nm -> nm;;
(*********************** recursion check ***********************************)

let rec is_recursive funname = function
	| Val i -> false
	| LV lv -> lvalue_uses_fun funname lv
	| Arith(op, e1, e2) -> (is_recursive funname e1) || (is_recursive funname e2) 
	| Call(name, elist) -> funname = name || (List.exists (is_recursive funname) elist) 
	| Seq seq ->  seq_uses_fun funname seq
	| Length e | Head e | Tail e -> is_recursive funname e
	| New(arrtype, e) -> is_recursive funname e 
	| If(con, e1, e2o) -> (cond_uses_fun funname con) || (is_recursive funname e1) || (Option.map_default (is_recursive funname) false e2o)												
	| Lambda(name_list, e) -> 
			(if List.mem funname name_list then failwith ("one name for function and lambda arg: "^funname);
		 	is_recursive funname e)
	| Comp code -> List.exists (stmt_uses_fun funname) code
	
and cond_uses_fun funname = function
	| Less(e1, e2) 
	| Eq(e1, e2) -> (is_recursive funname e1) || (is_recursive funname e2)
	| And(con1, con2) 
	| Or(con1, con2) -> (cond_uses_fun funname con1) || (cond_uses_fun funname con2)
	| Not con -> cond_uses_fun funname con

and stmt_uses_fun funname = function
	| Def(name, arglist, exp) -> 
			(if List.mem funname (name::arglist) then failwith ("function name already used: "^funname);
			is_recursive funname exp) 
	| Write(_, e) | Print e	| Ret e	| Expr e -> is_recursive funname e
	| For(name_seq_list, code) -> (List.exists (snd >> seq_uses_fun funname) name_seq_list) || (List.exists (stmt_uses_fun funname) code)
	| Typedef _ | Typing _ -> false

and seq_uses_fun funname = function 
	| SVal _ -> false 
	| SRange(e1, e2) -> (is_recursive funname e1) || (is_recursive funname e2)		

and lvalue_uses_fun funname = function
	| Var name -> name = funname
	| ArrElt(_, e) -> is_recursive funname e
	| SubArr(_, seq) -> seq_uses_fun funname seq
				
(*********************** expand ************************************)		

type name_desc = NValue | NFun of name list * expr | NRecFun of int;;

let add_name ctx name desc = M.add name desc ctx;;
let get_name ctx name = M.find name ctx;;
let rename map name = 
	try (match M.find name map with LV(Var nm) -> nm | _ -> name) with Not_found -> name 	

let rec expand_code ctx code = 
	let ctx2, code2 = 
		List.fold_left (fun (cx,cd) stmt -> let ctx1, st1 = expand_stmt cx stmt in ctx1, st1::cd) (ctx, []) code in
	ctx2, (code2 |> List.enum |> Enum.filter_map identity |> List.of_enum |> List.rev) 

and expand_stmt ctx = function
	| Def(name, arglist, Lambda(params, e)) -> 
			expand_stmt ctx (Def(name, arglist @ params, e)) 			
	| Def(name, arglist, exp) ->
			if arglist = [] then add_name ctx name NValue, Some (Def(name, [], expand_expr ctx exp))
			else 
				if is_recursive name exp then
					let ctx1 = add_name ctx name (NRecFun (List.length arglist)) in
					ctx1, Some (Def(name, arglist, (*expand_expr ctx1*) exp))
				else add_name ctx name (NFun(arglist, exp)), None 			
	| Write(lv, e) -> ctx, Some(Write(expand_lvalue ctx lv, expand_expr ctx e))
	| Print e -> ctx, Some(Print (expand_expr ctx e))
	| Expr e -> ctx, Some(Expr (expand_expr ctx e))
	| For(name_seq_list, code) -> 
			let ns = List.map (fun (n,s) -> n, expand_seq ctx s) name_seq_list in
			let _, ecode = expand_code ctx code in
			ctx, Some(For(ns, ecode))
	| Ret e -> ctx, Some(Ret(expand_expr ctx e))
	| (Typedef _ as x) | (Typing _ as x) -> ctx, Some x

and expand_lvalue ctx = function
	| Var _ as x -> x
	| ArrElt(name, e) -> ArrElt(name, expand_expr ctx e)
	| SubArr(name, seq) -> SubArr(name, expand_seq ctx seq)

and expand_seq ctx = function
	| SVal _ as x -> x 
	| SRange(e1, e2) -> SRange(expand_expr ctx e1, expand_expr ctx e2)

and expand_expr ctx = function
	| Val i as x -> x
	| LV lv -> LV (expand_lvalue ctx lv) 
	| Arith(op, e1, e2) -> Arith(op, expand_expr ctx e1, expand_expr ctx e2) 
	| Call(name, elist) -> expand_call ctx name elist
	| Seq seq -> Seq(expand_seq ctx seq)
	| Length e -> Length(expand_expr ctx e) 
	| Head e -> Head(expand_expr ctx e)
	| Tail e -> Tail(expand_expr ctx e)
	| New(arrtype, e) -> New(arrtype, expand_expr ctx e) 
	| If(con, e1, e2o) -> If (expand_con ctx con, expand_expr ctx e1, Option.map (expand_expr ctx) e2o)												
	| Lambda(name_list, e) as x -> x 
	| Comp code -> Comp(expand_code ctx code |> snd)

and expand_con ctx = function
  | Less(e1, e2) -> Less(expand_expr ctx e1, expand_expr ctx e2)
	| Eq(e1, e2) -> Eq(expand_expr ctx e1, expand_expr ctx e2)
	| And(con1, con2) -> And(expand_con ctx con1, expand_con ctx con2)
	| Or(con1, con2) -> Or(expand_con ctx con1, expand_con ctx con2)
	| Not con -> Not(expand_con ctx con) 

and argval k (argname, argexp) =
	let var = Printf.sprintf "%s_%d" argname k in 
	match argexp with
	| Val _ | LV(Var _)	| LV(SubArr _) | Seq _ -> argexp, None 
	| Arith _ | Call _ | LV(ArrElt _) | Length _ | Head _ | Tail _  | New _ | If _ | Comp _ -> LV(Var var), Some(Def(var, [], argexp))
	| Lambda(name_list, e) -> LV(Var var), Some(Def(var, name_list, e)) 

and expand_call ctx name elist =
	try
		(match get_name ctx name with
		| NValue -> failwith (Printf.sprintf "'%s' is not a function" name)
		| NFun(name_list, exp) -> 
				if List.length name_list <> List.length elist then failwith (Printf.sprintf "wrong number of arguments for '%s'" name);
				let k = uid () in
				let subs, calcs = List.combine name_list elist |> List.map (argval k) |> List.split in
				let subs_map = List.fold_left2 (fun m argname argexp -> M.add argname argexp m) M.empty name_list subs in
				let e = subst_expr subs_map k exp in
				let precalcs = (List.enum calcs |> Enum.filter_map identity |> List.of_enum) @ [Expr e] in
				Comp(expand_code ctx precalcs |> snd)				
		| NRecFun np -> 
				let narg = List.length elist in 
				if np = narg  then Call(name, List.map (expand_expr ctx) elist)
				else failwith (Printf.sprintf "wrong number of arguments for '%s' (%d instead of %d)" name narg np))  
	with Not_found -> failwith (Printf.sprintf "function '%s' not found" name)	
	
and subst_expr subs_map k = function
	| Val i as x -> x 
	| LV lv -> subst_lvalue_expr subs_map k lv
	| Arith(op, e1, e2) -> Arith(op, subst_expr subs_map k e1, subst_expr subs_map k e2) 
	| Call(name, elist) -> Call(rename subs_map name, List.map (subst_expr subs_map k) elist)
	| Seq seq -> Seq(subst_seq subs_map k seq)
	| Length e -> Length(subst_expr subs_map k e)
	| Head e -> Head(subst_expr subs_map k e)
	| Tail e -> Tail(subst_expr subs_map k e)
	| New(arrtype, e) -> New(arrtype, subst_expr subs_map k e) 
	| If(con, e1, e2o) -> If (subst_con subs_map k con, subst_expr subs_map k e1, Option.map (subst_expr subs_map k) e2o)												
	| Lambda(name_list, e) -> Lambda(name_list, subst_expr subs_map k e) 
	| Comp code -> Comp(subst_code subs_map k code)	
	
and subst_seq subs_map k = function
	| SVal name -> (try (match M.find name subs_map with Seq s -> s | LV(Var v) -> SVal v | _ -> SVal name) 
									with Not_found -> SVal(rename subs_map name)) 
	| SRange(e1, e2) -> SRange(subst_expr subs_map k e1, subst_expr subs_map k e2)
	
and subst_con subs_map k = function
	| Less(e1, e2) -> Less(subst_expr subs_map k e1, subst_expr subs_map k e2)
	| Eq(e1, e2) -> Eq(subst_expr subs_map k e1, subst_expr subs_map k e2)
	| And(con1, con2) -> And(subst_con subs_map k con1, subst_con subs_map k con2)
	| Or(con1, con2) -> Or(subst_con subs_map k con1, subst_con subs_map k con2)
	| Not con -> Not(subst_con subs_map k con)
	
and subst_code subs_map k code =
	let subs, code2 = 
		List.fold_left (fun (subs,cd) stmt -> let subs1, st1 = subst_stmt subs k stmt in subs1, st1::cd) (subs_map, []) code in
	List.rev code2 
	
and subst_stmt subs k = function
	| Def(name, arglist, exp) -> 
			let name1 = Printf.sprintf "%s_%d" name k in
			let subs1 = M.add name (LV(Var name1)) subs in
			let subs2 = List.fold_left (fun m par_name -> M.remove par_name m) subs1 arglist in
			subs1, Def(name1, arglist, subst_expr subs2 k exp) 			
	| Write(lv, e) -> subs, Write(subst_lvalue subs k lv, subst_expr subs k e) 
	| Print e -> subs, Print(subst_expr subs k e)
	| Expr e -> subs, Expr(subst_expr subs k e)
	| For(name_seq_list, code) ->
			let ns = List.map (fun (nm, sq) -> nm, subst_seq subs k sq) name_seq_list in
			subs, For(ns, subst_code subs k code)
	| Ret e -> subs, Ret(subst_expr subs k e)
	| (Typedef _ as x) | (Typing _ as x) -> subs, x
			
and subst_lvalue_expr subs k = function
	| Var name as x -> (try M.find name subs with Not_found -> LV x) (*Var(rename subs name)*)
	| ArrElt(name, e) -> LV (ArrElt(rename subs name, subst_expr subs k e))
	| SubArr(name, seq) -> LV (SubArr(rename subs name, subst_seq subs k seq))

and subst_lvalue subs k lv = 
	match subst_lvalue_expr subs k lv with
	| LV lv -> lv
	| e -> failwith "lvalue turned into expr after subst";;

(*************************** compile ********************************)		

module C = Leoc
type comp_res = Code of Leoc.code | Func of name

type val_type = TVoid | TInt | TByte | TArray of val_type * num | TRange of num * num

let rec show_type = function
	| TVoid -> "void" 
	| TInt -> "int" 
	| TByte -> "byte" 
	| TArray(vtype, _) -> (show_type vtype) ^ " array" 
	| TRange _ -> "range";;

type compiled_fun = { uname : name; cbody : C.statement; ftype : val_type }
type func_info = { params : name list; body : expr; compiled : (string * compiled_fun) list }
type compilation_context = { vars : val_type M.t;  currfunc : name; curr_uname : string }
let funs = ref M.empty
let empty_context = { vars = M.empty; currfunc = ""; curr_uname = "" }

let addvar ctx name ty = 
	Printf.printf "# %s : %s\n" name (show_type ty);
	{ctx with vars = M.add name ty ctx.vars};;

let gettype ctx name = try M.find name ctx.vars with Not_found -> failwith ("unknown type of "^name)
let rv_of_num = function NVal i -> Leoc.Val i | NVar nm -> Leoc.Var nm
let num_of_rv ctx = function
	| Leoc.Val i -> ctx, [], NVal i
	| Leoc.Var v -> ctx, [], NVar v
	| rv -> let var = Printf.sprintf "bound_%d" (uid ()) in
					addvar ctx var TInt, [C.DefVar var; C.Assign(C.Local var, rv)], NVar var;;
	
let get_code ctx = function 
	| Code c -> c 
	| Func name -> 
			try (M.find name !funs).compiled |> List.map (fun (ts, cf) ->  cf.cbody) 
			with Not_found -> failwith ("unknown function "^name);;  	
	
let addfundef  name params exp = 
	if not (M.mem name !funs) then 
		funs := M.add name { params = params; body = exp; compiled = [] } !funs;;	
	
let str_of_types types = List.map show_type types |> String.concat " * ";;	
	
let save_call = function
	| [C.FCall(name, rvs)] -> [C.Call(name, rvs)], []
	| x -> [], x 	
	
let rec returnize = function
	| If(con, e1, e2o) -> If(con, returnize e1, Option.map returnize e2o)
	| Comp code as x -> (match List.rev code with
											| Expr e :: rest -> Comp(List.rev (Expr(returnize e) :: rest)) 
											| _ -> x)
	| e -> Comp [Ret e]  	
	
let seq_of_expr = function LV(Var nm) -> SVal nm | Seq s -> s | _ -> failwith "strange range" 	
	
let rec compile_code ctx code =
	let ctx1, code1 = List.fold_left (fun (cx,cd) stmt -> let cx1, st1 = compile_stmt cx stmt in cx1, st1::cd) (ctx, []) code in
	ctx1, code1 |> List.rev |> List.map (get_code ctx1) |> List.concat 
	
and compile_stmt ctx = function
	| Def(name, [], If(con, e1, e2o)) ->
			let _, _, ty, _ = compile_expr ctx e1 in
			let ctx1 = addvar ctx name ty in
			let ctx2, compres = compile_stmt ctx1 (Write(Var name, If(con, e1, e2o))) in
			ctx2, Code ([C.DefVar name] @ get_code ctx2 compres)  
	| Def(name, [], exp) -> 
			let ctx1, code1, ty, rc = compile_expr ctx exp in
			let ctx2 = addvar ctx1 name ty in
			let code2 =	(match ty, rc with
				| TInt, [rv] ->  [Leoc.DefVar name; Leoc.Assign(Leoc.Local name, rv)]
				| TByte, [rv] -> [Leoc.DefVar name; Leoc.Assignb(Leoc.Local name, rv)]
				| TArray(aty, alen), [rva; rvlen] -> [Leoc.DefVar name; Leoc.Assign(Leoc.Local name, rva)]
				| TRange(n1, n2), [rv1; rv2] -> [] (* data already exist and referenced in type *)
				| TVoid, _ -> failwith ("defining a void value "^name)
				| _, _ -> failwith "bad number of values in Def") in
			ctx2, Code(code1 @ code2)
	| Def(name, arglist, exp) -> 
			addfundef name arglist (returnize exp);
			ctx, Func name
	| Write(lv, If(con, e1, e2o)) ->
			let e2o' = Option.map (fun e -> Comp[Write(lv, e)]) e2o in 
			compile_stmt ctx (Expr(If(con, Comp[Write(lv, e1)], e2o' )))
	| Write(lv, e) as orgst-> 
			(match lv with
			| Var name -> 
					let lty = gettype ctx name in
					let ctx1, code1, ty, rc = compile_expr ctx e in					
					let code2 =	(match lty, ty, rc with
						| TInt, TInt, [rv] ->  [Leoc.Assign(Leoc.Local name, rv)]
						| TInt, TByte, [rv] ->  [Leoc.Assign(Leoc.Local name, Leoc.Byte rv)]
						| TByte, TByte, [rv] 
						| TByte, TInt, [rv] -> [Leoc.Assignb(Leoc.Local name, rv)]
						| TArray(laty, lalen), TArray(aty, alen), [rva; rvlen] ->
								let name2 = match rva with Leoc.Var v -> v | _ -> failwith "bad array in array copy" in
								let k = uid () in
								let ivar = Printf.sprintf "i_%d" k and jvar = Printf.sprintf "j_%d" k in
								let st = For([ivar, SVal name; jvar, SVal name2], [
													Write(Var ivar, LV(Var jvar))   
												 ]) in
								(match compile_stmt ctx st with _, Code code' -> code' | _, _ -> failwith "bad result of array copy compilation")
						| TArray(laty, lalen), TRange(n1, n2), [rv_beg; rv_end] ->
								let k = uid () in
								let ivar = Printf.sprintf "i_%d" k and jvar = Printf.sprintf "j_%d" k in
								let st = For([ivar, SVal name; jvar, seq_of_expr e], [
													Write(Var ivar, LV(Var jvar))   
												 ]) in
								(match compile_stmt ctx st with _, Code code' -> code' | _, _ -> failwith "bad result of array copy compilation")																	
						| TRange _, _, _ -> failwith ("writing to range var is not supported: "^ name)
						| _, _, _ -> failwith (Printf.sprintf "type mismatch in '%s': %s and %s" (show_stmt 1 orgst) (show_type lty) (show_type ty)))
						in
					ctx1, Code(code1 @ code2) 
			| ArrElt(name, ie) -> 
					let lty = gettype ctx name in
					let ctx1, code1, ity, irc = compile_expr ctx ie in					
					let is_idx_range = match ity with TRange _ -> true | _ -> false in
					if is_idx_range then 
						compile_stmt ctx (Write(SubArr(name, seq_of_expr ie), e)) 
					else						
						let ctx2, code2, ty, rc = compile_expr ctx1 e in
						let idx_rv0 = (match ity, irc with
							| TInt, [rv] -> rv
							| TByte, [rv] -> Leoc.Byte rv 
							| _, _ -> failwith ("bad index type for "^name)) in
						let idx_rv = (match lty with
							| TArray(TByte, _) -> idx_rv0
							| TArray(TInt, _) -> Leoc.Arith(Mul, idx_rv0, Leoc.Val 4)
							| _ -> failwith ("bad array type for "^name)) in
						let pnt_var = Printf.sprintf "pnt_%d" (uid()) in
						let code_prep = [C.DefVar pnt_var; C.Assign(C.Local pnt_var, C.Arith(Add, C.Var name, idx_rv))] in
						let code3 =	(match lty, ty, rc with
							| TArray(TInt, _), TInt, [rv]  -> [C.Assign(C.PLocal pnt_var, rv)]
							| TArray(TInt, _), TByte, [rv] -> [C.Assign(C.PLocal pnt_var, Leoc.Byte rv)]
							| TArray(TByte, _), TInt, [rv]   
							| TArray(TByte, _), TByte, [rv] -> [C.Assignb(C.PLocal pnt_var, rv)]  
							| _, _, _ -> failwith (Printf.sprintf "type mismatch in '%s': %s and %s" (show_stmt 1 orgst) (show_type lty) (show_type ty))) in
						ctx, Code (code1 @ code2 @ [Leoc.Comp(code_prep @ code3)])
			| SubArr(name, seq) ->
					let k = uid () in
					let ivar = Printf.sprintf "i_%d" k and jvar = Printf.sprintf "j_%d" k in			
					let st = match e with
						| LV(SubArr(arr, aseq)) -> For([ivar,seq; jvar,aseq], 
																				[Write(ArrElt(name, LV(Var ivar)), LV(ArrElt(arr, LV(Var jvar))))])
						| Seq aseq -> For([ivar,seq; jvar,aseq], [Write(ArrElt(name, LV(Var ivar)), LV(Var jvar))])
						| LV(Var m) ->
								(match gettype ctx m with
								| TVoid -> failwith ("writing void expression to "^name)
								| TInt | TByte -> For([ivar, seq], [Write(ArrElt(name, LV(Var ivar)), e)])
								| TRange _ | TArray _ ->  For([ivar, seq; jvar, SVal m], [Write(ArrElt(name, LV(Var ivar)), LV(Var jvar))]))
						| LV(ArrElt(rname, ie)) ->
								let _, _, iety, _ = compile_expr ctx ie in
								(match iety with
									| TRange _ -> Write(lv, LV(SubArr(rname, seq_of_expr ie)))
									| _ -> For([ivar, seq], [Write(ArrElt(name, LV(Var ivar)), e)]))
						| _ -> For([ivar, seq], [Write(ArrElt(name, LV(Var ivar)), e)]) in
					compile_stmt ctx st )
	| Print e -> 
			let _, code, ty, rc = compile_expr ctx e in
			(match ty, rc with
			| TInt, [rv] ->  ctx, Code (code @ [Leoc.Print rv])
			| TByte, [rv] -> ctx, Code (code @ [Leoc.Print (Leoc.Byte rv)])
			| _, _ -> failwith (Printf.sprintf "bad argument type for print(%s): %s" (show_expr 0 e) (show_type ty)))			
	| Expr e -> 
			let _, code, ty, rc = compile_expr ctx e in
			let code2, rc2 = save_call rc in 
			ctx, Code (code @ code2)
	| For(name_seq_list, code) ->
			let org_names, seqs = List.split name_seq_list in
			let k = uid () in
			let mkname s = Printf.sprintf "%s_%d" s k and mkend s = Printf.sprintf "%s_end%d" s k in 
			let names, end_names = List.map (fun s-> mkname s, mkend s) org_names |> List.split in
			let def_code = List.map2 (fun inm ienm -> [Leoc.DefVar inm; Leoc.DefVar ienm]) names end_names |> List.concat in
			let seq_type = function
				| SRange _ -> TInt, false
				| SVal name -> match gettype ctx name with
					| TRange _ -> TInt, false
					| TArray(vtype, _) -> vtype, true
					| _ -> failwith (name ^ "is not a sequence") in
			let elt_types, elt_pointers = List.map seq_type seqs |> List.split in
			let ctx1 = List.fold_left2 addvar ctx org_names elt_types in
			let init_codes, deltas = List.map (fun (name,seq) ->
				(match seq with
				| SRange(e1, e2) -> 
						let _, e1code, _, rc1 = compile_expr ctx e1 in
						let _, e2code, _, rc2 = compile_expr ctx (Arith(Add, e2, Val 1)) in
						e1code @ e2code @ 
						[Leoc.Assign(Leoc.Local (mkname name), List.hd rc1); 
						 Leoc.Assign(Leoc.Local (mkend name), List.hd rc2)], 1
				| SVal sname ->
						match gettype ctx sname with
						| TRange(n1, n2) -> [Leoc.Assign(Leoc.Local (mkname name), rv_of_num n1); 
								Leoc.Assign(Leoc.Local (mkend name), Leoc.Arith(Add, rv_of_num n2, C.Val 1))], 1
						| TArray(aty, alen) ->
								let rv_len = rv_of_num alen in
								let rv_end, delta = match aty with
									| TByte -> C.Arith(Add, C.Var sname, rv_len), 1
									| TInt -> C.Arith(Add, C.Var sname, C.Arith(Mul, rv_len, C.Val 4)), 4
									| _ -> failwith "bad array type in seq" in 
								[Leoc.Assign(Leoc.Local (mkname name), Leoc.Var sname);
						  	 Leoc.Assign(Leoc.Local (mkend name), rv_end)], delta
						| _ -> failwith "value of wrong type in seq")) name_seq_list |> List.split in
			let init_code = List.concat init_codes in  
			let conds = org_names |> List.map (fun s -> Leoc.Less(C.Var (mkname s), C.Var (mkend s))) in 
			let cond = List.fold_left (fun con cmp -> Leoc.And(con, cmp)) (List.hd conds) (List.tl conds) in
			let next_code = List.map2 (fun inm delta -> Leoc.Assign(Leoc.Local inm, C.Arith(Add, C.Var inm, C.Val delta))) names deltas in
			let _, ccode = compile_code ctx1 code in
			let subs = List.map2 (fun orgname is_pnt ->
									let s = mkname orgname in
									if is_pnt then orgname, Leoc.PVar s else orgname, Leoc.Var s) org_names elt_pointers in
			let subs_map = List.fold_left (fun m (orgname, rv) ->	M.add orgname rv m) M.empty subs in
			let ccode1 = Leoc.subst_code subs_map ccode in
			let all_code = def_code @ init_code @ [Leoc.While(cond, ccode1 @ next_code)] in
			ctx, Code [Leoc.Comp all_code] 
	| Ret e ->
			let _, code, ty, rc = compile_expr ctx e in
			(match ty, rc with
			| TInt, rvs ->  ctx, Code (code @ [Leoc.Ret rvs])
			| TByte, rvs -> ctx, Code (code @ [Leoc.Ret (List.map (fun rv-> Leoc.Byte rv) rvs)])
			| TVoid, _ -> ctx, Code  (code @ [Leoc.Ret []])
			| _, _ -> failwith (Printf.sprintf "trying to return a %s" (show_type ty)))
	| Typedef _ | Typing _ -> ctx, Code []					 
			
and (compile_expr : compilation_context -> expr -> compilation_context * Leoc.code * val_type * Leoc.rvalue list) = fun ctx -> function
	| Val i -> ctx, [], TInt, [Leoc.Val i] 
	| LV(Var name) -> 
			let ty = gettype ctx name in
			let rvc =	(match ty with
				| TInt  | TByte   -> [Leoc.Var name]    
				| TVoid -> []
				| TArray(aty, alen) -> [Leoc.Var name; rv_of_num alen]
				| TRange(st,en) -> [rv_of_num st; rv_of_num en]) in
			ctx, [], ty, rvc
	| Arith(op, e1, e2) ->
			let ctx1, code1, ty1, rc1 = compile_expr ctx e1 in
			let ctx2, code2, ty2, rc2 = compile_expr ctx1 e2 in
			let rv = Leoc.Arith(op, rv_of_rc ty1 rc1, rv_of_rc ty2 rc2) in 
			ctx2, code1 @ code2, TInt, [rv]			     
	| Call(name, elist) -> 
			let ctx1, code1, args, compfun = get_compiled_fun ctx name elist in
			ctx1, code1, compfun.ftype, [Leoc.FCall(compfun.uname, args)]			
	| LV(ArrElt(name, e)) ->
			let aty, alen = match gettype ctx name with
				| TArray(ty, len) -> ty, len
				| _ -> failwith (name ^ " is not an array") in
			let ctx1, code1, ety, erc = compile_expr ctx e in
			let is_idx_range = match ety with TRange _ -> true | _ -> false in		
			if is_idx_range then 
				compile_expr ctx (LV(SubArr(name, seq_of_expr e))) 
			else					
				let idx_rv = match ety, erc with
					| TInt, [rv] -> rv
					| TByte, [rv] -> Leoc.Byte rv
					| _, _ -> failwith ("bad type of array index: " ^ (show_expr 0 e)) in
				let elt_size = match aty with
					| TInt  -> 4	| TByte  -> 1	| _ -> failwith (name ^ "is an array of an unsuported type") in  
				let rv = 
					if elt_size > 1 then Leoc.PArith(Add, Leoc.Var name, Leoc.Arith(Mul, idx_rv, Leoc.Val elt_size))   
					else Leoc.PArith(Add, Leoc.Var name, idx_rv) in
				ctx1, code1, aty, [rv]	
	| Seq(SRange(e1, e2)) ->
			let ctx1, code1, ty1, rc1 = compile_expr ctx e1 in
			let ctx2, code2, ty2, rc2 = compile_expr ctx1 e2 in
			let to_int ty rv = if ty = TByte then C.Byte rv else rv in
			let is_num ty = ty = TInt || ty = TByte in
			let check_num ty e = 
				if not(is_num ty) then failwith ("not a number in range constructor: " ^ (show_expr 0 e)) in
			check_num ty1 e1;
			check_num ty2 e2;
			let ctx3, code3, num1 = num_of_rv ctx2 (to_int ty1 (List.hd rc1)) in
			let ctx4, code4, num2 = num_of_rv ctx3 (to_int ty2 (List.hd rc2)) in
			ctx4, code1 @ code2 @ code3 @ code4, TRange(num1, num2), [rv_of_num num1; rv_of_num num2]			
	| Seq(SVal name) ->
			let ty = gettype ctx name in
			let rc = match ty with
				| TRange(n1, n2) -> [rv_of_num n1; rv_of_num n2]
				| _ -> failwith (name ^ "is not a sequence") in
			ctx, [], ty, rc  		
	| Length exp ->
			let _, code1, ty, rc = compile_expr ctx exp in
			let rv = (match ty with
				| TArray(aty, alen) -> rv_of_num alen
				| TRange(n1, n2) -> let rv1 = rv_of_num n1 and rv2 = rv_of_num n2 in
														Leoc.Arith(Add, Leoc.Arith(Sub, rv2, rv1), Leoc.Val 1) 
				| _ -> failwith (Printf.sprintf "bad type in Length(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
			ctx, code1, TInt, [rv]
	| Head exp ->
			let _, code1, ty, rc = compile_expr ctx exp in
			let hty, rv = (match ty with
				| TArray(aty, alen) -> aty, (match List.hd rc with 
						| C.Var v -> C.PVar v
						| C.Arith(op,a,b) -> C.PArith(op, a,b) 
						| _ -> failwith "head of strange array")						
				| TRange(n1, n2) -> TInt, rv_of_num n1														 
				| _ -> failwith (Printf.sprintf "bad type in Head(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
			ctx, code1, hty, [rv]
	| Tail exp -> 
			let _, code1, ty, rc1 = compile_expr ctx exp in
			let code2, tty, rvs = (match ty with
				| TArray(aty, alen) ->
						let code, len' = (match alen with 
							| NVal i -> [],  NVal (i-1)
							| NVar v -> let lenvar = Printf.sprintf "arr_len_%d" (uid ()) in
													[C.DefVar lenvar; C.Assign(C.Local lenvar, C.Arith(Sub, C.Var v, C.Val 1))], NVar lenvar) in													
						let elt_size = match aty with TInt  -> 4 | TByte  -> 1 | _ -> failwith "Tail of an array of an unsuported type" in
						code, TArray(aty, len'), [C.Arith(Add, List.hd rc1, C.Val elt_size); rv_of_num len']
				| TRange(n1, n2) -> 
						let code, num1 = match n1 with 
							| NVal i -> [], NVal (i+1) 
							| NVar v -> let numvar = Printf.sprintf "bound_%d" (uid()) in
													[C.DefVar numvar; C.Assign(C.Local numvar, C.Arith(Add, C.Var v, C.Val 1))], NVar numvar in
						code, TRange(num1, n2), [rv_of_num num1; rv_of_num n2]																																			 
				| _ -> failwith (Printf.sprintf "bad type in Head(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
			ctx, code1 @ code2, tty, rvs
	| LV(SubArr(name, seq)) -> 
			let prep_code, rv1, rv2 = match seq with 
				| SRange(e1, e2) -> 
						let _, code1, ty1, rc1 = compile_expr ctx e1 in
						let _, code2, ty2, rc2 = compile_expr ctx e2 in
						code1 @ code2, List.hd rc1, List.hd rc2 
				| SVal sv ->
						(match gettype ctx sv with
						| TRange(n1, n2) -> [], rv_of_num n1, rv_of_num n2 
						| _ -> failwith (name^": subarray argument must be a range")) in
			let elt_size, ety = match gettype ctx name with
				| TArray(aty, alen) -> (match aty with TInt->4 | TByte->1 | _->failwith "bad array type"), aty
				| _ -> failwith (name ^ " is not an array") in
			let lenvar = Printf.sprintf "arr_len_%d" (uid()) in
			let len_code = [C.DefVar lenvar; C.Assign(C.Local lenvar, C.Arith(Add, C.Arith(Sub, rv2, rv1), C.Val 1))] in
			let rv_begin = if elt_size > 1 then C.Arith(Mul, rv1, C.Val elt_size) else rv1 in
			ctx, prep_code @ len_code, TArray(ety, NVar lenvar), [C.Arith(Add, C.Var name, rv_begin); C.Var lenvar]  				
	| New(arrtype, e) ->  
			let ctx1, code1, ety, erc = compile_expr ctx e in
			let size_rv = match ety, erc with
				| TInt, [rv] -> rv
				| TByte, [rv] -> Leoc.Byte rv
				| _, _ -> failwith "wrong size type in New" in
			let size_rv2 = match arrtype with AByte -> size_rv | AInt -> Leoc.Arith(Mul, size_rv, Leoc.Val 4) in
			let k = uid () in 
			let alen, code2, ctx2 = (match size_rv with
				| Leoc.Val i -> NVal i, [], ctx1
				| Leoc.Var v -> NVar v, [], ctx1
				| _ -> let size_var = Printf.sprintf "array_length_%d" k in
							 let ctx' = addvar ctx1 size_var TInt in  
							 NVar size_var, [Leoc.DefVar size_var; Leoc.Assign(Leoc.Local size_var, size_rv)], ctx') in
			let arr_var = Printf.sprintf "array_%d" k in
			let alloc_code = [Leoc.DefVar arr_var; Leoc.Alloc(Leoc.Local arr_var, size_rv2)] in
			let ty = match arrtype with AInt -> TArray(TInt, alen) | AByte -> TArray(TByte, alen) in
			ctx2, code1 @ code2 @ alloc_code, ty, [Leoc.Var arr_var; size_rv]
	| If(con, e1, e2o) -> 
			let con_code, ccon = compile_cond ctx con in
			let _, code1, ty1, rc1 = compile_expr ctx e1 in
			let _, code2, ty2, rc2 = Option.map_default (compile_expr ctx) (ctx, [], TVoid, []) e2o in
			let types_ok = if e2o = None then true else match ty1, ty2 with
				| TInt, TInt -> true
				| TByte, TByte -> true
				| TVoid, TVoid -> true
				| TRange _, TRange _ -> true
				| TArray(t1, _), TArray(t2, _) when t1 = t2 -> true
				| _, _ -> false in
			if not types_ok then failwith (Printf.sprintf "different types in If: %s and %s" (show_type ty1) (show_type ty2));
			let tmp_vars = List.map (fun _-> Printf.sprintf "if_res_%d" (uid())) rc2  in
			let store rc = 
				let rvs = Array.of_list rc in
				List.mapi (fun i tmp -> Leoc.Assign(C.Local tmp, rvs.(i))) tmp_vars in
			let def_tmp_vars = List.map (fun tmp -> Leoc.DefVar tmp) tmp_vars in
			let store1 = store rc1 and store2 = store rc2 in
			let res = List.map (fun tmp -> Leoc.Var tmp) tmp_vars in			   
			ctx, con_code @ def_tmp_vars @ [Leoc.If(ccon, code1 @ store1, code2 @ store2)], ty2, res			
	| Lambda(name_list, e) ->  failwith "not expanded lambda expression"
	| Comp code ->
			(match List.rev code with
			| Expr e :: rest_code ->
					let ctx1, code01 = List.fold_left (fun (cx,cd) stmt -> 
						let cx1, st1 = compile_stmt cx stmt in cx1, st1::cd) (ctx, []) (List.rev rest_code) in
					let ctx2, code2, ty, rc = compile_expr ctx1 e in
					let ccode = code01 |> List.rev |> List.map (get_code ctx2) |> List.concat in
					ctx2, ccode @ code2, ty, rc
			| Ret e :: rest_code ->		
					let ctx1, _ = compile_code ctx (List.rev rest_code) in
					let _, _, ty, _ = compile_expr ctx1 e in
					let ctx2, ccode = compile_code ctx code in
					ctx2, ccode, ty, []
			| _ -> 
					let ctx1, ccode = compile_code ctx code in
			  	ctx1, ccode, TVoid, [])
	
and get_compiled_fun ctx name elist =
	let fi = try M.find name !funs with Not_found -> failwith ("function not found: " ^ name) in
	if List.length elist <> List.length fi.params then failwith ("wrong number of arguments for " ^ name);
	let codes0, types0, rcs0 = List.fold_left (fun (cd, tps, rvs) e -> 
		let _, ecode, ety, erc = compile_expr ctx e in (ecode::cd, ety::tps, erc::rvs)) ([],[],[]) elist in
	let codes, types, rcs = List.rev codes0, List.rev types0, List.rev rcs0 in
	let typestr = str_of_types types in
	let code = List.concat codes in
	let args = List.concat rcs in
	if name = ctx.currfunc then 
		ctx, code, args, { uname = ctx.curr_uname; cbody = Leoc.Ret []; ftype = TInt }
	else
	(try ctx, code, args, List.assoc typestr fi.compiled with Not_found ->
	(let uname = Printf.sprintf "%s_%d" name (uid()) in	
	let ftps, pars0 = List.map2 (fun par ty ->
			match ty with
			| TVoid -> ty, []
			| TInt | TByte -> ty, [par]
			| TArray(ety, _) -> let lenvar = par ^ "_len" in TArray(ety, NVar lenvar), [par; lenvar]
			| TRange _ -> let n2 = par ^ "_end" in TRange(NVar par, NVar n2), [par; n2]   
		) fi.params types |> List.split in
	let pars = List.concat pars0 in
	let ctx0 = { vars = M.empty; currfunc = name; curr_uname = uname } in
	let fctx = List.fold_left2 addvar ctx0 fi.params ftps in
	let _, fcode, fty, frc = compile_expr fctx fi.body in
	let ret_code = if frc = [] then [] else [Leoc.Ret frc] in
	let st = Leoc.Defun(uname, pars, fcode @ ret_code) in
	let compfun = { uname = uname; cbody = st; ftype = fty } in
	let fi1 = { fi with compiled = (typestr, compfun)::fi.compiled } in
	funs := M.add name fi1 !funs;
	ctx, code, args, compfun))	 
	
and rv_of_rc ty rc = 
	match ty, rc with TInt, [rv] -> rv | TByte, [rv] -> Leoc.Byte rv | _ -> failwith "bad type in condition or arithmetic"	
	
and compile_cond ctx = function
	| Less(e1, e2) -> 
			let _, code1, ty1, rc1 = compile_expr ctx e1 in
			let _, code2, ty2, rc2 = compile_expr ctx e2 in
			code1 @ code2, Leoc.Less(rv_of_rc ty1 rc1, rv_of_rc ty2 rc2)
	| Eq(e1, e2) -> 
			let _, code1, ty1, rc1 = compile_expr ctx e1 in
			let _, code2, ty2, rc2 = compile_expr ctx e2 in
			code1 @ code2, Leoc.Eq(rv_of_rc ty1 rc1, rv_of_rc ty2 rc2)		
	| And(con1, con2) -> 
			let code1, ccon1 = compile_cond ctx con1 in
			let code2, ccon2 = compile_cond ctx con2 in
			code1 @ code2, Leoc.And(ccon1, ccon2)
	| Or(con1, con2) ->
			let code1, ccon1 = compile_cond ctx con1 in
			let code2, ccon2 = compile_cond ctx con2 in
			code1 @ code2, Leoc.Or(ccon1, ccon2)		 
	| Not con -> 	
			let code1, ccon1 = compile_cond ctx con in
			code1, Leoc.Not(ccon1)
;;
(***********************************************************)		


(*let prg = [
	Def("x", [], Val 5);
	Def("sum", ["a"; "b"], Arith(Add, Var "a", Var "b"));
	Def("m", [], New(AInt, Val 10));
	For(["i", SRange(Val 0, Val 9)], [Write(LArr("m", Var "i"), Arith(Add, Var "i", Val 1))]);
	Def("bs", [], New(AByte, Val 10));
	Write(LVar "bs", Var "m");	
	Def("iter", ["f"; "seq"], Comp [
		For(["i", SVal "seq"], [Expr(Call("f", [Var "i"]))])
	]);
	Def("fold", ["f"; "v0"; "seq"], Comp [
		Def("t", [], Var "v0");
		For(["x", SVal "seq"], [ Write(LVar "t", Call("f", [Var "t"; Var "x"])) ]);
		Expr(Var "t")
	]);
	Def("printer", ["x"], Comp[Print(Var "x")]);
	(*Expr(Call("iter", [ Lambda(["x"], Comp [Print (Var "x")]); Var "m" ]));*)
	Expr(Call("iter", [Var "printer"; Var "m"]));
	Expr(Call("iter", [Var "printer"; Var "bs"]));
	Expr(Call("iter", [Var "printer"; Seq(SRange(Val 5, Val 15))]));
	Print(Call("fold", [ Var "sum"; Val 0; Var "m" ]));
	
	Def("add", [], Lambda(["x"], Lambda(["y"], Arith(Add, Var "x", Var "y"))));
	Print(Call("add", [Val 2; Val 3]));
	Def("five", [], Call("add", [Val 2; Val 3]));
	Print(Call("add", [Var "five"; Var "five"]));
	
	Def("map", ["f"; "m"], Comp [
		Def("r", [], New(AInt, Length (Var "m")));	
		For(["i", SRange(Val 0, Arith(Sub, Length(Var "m"), Val 1))], [Write(LArr("r", Var "i"), Call("f", [Arr("m", Var "i")]))]);
		Expr(Var "r")
	]);	
	
	Def("adder", [], Lambda(["y"], Arith(Add, Val 5, Var "y")));
	Def("a", [], Call("map", [Var "adder"; Var "m"]));
	
	Def("big", [], If(Less(Val 5, Var "five"), Val 1, Some (Val 0)));
	
	For(["i", SVal "a"], [ Expr(If(Less(Val 4, Var "i"), Comp [Print(Var "i")], None)) ]);
	
	Def("fib", ["n"], 
		Comp [
			Print(Var "n");
			Expr(If(Less(Var "n", Val 2), Val 1, 
				Some( Arith(Add, Call("fib", [Arith(Sub, Var "n", Val 1)]), Call("fib", [Arith(Sub, Var "n", Val 2)]) ))))
		]
	);
	Print(Call("fib", [Var "five"]));
	
	Def("printall", ["xs"; "i"],
		If(Less(Var "i", Length(Var "xs")), 
			Comp[ Print(Arr("xs", Var "i")); Expr(Call("printall", [Var "xs"; Arith(Add, Var "i", Val 1)])) ],
			None)
	);
	Expr(Call("printall", [Var "m"; Val 0]));
	Expr(Call("printall", [Var "bs"; Val 0]));
	
	Def("showhead", ["xs"], Comp [Print(Head(Var "xs"))]);
	Expr(Call("showhead", [Var "m"]));
	Expr(Call("showhead", [Var "bs"]));
	Expr(Call("showhead", [Seq(SRange(Val 2, Val 5))]));

	Def("showall", ["xs"],
		If(Less(Val 0, Length(Var "xs")), 
			Comp[ Print(Head(Var "xs")); Expr(Call("showall", [Tail(Var "xs")])) ],
			None)
	);
	
	Def("m2", [], Var "m");
			
	Expr(Call("showall", [Var "m"]));
	Expr(Call("showall", [Var "bs"]));
	Expr(Call("showall", [Seq(SRange(Val 2, Val 5))]));
	Expr(Call("showall", [SubArr("m", SRange(Val 5, Val 7))]));
	
	Write(LSubArr("m", SRange(Val 3, Val 6)), Var "a");
];;*)

let process prg show =
	if show then begin
		prg |> show_code 0 |> print_endline;
		print_endline "\n";
		let eprg = prg |> expand_code M.empty |> snd in
		eprg |> show_code 0 |> print_endline;
		print_endline "\n";
		let _, ccode = compile_code empty_context eprg in
		ccode |> Leoc.simp_code |> Leoc.show_code 0 |> print_endline;
		print_endline "\n";
		Leoc.process ccode
	end else
		prg |> expand_code M.empty |> snd |> compile_code empty_context |> snd |> Leoc.process;;

(*prg |> show_code 0 |> print_endline;;
print_endline "\n";;
let eprg = prg |> expand_code M.empty |> snd;;
eprg |> show_code 0 |> print_endline;;
print_endline "\n";;
let _, ccode = compile_code empty_context eprg;;
ccode |> Leoc.show_code 0 |> print_endline;;
print_endline "\n";;
Leoc.process ccode;;*)