open ExtLib
open Commons

type name = string
type array_type = AInt | AByte
type path = name * name list
type num = NVal of int | NVar of name
type field_type = FInt | FArray of array_type * num | FStruct of name
type struct_type = (name * field_type) list

type code = statement list
and statement =
	| Def of name * name list * expr
	| Write of lvalue * expr
	| Print of expr
	| Expr of expr
	| For of (name * expr) list * code
	| Ret of expr
	| Typedef of name * struct_type
	| Typing of name * name
	| Trash of bool
	| While of condition * code

and expr =
	| Val of int
	| LV of lvalue
	| Arith of oper * expr * expr
	| Call of name * expr list
	| Range of expr * expr
	| Length of expr
	| Head of expr
	| Tail of expr
	| New of array_type * expr list
	| If of condition * expr * expr option
	| Lambda of name list * expr
	| Comp of code

and lvalue =
	| Var of path
	| ArrElt of path * expr

and condition =
	| Less of expr * expr
	| Eq of expr * expr
	| And of condition * condition
	| Or of condition * condition
	| Not of condition;;

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
			Printf.sprintf "for %s\n%s\n%send" (List.map (fun (nm, sq) -> Printf.sprintf "%s in %s" nm (show_expr 0 sq)) name_seq_list |> String.concat ", ")
				(show_code (n + 1) code) (tab n "")
	| Ret e -> "return " ^ (show_expr n e)
	| Typedef(name, fields) ->
			let show_field (nm, ft) = Printf.sprintf "%s : %s;\n" nm (show_ftype ft) |> tab (n + 1) in
			let delim = tab n "" in
			Printf.sprintf "type %s = {\n%s%s" name (List.map show_field fields |> String.concat delim) (tab n "}")
	| Typing(varname, typename) -> Printf.sprintf "%s : %s" varname typename
	| Trash on -> if on then "$trash" else "$notrash"
	| While(con, code) -> Printf.sprintf "while %s\n%s\n%send" (show_cond n con) (show_code (n + 1) code) (tab n "")

and show_expr n = function
	| Val i -> string_of_int i
	| LV lv -> show_lvalue lv
	| Arith(op, e1, e2) -> Printf.sprintf "(%s %s %s)" (show_expr n e1) (show_op op) (show_expr n e2)
	| Call(name, elist) -> Printf.sprintf "%s(%s)" name (List.map (show_expr n) elist |> String.concat ", ")
	| Range(e1, e2) -> Printf.sprintf "%s..%s" (show_expr 0 e1) (show_expr 0 e2)
	| Length e -> Printf.sprintf "|%s|" (show_expr n e)
	| Head e -> (show_expr n e) ^ ".head"
	| Tail e -> (show_expr n e) ^ ".tail"
	| New(arrtype, es) -> Printf.sprintf "new %s[%s]" (show_atype arrtype) (List.map (show_expr n) es |> String.concat ", ")
	| If(con, e1, e2o) -> Printf.sprintf "if %s then %s %s" (show_cond n con) (show_expr n e1)
				(Option.map_default (show_expr n >> Printf.sprintf "else %s") "" e2o)
	| Lambda(name_list, e) -> Printf.sprintf "(%s) => %s" (String.concat ", " name_list) (show_expr n e)
	| Comp code -> if List.length code > 1 then Printf.sprintf "{\n%s\n%s}" (show_code (n + 1) code) (tab n "")
			else show_code (n + 1) code

and show_cond n = function
	| Less(e1, e2) -> Printf.sprintf "%s < %s" (show_expr n e1) (show_expr n e2)
	| Eq(e1, e2) -> Printf.sprintf "%s = %s" (show_expr n e1) (show_expr n e2)
	| And(con1, con2) -> Printf.sprintf "(%s && %s)" (show_cond n con1) (show_cond n con2)
	| Or(con1, con2) -> Printf.sprintf "(%s || %s)" (show_cond n con1) (show_cond n con2)
	| Not con -> Printf.sprintf "not (%s)" (show_cond n con)

and show_path (name, fields) =
	if fields = [] then name else	Printf.sprintf "%s.%s" name (String.concat "." fields)

and show_lvalue = function
	| Var path -> show_path path
	| ArrElt(path, e) -> Printf.sprintf "%s[%s]" (show_path path) (show_expr 0 e)

and show_ftype = function
	| FInt -> "int" | FStruct nm -> nm | FArray(aty, n) -> Printf.sprintf "%s[%s]" (show_atype aty) (show_num n)

and show_num = function NVal i -> string_of_int i | NVar nm -> nm;;
(*********************** recursion check ***********************************)

let rec is_recursive funname = function
	| Val i -> false
	| LV lv -> lvalue_uses_fun funname lv
	| Arith(op, e1, e2) -> (is_recursive funname e1) || (is_recursive funname e2)
	| Call(name, elist) -> funname = name || (List.exists (is_recursive funname) elist)
	| Range(e1, e2) -> (is_recursive funname e1) || (is_recursive funname e2)
	| Length e | Head e | Tail e -> is_recursive funname e
	| New(arrtype, es) -> List.exists (is_recursive funname) es
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
			(if List.mem funname (name:: arglist) then failwith ("function name already used: "^funname);
				is_recursive funname exp)
	| Write(_, e) | Print e	| Ret e	| Expr e -> is_recursive funname e
	| For(name_seq_list, code) -> (List.exists (snd >> is_recursive funname) name_seq_list) || (List.exists (stmt_uses_fun funname) code)
	| Typedef _ | Typing _ | Trash _ -> false
	| While(con, code) -> cond_uses_fun funname con || (List.exists (stmt_uses_fun funname) code)

and lvalue_uses_fun funname = function
	| Var (name, _) -> name = funname
	| ArrElt(_, e) -> is_recursive funname e

(*********************** expand ************************************)

type name_desc = NValue | NFun of name list * expr | NRecFun of int;;

let add_name ctx name desc = M.add name desc ctx;;
let get_name ctx name = M.find name ctx;;
let pathname = fst
let pathflds = snd
let mkpath name fields = name, fields
let mkvar name = LV(Var(name, []))

let rename map name =
	try (match M.find name map with LV(Var p) -> pathname p | _ -> name) with Not_found -> name

let rec expand_code ctx code =
	let ctx2, code2 =
		List.fold_left (fun (cx, cd) stmt -> let ctx1, st1 = expand_stmt cx stmt in ctx1, st1:: cd) (ctx, []) code in
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
			let ns = List.map (fun (n, s) -> n, expand_expr ctx s) name_seq_list in
			let _, ecode = expand_code ctx code in
			ctx, Some(For(ns, ecode))
	| Ret e -> ctx, Some(Ret(expand_expr ctx e))
	| Typedef _ | Typing _ | Trash _ as x -> ctx, Some x
	| While(con, code) ->
			let _, ecode = expand_code ctx code in
			ctx, Some(While(expand_con ctx con, ecode))

and expand_lvalue ctx = function
	| Var _ as x -> x
	| ArrElt(name, e) -> ArrElt(name, expand_expr ctx e)

and expand_expr ctx = function
	| Val i as x -> x
	| LV lv -> LV (expand_lvalue ctx lv)
	| Arith(op, e1, e2) -> Arith(op, expand_expr ctx e1, expand_expr ctx e2)
	| Call(name, elist) -> expand_call ctx name elist
	| Range(e1, e2) -> Range(expand_expr ctx e1, expand_expr ctx e2)
	| Length e -> Length(expand_expr ctx e)
	| Head e -> Head(expand_expr ctx e)
	| Tail e -> Tail(expand_expr ctx e)
	| New(arrtype, es) -> New(arrtype, List.map (expand_expr ctx) es)
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
	| Val _ | LV(Var _)	| Range _ -> argexp, None
	| Arith _ | Call _ | LV(ArrElt _) | Length _ | Head _ | Tail _ | New _ | If _ | Comp _ -> mkvar var, Some(Def(var, [], argexp))
	| Lambda(name_list, e) -> mkvar var, Some(Def(var, name_list, e))

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
					if np = narg then Call(name, List.map (expand_expr ctx) elist)
					else failwith (Printf.sprintf "wrong number of arguments for '%s' (%d instead of %d)" name narg np))
	with Not_found -> failwith (Printf.sprintf "function '%s' not found" name)

and subst_expr subs_map k = function
	| Val i as x -> x
	| LV lv -> subst_lvalue_expr subs_map k lv
	| Arith(op, e1, e2) -> Arith(op, subst_expr subs_map k e1, subst_expr subs_map k e2)
	| Call(name, elist) -> Call(rename subs_map name, List.map (subst_expr subs_map k) elist)
	| Range(e1, e2) -> Range(subst_expr subs_map k e1, subst_expr subs_map k e2)
	| Length e -> Length(subst_expr subs_map k e)
	| Head e -> Head(subst_expr subs_map k e)
	| Tail e -> Tail(subst_expr subs_map k e)
	| New(arrtype, es) -> New(arrtype, List.map (subst_expr subs_map k) es)
	| If(con, e1, e2o) -> If (subst_con subs_map k con, subst_expr subs_map k e1, Option.map (subst_expr subs_map k) e2o)
	| Lambda(name_list, e) -> Lambda(name_list, subst_expr subs_map k e)
	| Comp code -> Comp(subst_code subs_map k code)

and subst_con subs_map k = function
	| Less(e1, e2) -> Less(subst_expr subs_map k e1, subst_expr subs_map k e2)
	| Eq(e1, e2) -> Eq(subst_expr subs_map k e1, subst_expr subs_map k e2)
	| And(con1, con2) -> And(subst_con subs_map k con1, subst_con subs_map k con2)
	| Or(con1, con2) -> Or(subst_con subs_map k con1, subst_con subs_map k con2)
	| Not con -> Not(subst_con subs_map k con)

and subst_code subs_map k code =
	let subs, code2 =
		List.fold_left (fun (subs, cd) stmt -> let subs1, st1 = subst_stmt subs k stmt in subs1, st1:: cd) (subs_map, []) code in
	List.rev code2

and subst_stmt subs k = function
	| Def(name, arglist, exp) ->
			let name1 = Printf.sprintf "%s_%d" name k in
			let subs1 = M.add name (mkvar name1) subs in
			let subs2 = List.fold_left (fun m par_name -> M.remove par_name m) subs1 arglist in
			subs1, Def(name1, arglist, subst_expr subs2 k exp)
	| Write(lv, e) -> subs, Write(subst_lvalue subs k lv, subst_expr subs k e)
	| Print e -> subs, Print(subst_expr subs k e)
	| Expr e -> subs, Expr(subst_expr subs k e)
	| For(name_seq_list, code) ->
			let ns = List.map (fun (nm, sq) -> nm, subst_expr subs k sq) name_seq_list in
			subs, For(ns, subst_code subs k code)
	| Ret e -> subs, Ret(subst_expr subs k e)
	| Typedef _ | Typing _ | Trash _ as x -> subs, x
	| While(con, code) -> subs, While(subst_con subs k con, subst_code subs k code)

and subst_lvalue_expr subs k = function
	| Var path as x -> (try M.find (pathname path) subs with Not_found -> LV x)
	| ArrElt(path, e) -> LV (ArrElt(mkpath (rename subs (pathname path)) (pathflds path), subst_expr subs k e))

and subst_lvalue subs k lv =
	match subst_lvalue_expr subs k lv with
	| LV lv -> lv
	| e -> failwith "lvalue turned into expr after subst";;

(*************************** compile ********************************)

module C = Leoc
type comp_res = Code of Leoc.code | Func of name

type val_type = TVoid | TInt | TByte | TArray of val_type * num | TRange of num * num | TStruct of name

let rec show_type = function
	| TVoid -> "void"
	| TInt -> "int"
	| TByte -> "byte"
	| TArray(vtype, _) -> (show_type vtype) ^ " array"
	| TRange _ -> "range"
	| TStruct nm -> nm;;

type compiled_fun = { uname : name; cbody : C.statement; ftype : val_type }
type func_info = { params : name list; body : expr; compiled : (string * compiled_fun) list }
type compilation_context = { vars : val_type M.t; currfunc : name; curr_uname : string }
let funs = ref M.empty
let struct_types = ref M.empty
let empty_context = { vars = M.empty; currfunc = ""; curr_uname = "" }

let addvar ctx name ty =
	if !verbose then Printf.printf "# %s : %s\n" name (show_type ty);
	{ ctx with vars = M.add name ty ctx.vars };;

let gettype ctx name = try M.find name ctx.vars with Not_found -> failwith ("unknown type of "^name)
let vtype_of_atype = function	| AInt -> TInt | AByte -> TByte
let valtype_of_ftype = function
	| FInt -> TInt
	| FArray(atype, num) -> TArray(vtype_of_atype atype, num)
	| FStruct name -> TStruct name

let get_code = function
	| Code c -> c
	| Func name ->
			try (M.find name !funs).compiled |> List.map (fun (ts, cf) -> cf.cbody)
			with Not_found -> failwith ("unknown function "^name);;

let addfundef name params exp =
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

let rec compile_path ctx (name, flds) =
	if !verbose then Printf.printf "#compile_path %s\n" (show_path (name, flds));
	let rec loop lv ty fields lfields =
		match ty with
		| TInt | TByte -> ctx, [], ty, [C.LV lv]
		| TVoid -> ctx, [], ty, []
		| TArray(aty, alen) ->
				let ctx1, code, len =
					if lfields =[] then ctx, [], alen else
						match alen with
						| NVal i -> ctx, [], alen
						| NVar nm ->
								let p = name, nm :: (List.tl lfields) |> List.rev in
								let ctx1, code1, _, rc = compile_path ctx p in
								let ctx2, code2, rc2 =	num_of_rv ctx1 (List.hd rc)
								in ctx2, code1 @ code2, rc2 in
				ctx1, code, TArray(aty, len) , [C.LV lv; rv_of_num ctx1 len]
		| TRange(st, en) -> ctx, [], ty, [rv_of_num ctx st; rv_of_num ctx en]
		| TStruct sname ->
				match fields with
				| [] -> ctx, [], ty, [C.LV lv]
				| fld:: fs ->
						let sty = try M.find sname !struct_types with Not_found -> failwith ("unknown type "^sname) in
						let fty = try List.assoc fld sty with Not_found -> failwith (Printf.sprintf "unknown field %s in type %s" fld sname) in
						let vty = valtype_of_ftype fty in
						let fidx = List.findi (fun _ (fname, _) -> fname = fld) sty |> fst in
						let lv1 = C.PArith(Add, lv, C.Val(4 * fidx)) in
						loop lv1 vty fs (fld:: lfields) in
	loop (C.Var name) (gettype ctx name) flds []

and rv_of_num ctx = function NVal i -> C.Val i	| NVar nm -> C.LV(C.Var nm)

and num_of_rv ctx = function
	| C.Val i -> ctx, [], NVal i
	| C.LV(C.Var nm) -> ctx, [], NVar nm
	| rv -> let var = Printf.sprintf "bound_%d" (uid ()) in
			addvar ctx var TInt, [C.DefVar var; C.Assign(C.Var var, rv)], NVar var

let mix lst =
	let a = Array.of_list lst |> Array.map (fun x -> (Random.float 1.0, x)) in Array.sort compare a;
	Array.map snd a |> Array.to_list

let rec compile_code ctx code =
	let ctx1, code1 = List.fold_left (fun (cx, cd) stmt -> let cx1, st1 = compile_stmt cx stmt in cx1, st1:: cd) (ctx, []) code in
	ctx1, code1 |> List.rev |> List.map get_code |> List.concat

and compile_lvalue ctx = function
	| Var path -> compile_path ctx path
	| ArrElt(path, ie) ->
			let ctx1, code1, ety, erc = compile_expr ctx ie in
			let ctx2, code2, pty, prc = compile_path ctx1 path in
			let aty, alen, elt_size = match pty with
				| TArray(TInt, len) -> TInt, len, 4
				| TArray(TByte, len) -> TByte, len, 1
				| _ -> failwith (Printf.sprintf "%s is not an array or a bad one" (show_path path)) in
			let is_idx_range, idx_rv, end_rvo = match ety, erc with
				| TInt, [rv] -> false, rv, None
				| TByte, [rv] -> false, Leoc.Byte rv, None
				| TRange(n1, n2), [rv1; rv2] -> true, rv1, Some rv2
				| t, _ -> failwith (Printf.sprintf "bad type of array index: %s : %s" (show_expr 0 ie) (show_type t)) in
			let lv = match prc with C.LV lv :: _ -> lv | _ -> failwith "bad result of path compilation" in
			if is_idx_range then
				let lenvar = Printf.sprintf "arr_len_%d" (uid()) in
				let len_code = [C.DefVar lenvar;
					C.Assign(C.Var lenvar, C.Arith(Add, C.Arith(Sub, Option.get end_rvo, idx_rv), C.Val 1))] in
				let rv_begin = C.Arith(Mul, idx_rv, C.Val elt_size) in
				let rc = [C.Arith(Add, List.hd prc, rv_begin); C.LV(C.Var lenvar)] in
				ctx2, code1 @ code2 @ len_code, TArray(aty, NVar lenvar), rc
			else
				let lv_res = Leoc.PArith(Add, lv, Leoc.Arith(Mul, idx_rv, Leoc.Val elt_size)) in
				ctx2, code1 @ code2, aty, [C.LV lv_res]

and compile_stmt ctx = function
	| Def(name, [], If(con, e1, e2o)) ->
			let _, _, ty, _ = compile_expr ctx e1 in
			let ctx1 = addvar ctx name ty in
			let ctx2, compres = compile_stmt ctx1 (Write(Var (name, []), If(con, e1, e2o))) in
			ctx2, Code ([C.DefVar name] @ get_code compres)
	| Def(name, [], exp) ->
			let ctx1, code1, ty, rc = compile_expr ctx exp in
			let ctx2 = addvar ctx1 name ty in
			let code2 =	(match ty, rc with
					| TInt, [rv] -> [Leoc.DefVar name; Leoc.Assign(Leoc.Var name, rv)]
					| TByte, [rv] -> [Leoc.DefVar name; Leoc.Assignb(Leoc.Var name, rv)]
					| TArray(aty, alen), [rva; rvlen] -> [Leoc.DefVar name; Leoc.Assign(Leoc.Var name, rva)]
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
	| Write(lv, e) as orgst ->
			let ctx1, code1, lty, lrc = compile_lvalue ctx lv in
			let ctx2, code2, ty, rc = compile_expr ctx1 e in
			let code3 = match lty, ty, lrc, rc with
				| TInt, TInt, [C.LV lv], [rv] -> [Leoc.Assign(lv, rv)]
				| TInt, TByte, [C.LV lv], [rv] -> [Leoc.Assign(lv, Leoc.Byte rv)]
				| TByte, TByte, [C.LV lv], [rv]
				| TByte, TInt, [C.LV lv], [rv] -> [Leoc.Assignb(lv, rv)]
				| TArray _, TArray _, _, _
				| TArray _, TRange _, _, _ ->
						let k = uid () in
						let ivar = Printf.sprintf "i_%d" k and jvar = Printf.sprintf "j_%d" k in
						let st = For([ivar, LV lv; jvar, e], [
								Write(Var(mkpath ivar []), LV(Var(mkpath jvar [])))
								]) in
						(match compile_stmt ctx st with _, Code code' -> code' | _, _ -> failwith "bad result of array copy compilation")
				| TArray _, TInt,  _, _ 
				| TArray _, TByte, _, _ -> 
						let k = uid () in
						let ivar = Printf.sprintf "i_%d" k  in
						let st = For([ivar, LV lv], [
								Write(Var(mkpath ivar []), e)
								]) in
						(match compile_stmt ctx st with _, Code code' -> code' | _, _ -> failwith "bad result of array copy compilation")
				| _, _, _, _ -> failwith (Printf.sprintf "wrong types in assignment %s: %s and %s"
									(show_stmt 0 orgst) (show_type lty) (show_type ty)) in
			ctx, Code(code1 @ code2 @ code3)
	| Print e ->
			let _, code, ty, rc = compile_expr ctx e in
			(match ty, rc with
				| TInt, [rv] -> ctx, Code (code @ [Leoc.Print rv])
				| TByte, [rv] -> ctx, Code (code @ [Leoc.Print (Leoc.Byte rv)])
				| _, _ -> failwith (Printf.sprintf "bad argument type for print(%s): %s" (show_expr 0 e) (show_type ty)))
	| Expr e ->
			let _, code, ty, rc = compile_expr ctx e in
			let code2, rc2 = save_call rc in
			ctx, Code (code @ code2)
	| For(name_seq_list, code) ->
			let k = uid () in
			let mkname s = Printf.sprintf "%s_%d" s k and mkend s = Printf.sprintf "%s_end%d" s k in
			let stuff = name_seq_list |> List.map (fun (vname, exp) ->
								let iname = mkname vname and end_name = mkend vname in
								let def_code = [C.DefVar iname; C.DefVar end_name] in
								let _, ecode, ety, rc = compile_expr ctx exp in
								let init_code, delta, ity, clv = match ety, rc with
									| TRange(n1, n2), [rv1; rv2] ->
											[C.Assign(C.Var iname, rv1); C.Assign(C.Var end_name, C.Arith(Add, rv2, C.Val 1))], 1, TInt, C.Var iname
									| TArray(aty, alen), [rv1; rv_len] ->
											let rv_end, delta = match aty with
												| TByte -> C.Arith(Add, rv1, rv_len), 1
												| TInt -> C.Arith(Add, rv1, C.Arith(Mul, rv_len, C.Val 4)), 4
												| _ -> failwith "bad array type in for" in
											[C.Assign(C.Var iname, rv1); C.Assign(C.Var end_name, rv_end)], delta, aty, C.PVar iname
									| _, _ -> failwith "bad type in for" in
								let next_code = [Leoc.Assign(Leoc.Var iname, C.Arith(Add, C.LV(C.Var iname), C.Val delta))] in
								let cond = C.Less(C.LV(C.Var iname), C.LV(C.Var end_name)) in
								def_code, ecode @ init_code, next_code, iname, ity, clv, cond, vname ) in
			let def_code, init_code, next_code, ctx1, subs_map, conds =
				List.fold_right (fun (d, i, n, nm, ity, clv, cond, vname) (ds, is, ns, ctx, subs, conds) ->
								d @ ds, i @ is, n @ ns, addvar ctx vname ity, M.add vname clv subs, cond:: conds) stuff ([],[],[], ctx, M.empty, []) in
			let cond = List.fold_left (fun con cmp -> C.And(con, cmp)) (List.hd conds) (List.tl conds) in
			let _, ccode = compile_code ctx1 code in
			let ccode1 = Leoc.subst_code subs_map ccode in
			let all_code = def_code @ init_code @ [Leoc.While(cond, ccode1 @ next_code)] in
			ctx, Code [Leoc.Comp all_code]
	| While(con, code) ->
			let con_code, ccon = compile_cond ctx con in
			let _, ccode = compile_code ctx code in
			let all_code = con_code @ [Leoc.While(ccon, ccode)] in
			ctx, Code [Leoc.Comp all_code]
	| Ret e ->
			let _, code, ty, rc = compile_expr ctx e in
			(match ty, rc with
				| TInt, rvs -> ctx, Code (code @ [Leoc.Ret rvs])
				| TByte, rvs -> ctx, Code (code @ [Leoc.Ret (List.map (fun rv -> Leoc.Byte rv) rvs)])
				| TVoid, _ -> ctx, Code (code @ [Leoc.Ret []])
				| _, _ -> failwith (Printf.sprintf "trying to return a %s" (show_type ty)))
	| Typedef (name, ty) -> struct_types := M.add name ty !struct_types; ctx, Code []
	| Typing (varname, typename) -> addvar ctx varname (TStruct typename), Code []
	| Trash on -> ctx, Code [Leoc.Trash on]

and (compile_expr : compilation_context -> expr -> compilation_context * Leoc.code * val_type * Leoc.rvalue list) = fun ctx -> function
			| Val i -> ctx, [], TInt, [Leoc.Val i]
			| LV lv -> compile_lvalue ctx lv
			| Arith(op, e1, e2) ->
					let ctx1, code1, ty1, rc1 = compile_expr ctx e1 in
					let ctx2, code2, ty2, rc2 = compile_expr ctx1 e2 in
					let rv = Leoc.Arith(op, rv_of_rc ty1 rc1, rv_of_rc ty2 rc2) in
					ctx2, code1 @ code2, TInt, [rv]
			| Call(name, elist) ->
					let ctx1, code1, args, compfun = get_compiled_fun ctx name elist in
					ctx1, code1, compfun.ftype, [Leoc.FCall(compfun.uname, args)]
			| Range(e1, e2) ->
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
					ctx4, code1 @ code2 @ code3 @ code4, TRange(num1, num2), [rv_of_num ctx4 num1; rv_of_num ctx4 num2]
			| Length exp ->
					let ctx1, code1, ty, rc = compile_expr ctx exp in
					let rv = (match ty with
							| TArray(aty, alen) -> rv_of_num ctx alen
							| TRange(n1, n2) -> let rv1 = rv_of_num ctx n1 and rv2 = rv_of_num ctx n2 in
									Leoc.Arith(Add, Leoc.Arith(Sub, rv2, rv1), Leoc.Val 1)
							| _ -> failwith (Printf.sprintf "bad type in Length(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
					ctx1, code1, TInt, [rv]
			| Head exp ->
					let _, code1, ty, rc = compile_expr ctx exp in
					let hty, rv = (match ty with
							| TArray(aty, alen) -> aty, (match List.hd rc with
										| C.LV(C.Var v) -> C.LV(C.PVar v)
										| C.Arith(op, C.LV a, b) -> C.LV (C.PArith(op, a, b))
										| _ -> failwith "head of strange array")
							| TRange(n1, n2) -> TInt, rv_of_num ctx n1
							| _ -> failwith (Printf.sprintf "bad type in Head(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
					ctx, code1, hty, [rv]
			| Tail exp ->
					let _, code1, ty, rc1 = compile_expr ctx exp in
					let code2, tty, rvs = (match ty with
							| TArray(aty, alen) ->
									let code, len' = match alen with
										| NVal i -> [], NVal (i - 1)
										| NVar nm ->
												let lenvar = Printf.sprintf "arr_len_%d" (uid ()) in
												[C.DefVar lenvar; C.Assign(C.Var lenvar, C.Arith(Sub, C.LV(C.Var nm), C.Val 1))], NVar lenvar in
									let elt_size = match aty with TInt -> 4 | TByte -> 1 | _ -> failwith "Tail of an array of an unsuported type" in
									code, TArray(aty, len'), [C.Arith(Add, List.hd rc1, C.Val elt_size); rv_of_num ctx len']
							| TRange(n1, n2) ->
									let code, num1 = match n1 with
										| NVal i -> [], NVal (i + 1)
										| NVar nm ->
												let numvar = Printf.sprintf "bound_%d" (uid()) in
												[C.DefVar numvar; C.Assign(C.Var numvar, C.Arith(Add, C.LV(C.Var nm), C.Val 1))], NVar numvar in
									code, TRange(num1, n2), [rv_of_num ctx num1; rv_of_num ctx n2]
							| _ -> failwith (Printf.sprintf "bad type in Head(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
					ctx, code1 @ code2, tty, rvs
			| New(arrtype, [e]) ->
					let ctx1, code1, ety, erc = compile_expr ctx e in
					let size_rv = match ety, erc with
						| TInt, [rv] -> rv
						| TByte, [rv] -> Leoc.Byte rv
						| _, _ -> failwith "wrong size type in New" in
					let size_rv2 = match arrtype with AByte -> size_rv | AInt -> Leoc.Arith(Mul, size_rv, Leoc.Val 4) in
					let k = uid () in
					let alen, code2, ctx2 = (match size_rv with
							| C.Val i -> NVal i, [], ctx1
							| C.LV(C.Var v) -> NVar v, [], ctx1
							| _ -> let size_var = Printf.sprintf "array_length_%d" k in
									let ctx' = addvar ctx1 size_var TInt in
									NVar size_var, [Leoc.DefVar size_var; Leoc.Assign(Leoc.Var size_var, size_rv)], ctx') in
					let arr_var = Printf.sprintf "array_%d" k in
					let alloc_code = [Leoc.DefVar arr_var; Leoc.Alloc(Leoc.Var arr_var, size_rv2)] in
					let ty = match arrtype with AInt -> TArray(TInt, alen) | AByte -> TArray(TByte, alen) in
					ctx2, code1 @ code2 @ alloc_code, ty, [C.LV(C.Var arr_var); size_rv]
			| New(arrtype, es) ->
					let len = List.length es in
					let size_rv = match arrtype with AByte -> C.Val len | AInt -> C.Val (4 * len) in
					let k = uid () in
					let alen = NVal len in
					let arr_var = Printf.sprintf "array_%d" k in
					let alloc_code = [Leoc.DefVar arr_var; Leoc.Alloc(Leoc.Var arr_var, size_rv)] in
					let ty, mkasgn, elt_sz = match arrtype with
						| AInt -> TArray(TInt, alen), (fun l r -> Leoc.Assign(l, r)), 4
						| AByte -> TArray(TByte, alen), (fun l r -> Leoc.Assignb(l, r)), 1 in
					let ctx2 = addvar ctx arr_var ty in
					let ass_code = es |> List.mapi (fun idx e ->
										let stmt = Write(ArrElt((arr_var, []), Val idx), e) in
										let _, res = compile_stmt ctx2 stmt in
										get_code res) |> mix |> List.concat in
					ctx, alloc_code @ ass_code, ty, [C.LV(C.Var arr_var); C.Val len]
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
					let tmp_vars = List.map (fun _ -> Printf.sprintf "if_res_%d" (uid())) rc2 in
					let store rc =
						let rvs = Array.of_list rc in
						List.mapi (fun i tmp -> Leoc.Assign(C.Var tmp, rvs.(i))) tmp_vars in
					let def_tmp_vars = List.map (fun tmp -> Leoc.DefVar tmp) tmp_vars in
					let store1 = store rc1 and store2 = store rc2 in
					let res = List.map (fun tmp -> C.LV(C.Var tmp)) tmp_vars in
					ctx, con_code @ def_tmp_vars @ [Leoc.If(ccon, code1 @ store1, code2 @ store2)], ty2, res
			| Lambda(name_list, e) -> failwith "not expanded lambda expression"
			| Comp code ->
					(match List.rev code with
						| Expr e :: rest_code ->
								let ctx1, code01 = List.fold_left (fun (cx, cd) stmt ->
													let cx1, st1 = compile_stmt cx stmt in cx1, st1:: cd) (ctx, []) (List.rev rest_code) in
								let ctx2, code2, ty, rc = compile_expr ctx1 e in
								let ccode = code01 |> List.rev |> List.map get_code |> List.concat in
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
						let _, ecode, ety, erc = compile_expr ctx e in (ecode:: cd, ety:: tps, erc:: rvs)) ([],[],[]) elist in
	let codes, types, rcs = List.rev codes0, List.rev types0, List.rev rcs0 in
	let typestr = str_of_types types in
	let code = List.concat codes in
	let args = List.concat rcs in
	if name = ctx.currfunc then
		ctx, code, args, { uname = ctx.curr_uname; cbody = Leoc.Ret []; ftype = TInt }
	else
		(try ctx, code, args, List.assoc typestr fi.compiled with Not_found ->
				(let uname = Printf.sprintf "%s_%d" name (uid()) in
					let ftps0, pars0 = List.map2 (fun par ty ->
										match ty with
										| TVoid -> [ty], []
										| TInt | TByte | TStruct _ -> [ty], [par]
										| TArray(ety, _) -> let lenvar = par ^ "_len" in [TArray(ety, NVar lenvar); TInt], [par; lenvar]
										| TRange _ -> let n2 = par ^ "_end" in [TRange(NVar par, NVar n2); TInt], [par; n2]
							) fi.params types |> List.split in
					let pars = List.concat pars0 and ftps = List.concat ftps0 in
					let ctx0 = { vars = M.empty; currfunc = name; curr_uname = uname } in
					let fctx = List.fold_left2 addvar ctx0 (*fi.params*) pars ftps in
					let _, fcode, fty, frc = compile_expr fctx fi.body in
					let ret_code = if frc = [] then [] else [Leoc.Ret frc] in
					let st = Leoc.Defun(uname, pars, fcode @ ret_code) in
					let compfun = { uname = uname; cbody = st; ftype = fty } in
					let fi1 = { fi with compiled = (typestr, compfun):: fi.compiled } in
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
