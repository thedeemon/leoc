open ExtLib
open Commons

type name = string
type array_type = arg_size
type path = name * name list
type num = NVal of int | NVar of name
type field_type = FInt | FArray of array_type * num | FStruct of name
type struct_type = (name * field_type) list

let int_size = ref 4

type code = statement list
and statement = raw_statement * source_loc
and raw_statement =
	| Def of name * name list * expr
	| Write of lvalue * expr
	| Print of expr list
	| Expr of expr
	| For of (name * expr) list * code
	| Ret of expr
	| Typedef of name * struct_type
	| Typing of name * name
	| Trash of bool
	| While of condition * code
	| PostMessage of int * expr * expr

and expr = raw_expr * source_loc
and raw_expr =
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

(************************** map ********************************)
class mapper = object(self)
  method map_code code = List.map self#map_stmt code
	method map_stmt (stmt, sl) = self#map_raw_stmt stmt, sl	
	method map_raw_stmt = function
		| Def(name, name_list, expr) -> Def(name, name_list, self#map_expr expr)
		| Write(lv, expr) -> Write(self#map_lvalue lv, self#map_expr expr)
		| Print(expr_list) -> Print(List.map self#map_expr expr_list) 
		| Expr expr -> Expr(self#map_expr expr)
		| For(name_expr_list, code) -> For(List.map (fun (n,e) -> n, self#map_expr e) name_expr_list, self#map_code code)
		| Ret expr -> Ret(self#map_expr expr)
		| Typedef _ as x -> x
		| Typing _ as x -> x
		| Trash _ as x -> x
		| While(cond, code) -> While(self#map_cond cond, self#map_code code)
		| PostMessage(n, e1, e2) -> PostMessage(n, self#map_expr e1, self#map_expr e2)

	method map_expr (e,sl) = self#map_raw_expr e, sl
	method map_raw_expr = function
		| Val _ as x -> x
		| LV lv -> LV(self#map_lvalue lv)
		| Arith(oper, e1, e2) -> Arith(oper, self#map_expr e1, self#map_expr e2)
		| Call(name, expr_list) -> Call(name, List.map self#map_expr expr_list)
		| Range(e1, e2) -> Range(self#map_expr e1, self#map_expr e2)
		| Length e -> Length(self#map_expr e)
		| Head e -> Head(self#map_expr e)
		| Tail e -> Tail(self#map_expr e)
		| New(atype, expr_list) -> New(atype, List.map self#map_expr expr_list)
		| If(con, e1, e2o) -> If(self#map_cond con, self#map_expr e1, Option.map self#map_expr e2o)
		| Lambda(name_list, expr) -> Lambda(name_list, self#map_expr expr)
		| Comp code -> Comp(self#map_code code)

	method map_lvalue = function
		| Var _ as x -> x
		| ArrElt(path, expr) -> ArrElt(path, self#map_expr expr)

	method map_cond = function
		| Less(e1, e2) -> Less(self#map_expr e1, self#map_expr e2)
		| Eq(e1, e2) -> Eq(self#map_expr e1, self#map_expr e2)
		| And(con1, con2) -> And(self#map_cond con1, self#map_cond con2)
		| Or(con1, con2) -> Or(self#map_cond con1, self#map_cond con2)
		| Not con -> Not(self#map_cond con)
end


(************************** pretty printing ********************************)

let show_args lst =
	if lst = [] then "" else Printf.sprintf "(%s)" (String.concat ", " lst);;

let show_atype = function ASInt -> "int" | ASByte -> "byte" | ASInt32 -> "int32";;

let rec show_code n code =
	code |> List.map (fst >> show_stmt n >> tab n) |> String.concat "\n"

and show_stmt n = function
	| Def(name, arglist, exp) -> Printf.sprintf "%s%s = %s" name (show_args arglist) (show_expr n exp)
	| Write(lv, exp) -> Printf.sprintf "%s <- %s" (show_lvalue lv) (show_expr n exp)
	| Print es -> Printf.sprintf "print(%s)" (List.map (show_expr n) es |> String.concat ", ")
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
	| PostMessage(msg, e1, e2) -> Printf.sprintf "PostMessage(%d, %s, %s)" msg (show_expr n e1) (show_expr n e2)

and show_expr n (expr,sl) = match expr with
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

exception Yes
let is_recursive funname exp = 
	let o = object(self)
		inherit mapper as super
		method map_raw_expr e = match e with
			| Call(name, elist) when funname = name -> raise Yes 
			| _ -> super#map_raw_expr e
	end in
	try ignore(o#map_expr exp); false with Yes -> true 

(*********************** expand ************************************)

type name_desc = NValue | NFun of name list * expr | NRecFun of int;;

let add_name ctx name desc = M.add name desc ctx;;
let get_name ctx name = M.find name ctx;;
let pathname = fst
let pathflds = snd
let mkpath name fields = name, fields
let mkvar name = LV(Var(name, []))

let rename map name =
	try (match M.find name map with (LV(Var p),_) -> pathname p | _ -> name) with Not_found -> name
	
let rec expand_code ctx code =
	let ctx2, code2 =
		List.fold_left (fun (cx, cd) (rstmt, sl) -> 
			let o = expander cx in
			let ctx1, st1 = o#expand_raw_stmt rstmt in
		  ctx1, (st1, sl):: cd) (ctx, []) code in
	code2 |> List.enum |> Enum.map (fun (sto, sl)-> Option.map (fun st-> st,sl) sto) |>
	Enum.filter_map identity |> List.of_enum |> List.rev

and expander ctx = object(self)
	inherit mapper as super
			
	method map_raw_expr = function
		| Call(name, elist) -> self#expand_call name elist
		| e -> super#map_raw_expr e
			
 	method map_code code = expand_code ctx code
			
	method expand_raw_stmt = function
		| Def(name, arglist, (Lambda(params, e),_)) ->
				self#expand_raw_stmt (Def(name, arglist @ params, e))
		| Def(name, arglist, exp) ->
				if arglist = [] then add_name ctx name NValue, Some (Def(name, [], self#map_expr exp))
				else
				if is_recursive name exp then
					let ctx1 = add_name ctx name (NRecFun (List.length arglist)) in
					ctx1, Some (Def(name, arglist, (*expand_expr ctx1*) exp))
				else add_name ctx name (NFun(arglist, exp)), None
		| stmt -> ctx, Some(self#map_raw_stmt stmt)

	method expand_call name elist =
		try
			(match get_name ctx name with
			| NValue -> failwith (Printf.sprintf "'%s' is not a function" name)
			| NFun(name_list, exp) ->
					if List.length name_list <> List.length elist then failwith (Printf.sprintf "wrong number of arguments for '%s'" name);
					let k = uid () in
					let subs, calcs = List.combine name_list elist |> List.map (argval k) |> List.split in
					(*Printf.printf "expanding %s(%s):\n" name (String.join ", " name_list);*)  
					let subs_map = List.fold_left2 (fun m argname argexp -> 
						(*Printf.printf "%s => %s;\n" argname (show_expr 0 argexp);*)
						M.add argname argexp m) M.empty name_list subs in
					(*Printf.printf "body:\n%s\n" (show_expr 0 exp);*)
					let e = subst_expr subs_map k exp in
					let precalcs = (List.enum calcs |> Enum.filter_map identity |> List.of_enum) @ [Expr e, no_source] in
					Comp(expand_code ctx precalcs)
			| NRecFun np ->
					let narg = List.length elist in
					if np = narg then Call(name, List.map self#map_expr elist)
					else failwith (Printf.sprintf "wrong number of arguments for '%s' (%d instead of %d)" name narg np))
		with Not_found -> failwith (Printf.sprintf "function '%s' not found" name)
end 

and argval k (argname, argexp) =
	let var = Printf.sprintf "%s_%d" argname k in
	let sl = snd argexp in
	match fst argexp with
	| Val _ | LV(Var _)	| Range _ -> argexp, None
	| Arith _ | Call _ | LV(ArrElt _) | Length _ | Head _ | Tail _ | New _ | If _ | Comp _ -> 
			(mkvar var, sl), Some(Def(var, [], argexp), sl)
	| Lambda(name_list, e) -> (mkvar var, sl), Some(Def(var, name_list, e), sl)

and subster (subs_map: expr Commons.M.t) k = object(self)
	inherit mapper as super
	method map_code code = subst_code subs_map k code
	
	method subst_stmt (st, sl) = match st with  
		| Def(name, arglist, exp) ->
				let name1 = Printf.sprintf "%s_%d" name k in
				let subs1 = M.add name ((mkvar name1),sl) subs_map in
				let subs2 = List.fold_left (fun m par_name -> M.remove par_name m) subs1 arglist in
				subs1, Def(name1, arglist, subst_expr subs2 k exp)
		| stmt -> subs_map, self#map_raw_stmt stmt

	method map_lvalue lv =
		match self#subst_lvalue_expr lv with 
		| LV lv, _ -> lv
		| e -> failwith (Printf.sprintf "lvalue turned into expr after subst:\n%s\n%s #line %d" 
											(show_lvalue lv) (show_expr 0 e) (snd e))
	
	method subst_lvalue_expr = function
		| Var path as x -> (try M.find (pathname path) subs_map with Not_found -> LV x, no_source)
		| ArrElt(path, e) -> LV (ArrElt(mkpath (rename subs_map (pathname path)) (pathflds path), self#map_expr e)), no_source

	method map_raw_expr = function
		| Call(name, elist) -> Call(rename subs_map name, List.map self#map_expr elist)
		| LV lv -> self#subst_lvalue_expr lv |> fst
		| e -> super#map_raw_expr e
end

and subst_code subs_map k code =
	let subs, code2 =
		List.fold_left (fun (subs, cd) stmt ->
			let o = subster subs k in
			let subs1, st1 = o#subst_stmt stmt				
			in subs1, (st1, snd stmt):: cd) (subs_map, []) code in
	List.rev code2

and subst_expr subs k exp = 
	let o = subster subs k in	o#map_expr exp
	
(*************************** compile ********************************)

module C = Leoc
type comp_res = Code of Leoc.code | Func of name

type val_type = TVoid | TInt | TInt32 | TByte | TArray of val_type * num | TRange of num * num | TStruct of name

let rec show_type = function
	| TVoid -> "void"
	| TInt -> "int"
	| TInt32 -> "int32"
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
let vtype_of_atype = function	| ASInt -> TInt | ASByte -> TByte | ASInt32 -> TInt32
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
	| [C.FCall(name, rvs),sl] -> [C.Call(name, rvs),sl], []
	| x -> [], x

let rec returnize (exp,loc) = match exp with
	| If(con, e1, e2o) -> If(con, returnize e1, Option.map returnize e2o), loc
	| Comp code as x -> (match List.rev code with
				| (Expr e, sl) :: rest -> Comp(List.rev ((Expr(returnize e), sl) :: rest))
				| _ -> x), loc
	| e -> Comp [Ret(e,loc), loc], loc

let rec compile_path ctx sl (name, flds) =
	if !verbose then Printf.printf "#compile_path %s\n" (show_path (name, flds));
	let rec loop lv ty fields lfields =
		match ty with
		| TInt | TByte | TInt32 -> ctx, [], ty, [C.LV lv, sl]
		| TVoid -> ctx, [], ty, []
		| TArray(aty, alen) ->
				let ctx1, code, len =
					if lfields =[] then ctx, [], alen else
						match alen with
						| NVal i -> ctx, [], alen
						| NVar nm ->
								let p = name, nm :: (List.tl lfields) |> List.rev in
								let ctx1, code1, _, rc = compile_path ctx sl p in
								let ctx2, code2, rc2 =	num_of_rv ctx1 (List.hd rc)
								in ctx2, code1 @ code2, rc2 in
				ctx1, code, TArray(aty, len) , [C.LV lv, sl; rv_of_num ctx1 sl len]
		| TRange(st, en) -> ctx, [], ty, [rv_of_num ctx sl st; rv_of_num ctx sl en]
		| TStruct sname ->
				match fields with
				| [] -> ctx, [], ty, [C.LV lv, sl]
				| fld:: fs ->
						let sty = try M.find sname !struct_types with Not_found -> failwith ("unknown type "^sname) in
						let fty = try List.assoc fld sty with Not_found -> failwith (Printf.sprintf "unknown field %s in type %s" fld sname) in
						let vty = valtype_of_ftype fty in
						let fidx = List.findi (fun _ (fname, _) -> fname = fld) sty |> fst in
						let lv1 = C.PArith(Add, lv, (C.Val(!int_size * fidx), sl)) in
						loop lv1 vty fs (fld:: lfields) in
	loop (C.Var name) (gettype ctx name) flds []

and rv_of_num ctx sl = function NVal i -> C.Val i, sl	| NVar nm -> C.LV(C.Var nm), sl

and num_of_rv ctx ((rval,sl) as org) = match rval with
	| C.Val i -> ctx, [], NVal i
	| C.LV(C.Var nm) -> ctx, [], NVar nm
	| rv -> let var = Printf.sprintf "bound_%d" (uid ()) in
			addvar ctx var TInt, [C.DefVar var, sl; C.Assign(ASInt, C.Var var, org), sl], NVar var

let mix lst =
	let a = Array.of_list lst |> Array.map (fun x -> (Random.float 1.0, x)) in Array.sort compare a;
	Array.map snd a |> Array.to_list

let rec compile_code ctx code =
	let ctx1, code1 = List.fold_left (fun (cx, cd) stmt -> let cx1, st1 = compile_stmt cx stmt in cx1, st1:: cd) (ctx, []) code in
	ctx1, code1 |> List.rev |> List.map get_code |> List.concat

and compile_lvalue ctx sl = function
	| Var path -> compile_path ctx sl path
	| ArrElt(path, ie) ->
			let ctx1, code1, ety, erc = compile_expr ctx ie in
			let ctx2, code2, pty, prc = compile_path ctx1 sl path in
			let esl = snd ie in
			let aty, alen, elt_size = match pty with
				| TArray(TInt, len) -> TInt, len, !int_size
				| TArray(TInt32, len) -> TInt32, len, 4
				| TArray(TByte, len) -> TByte, len, 1
				| _ -> failwith (Printf.sprintf "%s is not an array or a bad one" (show_path path)) in
			let is_idx_range, idx_rv, end_rvo = match ety, erc with
				| TInt, [rv] -> false, rv, None
				| TByte, [rv] -> false, (Leoc.Byte rv, esl), None
				| TRange(n1, n2), [rv1; rv2] -> true, rv1, Some rv2
				| t, _ -> failwith (Printf.sprintf "bad type of array index: %s : %s" (show_expr 0 ie) (show_type t)) in
			let lv = match prc with (C.LV lv, _) :: _ -> lv | _ -> failwith "bad result of path compilation" in
			if is_idx_range then
				let lenvar = Printf.sprintf "arr_len_%d" (uid()) in
				let len_code = [C.DefVar lenvar, sl;
					C.Assign(ASInt, C.Var lenvar, 
					        (C.Arith(Add, (C.Arith(Sub, Option.get end_rvo, idx_rv), esl), (C.Val 1, esl)), esl)),sl] in
				let rv_begin = C.Arith(Mul, idx_rv, (C.Val elt_size, esl)),esl in
				let rc = [C.Arith(Add, List.hd prc, rv_begin), esl; C.LV(C.Var lenvar), esl] in
				ctx2, code1 @ code2 @ len_code, TArray(aty, NVar lenvar), rc
			else
				let lv_res = Leoc.PArith(Add, lv, (Leoc.Arith(Mul, idx_rv, (Leoc.Val elt_size,esl)),sl)) in
				ctx2, code1 @ code2, aty, [C.LV lv_res, sl]

and compile_stmt ctx (stmt, loc) = match stmt with
	| Def(name, [], (If(con, e1, e2o), sl)) ->
			let _, _, ty, _ = compile_expr ctx e1 in
			let ctx1 = addvar ctx name ty in
			let ctx2, compres = compile_stmt ctx1 (Write(Var (name, []), (If(con, e1, e2o),sl)), loc) in
			ctx2, Code ([C.DefVar name,loc] @ get_code compres)
	| Def(name, [], exp) ->
			let ctx1, code1, ty, rc = compile_expr ctx exp in
			let ctx2 = addvar ctx1 name ty in
			let code2 =	(match ty, rc with
					| TInt, [rv] -> [Leoc.DefVar name, loc; Leoc.Assign(ASInt, Leoc.Var name, rv), loc]
					| TByte, [rv] -> [Leoc.DefVar name, loc; Leoc.Assign(ASByte, Leoc.Var name, rv), loc]
					| TArray(aty, alen), [rva; rvlen] -> [Leoc.DefVar name, loc; Leoc.Assign(ASInt, Leoc.Var name, rva), loc]
					| TRange(n1, n2), [rv1; rv2] -> [] (* data already exist and referenced in type *)
					| TVoid, _ -> failwith ("defining a void value "^name)
					| _, _ -> failwith "bad number of values in Def") in
			ctx2, Code(code1 @ code2)
	| Def(name, arglist, exp) ->
			addfundef name arglist (returnize exp);
			ctx, Func name
	| Write(lv, (If(con, e1, e2o),sl)) ->
			let e2o' = Option.map (fun e -> Comp[Write(lv, e), snd e], snd e) e2o in
			compile_stmt ctx (Expr(If(con, (Comp[Write(lv, e1), snd e1], snd e1), e2o' ),sl),loc)
	| Write(lv, e) as orgst ->
			let ctx1, code1, lty, lrc = compile_lvalue ctx loc lv in
			let ctx2, code2, ty, rc = compile_expr ctx1 e in
			let sl = snd e in
			let code3 = match lty, ty, lrc, rc with
				| TInt, TInt, [C.LV lv,_], [rv] -> [Leoc.Assign(ASInt, lv, rv), loc]
				| TInt, TInt32, [C.LV lv,_], [rv] -> [Leoc.Assign(ASInt32, lv, rv), loc]
				| TInt32, TInt, [C.LV lv,_], [rv] -> [Leoc.Assign(ASInt32, lv, rv), loc]
				| TInt32, TInt32, [C.LV lv,_], [rv] -> [Leoc.Assign(ASInt32, lv, rv), loc]
				| TInt, TByte, [C.LV lv,_], [rv] -> [Leoc.Assign(ASInt, lv, (Leoc.Byte rv, snd rv)), loc]
				| TInt32, TByte, [C.LV lv,_], [rv] -> [Leoc.Assign(ASInt32, lv, (Leoc.Byte rv, snd rv)), loc]
				| TByte, TByte, [C.LV lv,_], [rv]
				| TByte, TInt, [C.LV lv,_], [rv] -> [Leoc.Assign(ASByte, lv, rv), loc]
				| TByte, TInt32, [C.LV lv,_], [rv] -> [Leoc.Assign(ASByte, lv, rv), loc]
				| TArray _, TArray _, _, _
				| TArray _, TRange _, _, _ ->
						let k = uid () in
						let ivar = Printf.sprintf "i_%d" k and jvar = Printf.sprintf "j_%d" k in
						let st = For([ivar, (LV lv, sl); jvar, e], [
								Write(Var(mkpath ivar []), (LV(Var(mkpath jvar [])), sl)), sl
								]) in
						(match compile_stmt ctx (st,sl) with _, Code code' -> code' | _, _ -> failwith "bad result of array copy compilation")
				| TArray _, TInt,  _, _ 
				| TArray _, TInt32,  _, _ 
				| TArray _, TByte, _, _ -> 
						let k = uid () in
						let ivar = Printf.sprintf "i_%d" k  in
						let st = For([ivar, (LV lv, sl)], [
								Write(Var(mkpath ivar []), e), sl
								]) in
						(match compile_stmt ctx (st,sl) with _, Code code' -> code' | _, _ -> failwith "bad result of array copy compilation")
				| _, _, _, _ -> failwith (Printf.sprintf "wrong types in assignment %s: %s and %s"
									(show_stmt 0 orgst) (show_type lty) (show_type ty)) in
			ctx, Code(code1 @ code2 @ code3)
	| Print es ->
			let print_code e = 
				let _, code, ty, rc = compile_expr ctx e in
				let sl = snd e in
				let prcode = 
					match ty, rc with
					| TInt, [rv] -> [Leoc.Print rv, sl]
					| TInt32, [rv] -> [Leoc.Print rv, sl]
					| TByte, [rv] -> [Leoc.Print (Leoc.Byte rv, snd rv), sl]
					| TArray _, _ ->
							let k = uid () in
							let ivar = Printf.sprintf "i_%d" k  in
							let st = For([ivar, e], [
									Print([LV(Var(mkpath ivar [])), sl]),sl
								]) in
							(match compile_stmt ctx (st,sl) with 
								| _, Code cd -> Leoc.subst_code_by_stmt (function Leoc.Print x -> Some(Leoc.Prchar x) | _ -> None) cd									 
								| _ -> failwith "bad result of print compile")						 
					| _, _ -> failwith (Printf.sprintf "bad argument type for print(%s): %s" (show_expr 0 e) (show_type ty)) in
				code @ prcode  in
			ctx, Code(List.map print_code es |> List.concat)
	| Expr e ->
			let _, code, ty, rc = compile_expr ctx e in
			let code2, rc2 = save_call rc in
			ctx, Code (code @ code2)
	| For(name_seq_list, code) ->
			let k = uid () in
			let mkname s = Printf.sprintf "%s_%d" s k and mkend s = Printf.sprintf "%s_end%d" s k in
			let stuff = name_seq_list |> List.map (fun (vname, exp) ->
								let iname = mkname vname and end_name = mkend vname in
								let def_code = [C.DefVar iname,loc; C.DefVar end_name,loc] in
								let _, ecode, ety, rc = compile_expr ctx exp in
								let init_code, delta, ity, clv = match ety, rc with
									| TRange(n1, n2), [rv1; rv2] ->
											[C.Assign(ASInt, C.Var iname, rv1),loc; 
											 C.Assign(ASInt, C.Var end_name, (C.Arith(Add, rv2, (C.Val 1,loc)),loc)),loc], 1, TInt, C.Var iname
									| TArray(aty, alen), [rv1; rv_len] ->
											let rv_end, delta = match aty with
												| TByte -> C.Arith(Add, rv1, rv_len), 1
												| TInt32 -> C.Arith(Add, rv1, (C.Arith(Mul, rv_len, (C.Val 4,loc)),loc)), 4
												| TInt -> C.Arith(Add, rv1, (C.Arith(Mul, rv_len, (C.Val !int_size,loc)),loc)), !int_size
												| _ -> failwith "bad array type in for" in
											[C.Assign(ASInt, C.Var iname, rv1),loc; 
											 C.Assign(ASInt, C.Var end_name, (rv_end,loc)),loc], delta, aty, C.PVar iname
									| _, _ -> failwith "bad type in for" in
								let next_code = [C.Assign(ASInt, Leoc.Var iname, 
								                 (C.Arith(Add, (C.LV(C.Var iname),loc), (C.Val delta,loc)),loc)),loc] in
								let cond = C.Less((C.LV(C.Var iname),loc), (C.LV(C.Var end_name),loc)) in
								def_code, ecode @ init_code, next_code, iname, ity, clv, cond, vname ) in
			let def_code, init_code, next_code, ctx1, subs_map, conds =
				List.fold_right (fun (d, i, n, nm, ity, clv, cond, vname) (ds, is, ns, ctx, subs, conds) ->
								d @ ds, i @ is, n @ ns, addvar ctx vname ity, M.add vname clv subs, cond:: conds) stuff ([],[],[], ctx, M.empty, []) in
			let cond = List.fold_left (fun con cmp -> C.And(con, cmp)) (List.hd conds) (List.tl conds) in
			let _, ccode = compile_code ctx1 code in
			let ccode1 = Leoc.subst_code_by_lvalue subs_map ccode in
			let all_code = def_code @ init_code @ [Leoc.While(cond, ccode1 @ next_code),loc] in
			ctx, Code [Leoc.Comp all_code,loc]
	| While(con, code) ->
			let con_code, ccon = compile_cond ctx con in
			let _, ccode = compile_code ctx code in
			let all_code = con_code @ [Leoc.While(ccon, ccode),loc] in
			ctx, Code [Leoc.Comp all_code,loc]
	| Ret e ->
			let _, code, ty, rc = compile_expr ctx e in
			let sl = snd e in
			(match ty, rc with
				| TInt, rvs -> ctx, Code (code @ [Leoc.Ret rvs, sl])
				| TByte, rvs -> ctx, Code (code @ [Leoc.Ret (List.map (fun rv -> (Leoc.Byte rv, snd rv)) rvs),sl])
				| TVoid, _ -> ctx, Code (code @ [Leoc.Ret [],sl])
				| _, _ -> failwith (Printf.sprintf "trying to return a %s" (show_type ty)))
	| Typedef (name, ty) -> struct_types := M.add name ty !struct_types; ctx, Code []
	| Typing (varname, typename) -> addvar ctx varname (TStruct typename), Code []
	| Trash on -> ctx, Code [Leoc.Trash on, loc]
	| PostMessage(msg, e1, e2) ->
			let _, code1, ty1, rc1 = compile_expr ctx e1 
			and _, code2, ty2, rc2 = compile_expr ctx e2 in
			match ty1, ty2, rc1, rc2 with
			| TInt, TInt, [rv1], [rv2] -> ctx, Code[Leoc.PostMessage(msg, rv1, rv2), loc]
			| _, _, _, _ -> failwith "not ints in PostMessage" 

and (compile_expr : compilation_context -> expr -> compilation_context * Leoc.code * val_type * Leoc.rvalue list) = 
	fun ctx (rexp,loc) -> match rexp with
			| Val i -> ctx, [], TInt, [Leoc.Val i, loc]
			| LV lv -> compile_lvalue ctx loc lv
			| Arith(op, e1, e2) ->
					let ctx1, code1, ty1, rc1 = compile_expr ctx e1 in
					let ctx2, code2, ty2, rc2 = compile_expr ctx1 e2 in
					let rv = Leoc.Arith(op, rv_of_rc ty1 rc1, rv_of_rc ty2 rc2),loc in
					ctx2, code1 @ code2, TInt, [rv]
			| Call(name, elist) ->
					let ctx1, code1, args, compfun = get_compiled_fun ctx name elist in
					ctx1, code1, compfun.ftype, [Leoc.FCall(compfun.uname, args),loc]
			| Range(e1, e2) ->
					let ctx1, code1, ty1, rc1 = compile_expr ctx e1 in
					let ctx2, code2, ty2, rc2 = compile_expr ctx1 e2 in
					let to_int ty rv = if ty = TByte then (C.Byte rv, snd rv) else rv in
					let is_num ty = ty = TInt || ty = TByte in
					let check_num ty e =
						if not(is_num ty) then failwith ("not a number in range constructor: " ^ (show_expr 0 e)) in
					check_num ty1 e1;
					check_num ty2 e2;
					let ctx3, code3, num1 = num_of_rv ctx2 (to_int ty1 (List.hd rc1)) in
					let ctx4, code4, num2 = num_of_rv ctx3 (to_int ty2 (List.hd rc2)) in
					ctx4, code1 @ code2 @ code3 @ code4, TRange(num1, num2), [rv_of_num ctx4 (snd e1) num1; rv_of_num ctx4 (snd e2) num2]
			| Length exp ->
					let ctx1, code1, ty, rc = compile_expr ctx exp in
					let rv = (match ty with
							| TArray(aty, alen) -> rv_of_num ctx loc alen
							| TRange(n1, n2) -> let rv1 = rv_of_num ctx loc n1 and rv2 = rv_of_num ctx loc n2 in
									Leoc.Arith(Add, (Leoc.Arith(Sub, rv2, rv1), loc), (Leoc.Val 1, loc)), loc
							| _ -> failwith (Printf.sprintf "bad type in Length(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
					ctx1, code1, TInt, [rv]
			| Head exp ->
					let _, code1, ty, rc = compile_expr ctx exp in
					let hty, rv = (match ty with
							| TArray(aty, alen) -> aty, (match List.hd rc with
										| C.LV(C.Var v),_ -> C.LV(C.PVar v), loc
										| C.Arith(op, (C.LV a, _), b),_ -> C.LV (C.PArith(op, a, b)), loc
										| _ -> failwith "head of strange array")
							| TRange(n1, n2) -> TInt, rv_of_num ctx loc n1
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
												[C.DefVar lenvar, loc; 
												 C.Assign(ASInt, C.Var lenvar, 
												  (C.Arith(Sub, (C.LV(C.Var nm),loc), (C.Val 1,loc)),loc)), loc], NVar lenvar in
									let elt_size = match aty with 
										| TInt32 -> 4 | TInt -> !int_size | TByte -> 1 
										| _ -> failwith "Tail of an array of an unsuported type" in
									code, TArray(aty, len'), [C.Arith(Add, List.hd rc1, (C.Val elt_size,loc)), loc; rv_of_num ctx loc len']
							| TRange(n1, n2) ->
									let code, num1 = match n1 with
										| NVal i -> [], NVal (i + 1)
										| NVar nm ->
												let numvar = Printf.sprintf "bound_%d" (uid()) in
												[C.DefVar numvar, loc; 
												 C.Assign(ASInt, C.Var numvar, (C.Arith(Add, (C.LV(C.Var nm),loc), (C.Val 1, loc)),loc)), loc], NVar numvar in
									code, TRange(num1, n2), [rv_of_num ctx loc num1; rv_of_num ctx loc n2]
							| _ -> failwith (Printf.sprintf "bad type in Head(%s), type: %s" (show_expr 0 exp) (show_type ty))) in
					ctx, code1 @ code2, tty, rvs
			| New(arrtype, [e]) ->
					let ctx1, code1, ety, erc = compile_expr ctx e in
					let size_rv = match ety, erc with
						| TInt, [rv] -> rv
						| TByte, [rv] -> (Leoc.Byte rv, snd rv)
						| _, _ -> failwith "wrong size type in New" in
					let size_rv2 = match arrtype with 
						| ASByte -> size_rv 
						| ASInt -> Leoc.Arith(Mul, size_rv, (Leoc.Val !int_size, loc)), loc 
						| ASInt32 -> Leoc.Arith(Mul, size_rv, (Leoc.Val 4, loc)), loc in
					let k = uid () in
					let alen, code2, ctx2 = (match size_rv with
							| C.Val i,_ -> NVal i, [], ctx1
							| C.LV(C.Var v),_ -> NVar v, [], ctx1
							| _ -> let size_var = Printf.sprintf "array_length_%d" k in
									let ctx' = addvar ctx1 size_var TInt in
									NVar size_var, [Leoc.DefVar size_var, loc; Leoc.Assign(ASInt, Leoc.Var size_var, size_rv), loc], ctx') in
					let arr_var = Printf.sprintf "array_%d" k in
					let alloc_code = [Leoc.DefVar arr_var, loc; Leoc.Alloc(Leoc.Var arr_var, size_rv2), loc] in
					let ty = TArray(vtype_of_atype arrtype, alen) in
					ctx2, code1 @ code2 @ alloc_code, ty, [C.LV(C.Var arr_var),loc; size_rv]
			| New(arrtype, es) ->
					let len = List.length es in
					let size_rv = match arrtype with 
						| ASByte -> C.Val len 
						| ASInt32 -> C.Val (4 * len) 
						| ASInt -> C.Val (!int_size * len) in
					let k = uid () in
					let alen = NVal len in
					let arr_var = Printf.sprintf "array_%d" k in
					let alloc_code = [Leoc.DefVar arr_var, loc; Leoc.Alloc(Leoc.Var arr_var, (size_rv,loc)), loc] in
					(*let elt_sz = match arrtype with ASByte -> 1 | ASInt32 -> 4 | ASInt -> !int_size in*)
					let ty, mkasgn = TArray(vtype_of_atype arrtype, alen), (fun l r -> Leoc.Assign(arrtype, l, r)) in
					let ctx2 = addvar ctx arr_var ty in
					let ass_code = es |> List.mapi (fun idx e ->
										let stmt = Write(ArrElt((arr_var, []), (Val idx, loc)), e) in
										let _, res = compile_stmt ctx2 (stmt,loc) in
										get_code res) |> mix |> List.concat in
					ctx, alloc_code @ ass_code, ty, [C.LV(C.Var arr_var), loc; C.Val len, loc]
			| If(con, e1, e2o) ->
					let con_code, ccon = compile_cond ctx con in
					let _, code1, ty1, rc1 = compile_expr ctx e1 in
					let _, code2, ty2, rc2 = Option.map_default (compile_expr ctx) (ctx, [], TVoid, []) e2o in
					let types_ok = if e2o = None then true else match ty1, ty2 with
							| TInt, TInt -> true
							| TInt32, TInt32 -> true
							| TByte, TByte -> true
							| TVoid, TVoid -> true
							| TRange _, TRange _ -> true
							| TArray(t1, _), TArray(t2, _) when t1 = t2 -> true
							| _, _ -> false in
					if not types_ok then failwith (Printf.sprintf "different types in If: %s and %s" (show_type ty1) (show_type ty2));
					let tmp_vars = List.map (fun _ -> Printf.sprintf "if_res_%d" (uid())) rc2 in
					let store rc =
						let rvs = Array.of_list rc in
						List.mapi (fun i tmp -> Leoc.Assign(ASInt, C.Var tmp, rvs.(i)), loc) tmp_vars in
					let def_tmp_vars = List.map (fun tmp -> Leoc.DefVar tmp, loc) tmp_vars in
					let store1 = store rc1 and store2 = store rc2 in
					let res = List.map (fun tmp -> C.LV(C.Var tmp), loc) tmp_vars in
					ctx, con_code @ def_tmp_vars @ [Leoc.If(ccon, code1 @ store1, code2 @ store2),loc], ty2, res
			| Lambda(name_list, e) -> failwith "not expanded lambda expression"
			| Comp code ->
					(match List.rev code with
						| (Expr e,sl) :: rest_code ->
								let ctx1, code01 = List.fold_left (fun (cx, cd) stmt ->
													let cx1, st1 = compile_stmt cx stmt in cx1, st1:: cd) (ctx, []) (List.rev rest_code) in
								let ctx2, code2, ty, rc = compile_expr ctx1 e in
								let ccode = code01 |> List.rev |> List.map get_code |> List.concat in
								ctx2, ccode @ code2, ty, rc
						| (Ret e,sl) :: rest_code ->
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
		ctx, code, args, { uname = ctx.curr_uname; cbody = Leoc.Ret [], no_source; ftype = TInt }
	else
		(try ctx, code, args, List.assoc typestr fi.compiled with Not_found ->
				(let uname = Printf.sprintf "%s_%d" name (uid()) in
					let ftps0, pars0 = List.map2 (fun par ty ->
										match ty with
										| TVoid -> [ty], []
										| TInt | TInt32 | TByte | TStruct _ -> [ty], [par]
										| TArray(ety, _) -> let lenvar = par ^ "_len" in [TArray(ety, NVar lenvar); TInt], [par; lenvar]
										| TRange _ -> let n2 = par ^ "_end" in [TRange(NVar par, NVar n2); TInt], [par; n2]
							) fi.params types |> List.split in
					let pars = List.concat pars0 and ftps = List.concat ftps0 in
					let ctx0 = { vars = M.empty; currfunc = name; curr_uname = uname } in
					let fctx = List.fold_left2 addvar ctx0 (*fi.params*) pars ftps in
					let _, fcode, fty, frc = compile_expr fctx fi.body in
					let ret_code = if frc = [] then [] else [Leoc.Ret frc, List.hd frc |> snd] in
					let st = Leoc.Defun(uname, pars, fcode @ ret_code), List.hd fcode |> snd in
					let compfun = { uname = uname; cbody = st; ftype = fty } in
					let fi1 = { fi with compiled = (typestr, compfun):: fi.compiled } in
					funs := M.add name fi1 !funs;
					ctx, code, args, compfun))

and rv_of_rc ty rc =
	match ty, rc with TInt, [rv] -> rv | TByte, [rv] -> Leoc.Byte rv, snd rv | _ -> failwith "bad type in condition or arithmetic"

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
