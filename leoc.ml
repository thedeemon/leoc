open ExtLib
open Commons

type name = string
type code = statement list
and statement = 
	| DefVar of name
	| Assign of lvalue * rvalue
	| Assignb of lvalue * rvalue
	| Call of name * rvalue list
	| Defun of name * name list * code
	| Ret of rvalue list
	| If of condition * code * code
	| While of condition * code
	| Print of rvalue
	| Alloc of lvalue * rvalue
	| Comp of code
	| Break
	
and lvalue = Var of name | PVar of name	| LReg of int
and rvalue =
	| Val of int
	| LV of lvalue
	| Arith of oper * rvalue * rvalue
	| FCall of name * rvalue list
	| Byte of rvalue
	| PArith of oper * rvalue * rvalue

and condition = 
	| Less of rvalue * rvalue
	| Eq of rvalue * rvalue
	| And of condition * condition
	| Or of condition * condition
	| Not of condition;;

(*****************************pretty print ***********************************)

let rec show_code n code = 
	code |> List.map (show_stmt n >> tab n) |> String.concat "\n"
	
and show_stmt n = function
	| DefVar name -> Printf.sprintf "var %s" name
	| Assign(lv, rv) -> Printf.sprintf "%s <- %s" (show_lvalue lv) (show_rvalue n rv)
	| Assignb(lv, rv) -> Printf.sprintf "%s <-b- %s" (show_lvalue lv) (show_rvalue n rv)
	| Call(name, rvs) -> Printf.sprintf "%s(%s)" name (rvs |> List.map (show_rvalue n) |> String.concat ", ")  
	| Defun(name, params, code) -> 
			Printf.sprintf "fun %s(%s)\n%s\n%s\n%s" name (params |> String.concat ", ") 
				(tab n "{") (show_code (n+1) code) (tab n "}") 
	| Ret rvs -> Printf.sprintf "return %s" (rvs |> List.map (show_rvalue n) |> String.concat ", ")
	| If(con, code1, code2) ->
			Printf.sprintf "if %s {\n%s\n%s else {\n%s\n%s" (show_cond con) (show_code (n+1) code1) (tab n "}") (show_code (n+1) code2) (tab n "}")
	| While(con, code) -> Printf.sprintf "while %s {\n%s\n%s" (show_cond con) (show_code (n+1) code) (tab n "}")
	| Print rv -> Printf.sprintf "print(%s)" (show_rvalue n rv) 
	| Alloc(lv, rv) -> Printf.sprintf "%s <- new [%s]" (show_lvalue lv) (show_rvalue n rv)
	| Comp code -> Printf.sprintf "{\n%s\n%s" (show_code (n+1) code) (tab n "}")
	| Break -> "break"

and show_lvalue = function
	| Var name -> name 
	| PVar name	-> "*" ^ name
	| LReg r -> Printf.sprintf "$Reg[%d]" r

and show_rvalue n = function
	| LV lv -> show_lvalue lv
	| Val i -> string_of_int i
	| Arith(op, rv1, rv2) -> Printf.sprintf "(%s %s %s)" (show_rvalue n rv1) (show_op op) (show_rvalue n rv2)
	| FCall(name, rvs) -> Printf.sprintf "%s(%s)" name (rvs |> List.map (show_rvalue n) |> String.concat ", ")
	| Byte rv -> Printf.sprintf "byte(%s)" (show_rvalue n rv)
	| PArith(op, rv1, rv2) -> Printf.sprintf "$Mem[%s %s %s]" (show_rvalue n rv1) (show_op op) (show_rvalue n rv2)

and show_cond = function
	| Less(rv1, rv2) -> Printf.sprintf "%s < %s" (show_rvalue 0 rv1) (show_rvalue 0 rv2)
	| Eq(rv1, rv2) -> Printf.sprintf "%s = %s" (show_rvalue 0 rv1) (show_rvalue 0 rv2)
	| And(con1, con2) -> Printf.sprintf "%s && %s" (show_cond con1) (show_cond con2)
	| Or(con1, con2) -> Printf.sprintf "%s || %s" (show_cond con1) (show_cond con2)
	| Not con -> Printf.sprintf "not (%s)" (show_cond con);;

(**************************** subst ************************************)

let rec subst_code subs_map code = 
	List.map (subst_stmt subs_map) code
	
and subst_stmt smap = function
	| DefVar _ as x-> x
	| Assign(lv, rv) -> Assign(subst_lvalue smap lv, subst_rvalue smap rv)
	| Assignb(lv, rv) -> Assignb(subst_lvalue smap lv, subst_rvalue smap rv)
	| Call(name, rvs) -> Call(name, List.map (subst_rvalue smap) rvs)  
	| Defun(name, params, code) -> Defun(name, params, subst_code smap code) 
	| Ret rvs -> Ret(List.map (subst_rvalue smap) rvs)
	| If(con, code1, code2) -> If(subst_cond smap con, subst_code smap code1, subst_code smap code2)			
	| While(con, code) -> While(subst_cond smap con, subst_code smap code)
	| Print rv -> Print (subst_rvalue smap rv) 
	| Alloc(lv, rv) -> Alloc(subst_lvalue smap lv, subst_rvalue smap rv)
	| Comp code -> Comp(subst_code smap code)
	| Break as x -> x

and subst_lvalue smap = function
	| Var name as x -> (try M.find name smap with Not_found -> x) 
	| PVar _ | LReg _  as x -> x

and subst_rvalue smap = function
	| LV lv -> LV (subst_lvalue smap lv) 
	(*| Var name as x -> (try M.find name smap with Not_found -> x)*) 
	| Val i as x -> x
	| Arith(op, rv1, rv2) -> Arith(op, subst_rvalue smap rv1, subst_rvalue smap rv2)
	| FCall(name, rvs) -> FCall(name, List.map (subst_rvalue smap) rvs)
	(*| PVar name as x -> x*)
	| Byte rv -> Byte(subst_rvalue smap rv)
	| PArith(op, rv1, rv2) -> PArith(op, subst_rvalue smap rv1, subst_rvalue smap rv2)

and subst_cond smap = function
	| Less(rv1, rv2) -> Less(subst_rvalue smap rv1, subst_rvalue smap rv2)
	| Eq(rv1, rv2) -> Eq(subst_rvalue smap rv1, subst_rvalue smap rv2)
	| And(con1, con2) -> And(subst_cond smap con1, subst_cond smap con2)
	| Or(con1, con2) -> Or(subst_cond smap con1, subst_cond smap con2)
	| Not con -> Not(subst_cond smap con);;

(**************************** simplify *********************************)

let rec simp_code code = List.map simp_stmt code
	
and simp_stmt = function
	| DefVar _ as x-> x
	| Assign(lv, rv) -> Assign(lv, simp_rvalue rv)
	| Assignb(lv, rv) -> Assignb(lv, simp_rvalue rv)
	| Call(name, rvs) -> Call(name, List.map simp_rvalue rvs)  
	| Defun(name, params, code) -> Defun(name, params, simp_code code) 
	| Ret rvs -> Ret(List.map simp_rvalue rvs)
	| If(con, code1, code2) -> If(simp_cond con, simp_code code1, simp_code code2)			
	| While(con, code) -> While(simp_cond con, simp_code code)
	| Print rv -> Print (simp_rvalue rv) 
	| Alloc(lv, rv) -> Alloc(lv, simp_rvalue rv)
	| Comp code -> Comp(simp_code code)
	| Break as x -> x

and simp_rvalue = function
	| LV _ as x -> x 
	| Val _ as x -> x
	| Arith(Mul, Val a, Val b) -> Val (a * b) 
	| Arith(Add, Val a, Val b) -> Val (a + b) 
	| Arith(Sub, Val a, Val b) -> Val (a - b) 
	| Arith(Div, Val a, Val b) -> Val (a / b) 
	| Arith(Mod, Val a, Val b) -> Val (a mod b) 
	| Arith(Xor, Val a, Val b) -> Val (a lxor b) 
	| Arith(Add, rv1, Val 0) -> rv1 
	| Arith(Add, Val 0, rv2) -> rv2 
	| Arith(Sub, rv1, Val 0) -> rv1 
	| Arith(Mul, rv1, Val 1) -> rv1 
	| Arith(Mul, Val 1, rv2) -> rv2 
	| Arith(Div, rv1, Val 1) -> rv1 
	| Arith(op, rv1, rv2) -> 
			let s1 = simp_rvalue rv1 and s2 = simp_rvalue rv2 in
			if s1 = rv1 && s2 = rv2 then Arith(op, rv1, rv2) else simp_rvalue (Arith(op, s1, s2))			
	| FCall(name, rvs) -> FCall(name, List.map simp_rvalue rvs)
	| Byte rv -> Byte(simp_rvalue rv)
	| PArith(Add, LV(Var v), Val 0) -> LV(PVar v) 
	| PArith(Add, Val 0, LV(Var v)) -> LV(PVar v) 
	| PArith(Sub, LV(Var v), Val 0) -> LV(PVar v)
	| PArith(Mul, LV(Var v), Val 1) -> LV(PVar v) 
	| PArith(Mul, Val 1, LV(Var v)) -> LV(PVar v) 
	| PArith(op, rv1, rv2) -> 
			let s1 = simp_rvalue rv1 and s2 = simp_rvalue rv2 in
			if s1 = rv1 && s2 = rv2 then PArith(op, rv1, rv2) else simp_rvalue (PArith(op, s1, s2))

and simp_cond = function
	| Less(rv1, rv2) -> Less(simp_rvalue rv1, simp_rvalue rv2)
	| Eq(rv1, rv2) -> Eq(simp_rvalue rv1, simp_rvalue rv2)
	| And(con1, con2) -> And(simp_cond con1, simp_cond con2)
	| Or(con1, con2) -> Or(simp_cond con1, simp_cond con2)
	| Not con -> Not(simp_cond con);;

(**************************** compile **********************************)

type src = Tmp of int | TmpPnt of int | Src of Asm.src

module S = Set.Make(struct type t = int let compare = (-) end);;

type func_info = { 
	nparams : int;
	return_size : int;
	params : name list
}

type frame_context = { 
	vars : int M.t;
	regs : S.t;
	funs : func_info M.t;
	thisfun : func_info;
	thisfunname : name
}
	
type context = frame_context list;;

let new_frame = { vars = M.empty; regs = S.empty; funs = M.empty; thisfun = { nparams = 0; return_size = 0; params = []}; thisfunname = "" };;
 
let newregi regs = let rec loop i = if S.mem i regs then loop (i+1) else i in loop 0;;

let usereg r = function
	| f::cxs ->	{f with regs = S.add r f.regs}::cxs
	| [] -> failwith "empty context in usereg";; 

let newreg = function
	| f::cxs ->	let r = newregi f.regs in	{f with regs = S.add r f.regs}::cxs, r
	| [] -> failwith "empty context in newreg";;

let freereg r = function
	| f::cxs -> {f with regs = S.remove r f.regs}::cxs
	| [] -> failwith "empty context in freetmp";;  

let rec addvar ctx name =
	match ctx with 
	| f::ctxs ->	
			if M.mem name f.vars then failwith (Printf.sprintf "variable '%s' already defined." name);
			let r = newregi f.regs in
			{ f with vars = M.add name r f.vars; regs = S.add r f.regs } :: ctxs
	| [] -> addvar [new_frame] name;;

let getvar ctx name =
	match ctx with
	| f::cxs -> (try M.find name f.vars with Not_found -> failwith (Printf.sprintf "variable '%s' not found." name))
	| [] -> failwith "empty context in getvar";; 

let addfun ctx name params retsize = 
	match ctx with
	| f::cxs -> { f with funs = M.add name {nparams = List.length params; return_size = retsize; params = params} f.funs } :: cxs
	| [] -> failwith "empty context in addfun";; 

let getfun ctx name = try M.find name (List.hd ctx).funs with Not_found -> failwith ("function not found: "^name);;

let use_src ctx = function Tmp r -> freereg r ctx, Asm.Reg r | TmpPnt r -> freereg r ctx, Asm.Pnt r | Src s -> ctx, s;;
let strip_src = function Tmp r -> Asm.Reg r | TmpPnt r -> Asm.Pnt r | Src s -> s;;
let strip_dst ctx = function 
	| Var name  -> Asm.RegDest (getvar ctx name)
	| PVar name -> Asm.PntDest (getvar ctx name)
	| LReg r -> Asm.RegDest r;;

let used_params ctx rv =
	let rec gather lst = function
		| LV(Var name) | LV(PVar name) -> if getvar ctx name < 0 then name::lst else lst
		| LV(LReg _) -> failwith "LReg in rvalue"
		| Val _ -> lst
		| Arith(op, rv1, rv2) | PArith(op, rv1, rv2) -> gather (gather lst rv1) rv2  
		| FCall(name, rvals) -> rvals |> List.fold_left gather lst 
		| Byte x -> gather lst x	in
	gather [] rv;;

let rec calc_retsize name code =
	let update sz = function
		| None -> Some sz
		| Some z -> if sz <> z then failwith (Printf.sprintf "different return size in '%s': %d and %d" name z sz) 
								else Some z in 
	List.fold_left (fun szo cmd ->
		match cmd with
		| Ret rvs -> update (List.length rvs) szo
		| DefVar _ 	| Assign _	| Assignb _	| Call _	| Defun _	| Print _ | Alloc _ | Break -> szo
		| If(cond, code1, code2) -> 
				let szo1 = Option.map_default (flip update szo) szo (calc_retsize name code1) in
				Option.map_default (flip update szo1) szo1 (calc_retsize name code2) 
		| While(_, code1) | Comp code1 -> Option.map_default (flip update szo) szo (calc_retsize name code1)
		) None code;;

module T = Triasm;;

let rec compile_code ctx prg =
	let ctx', ccode = List.fold_left (fun (cx, compiled) cmd -> 
		let cx', cs = compile_stmt cx cmd in cx', cs::compiled) (ctx,[]) prg in
	ctx', List.rev ccode |> List.concat
	
and compile prg = 
	try compile_code [new_frame] prg |> snd
	with Failure s -> Printf.printf "LeoC error: %s\n" s; []

and compile_stmt ctx = function
	| DefVar name -> addvar ctx name, []
	| Print rv ->	
			let code, src, _ = compile_rvalue ctx rv in
			ctx, code @ [T.Print (strip_src src)]
	| If(cond, then_code, else_code) -> 
			let _, ccond = compile_cond ctx cond in
			let _, cthen = compile_code ctx then_code in
			let _, celse = compile_code ctx else_code in
			ctx, [Triasm.If(ccond, cthen, celse)] 				 			
	| While(cond, code) ->
			let _, ccond = compile_cond ctx cond in
			let _, ccode = compile_code ctx code in
			ctx, [Triasm.While(ccond, ccode)]
	| Comp code -> let _, ccode = compile_code ctx code in ctx, ccode
	| Assign(lv, rv) ->
			let code, src, ctx1 = compile_rvalue ctx rv in			
			let adst = strip_dst ctx lv in				
			(match List.rev code, src with
			| Triasm.Arith(op, d, a1, a2) :: rest, Tmp r ->
					freereg r ctx1, List.rev (Triasm.Arith(op, adst, a1, a2) :: rest)
			| _, _ -> 
					let ctx2, asrc = use_src ctx1 src in
					ctx2, code @ [T.Mov(adst, asrc)])
	| Assignb(lv, rv) ->
			let code, src, ctx1 = compile_rvalue ctx rv in			
			let adst = strip_dst ctx lv in				
			let ctx2, asrc = use_src ctx1 src in
			ctx2, code @ [T.Movb(adst, asrc)]
	| Defun(name, params, code) ->
			let nparams = List.length params in
			let retsize = Option.default 0 (calc_retsize name code) in
			let frame = List.enum params |> Enum.foldi (fun i par f -> 
				{f with vars = M.add par (i - nparams) f.vars }) new_frame in
			let fi = { nparams = nparams; return_size = retsize; params = params } in
			let ncx = { frame with thisfun = fi; thisfunname = name; funs = M.add name fi frame.funs } :: ctx in
			let ncx1, ccode = compile_code ncx code in			
			let ctx1 = addfun ctx name params retsize in 
			ctx1, [Triasm.Defun(name, ccode)]
	| Ret([FCall(name, rvs)]) when name = (List.hd ctx).thisfunname ->
			let affected_params = List.map (used_params ctx) rvs |> List.concat |> List.unique in
			let fi = getfun ctx name in
			let actions = List.map2 (fun par rv -> if rv = LV(Var par) then None else Some(par, rv)) fi.params rvs 
				|> List.enum |> Enum.filter_map identity |> List.of_enum in
			let phase1, phase2 = List.map (fun (par, rv) ->
				if List.mem par affected_params then
					let tmp = Printf.sprintf "ret_tmp_%d" (uid()) in
					[DefVar tmp; Assign(Var tmp, rv)], Assign(Var par, LV(Var tmp))
				else
					[], Assign(Var par, rv)	) actions |> List.split in			 
			let _, code = compile_code ctx ((List.concat phase1) @ phase2) in 
			ctx, code @ [Triasm.Goto name]			
	| Ret rvs ->
			let affected_params = List.map (used_params ctx) rvs |> List.concat |> List.unique in
			let ctx2, affected_tmps = List.fold_left (fun (cx,lst) _ ->
				let tmp_name = Printf.sprintf "ret_tmp_%d" (uid ()) in  
				let cx' = addvar cx tmp_name in (cx', tmp_name::lst)) (ctx, []) affected_params in
			let tmps = Array.of_list affected_tmps in
			let affected_offs = List.map (getvar ctx) affected_params in
			let fi = (List.hd ctx).thisfun in
			let delta = max fi.return_size fi.nparams in 
			let find_offs i = List.findi (fun _ offs -> offs = i) affected_offs |> fst in
			let phase1, phase2 = List.enum rvs |> Enum.foldi (fun i rv (ph1, ph2) ->
				let dest_offs = i - delta in
				(try 
					let tmp = tmps.(find_offs dest_offs) in
					Assign(Var tmp, rv)::ph1, Assign(LReg dest_offs, LV(Var tmp))::ph2  
				with Not_found -> 
					Assign(LReg dest_offs, rv)::ph1, ph2)) ([],[]) in
			let ctx3, cph1 = compile_code ctx2 (List.rev phase1) in
			let ctx4, cph2 = compile_code ctx3 (List.rev phase2) in
			ctx4, cph1 @ cph2 @ [Triasm.Ret]		
	| Call(name, rvs) -> 
			let code, src, ctx1 = compile_call ctx name rvs in 
			let fi = M.find name (List.hd ctx).funs in
			let r = match src with Src(Asm.Reg x) -> x | _ -> failwith "strange funcall result" in
			let ctx2 = Enum.init fi.return_size ((+) r) |> Enum.fold freereg ctx in
			ctx2, code	
	| Alloc(lv, rv) ->
			let code, src, ctx1 = compile_rvalue ctx rv in			
			let adst = strip_dst ctx lv in				
			let ctx2, asrc = use_src ctx1 src in
			ctx2, code @ [T.New(adst, asrc)]		
	| Break -> ctx, [Triasm.Break]			
	
and compile_call ctx name rvs =
	let fn = getfun ctx name in
	let nrvs = List.length rvs in
	if nrvs <> fn.nparams then 
		failwith (Printf.sprintf "wrong parameters count for '%s': %d and %d" name fn.nparams nrvs);				
	let arg_code, args, ctx2 = List.fold_left (fun (code, srcs, cx) rv ->
		let code', src, cx' = compile_rvalue cx rv in code @ code', src::srcs, cx') ([], [], ctx) rvs in
	let fp = try S.max_elt (List.hd ctx2).regs + 1 with Not_found -> 0 in
	let gap = max fn.return_size fn.nparams in
	let delta = fp + gap - fn.nparams in	
	let pass_args = List.rev args |> List.mapi (fun i src -> T.Mov(Asm.RegDest(i+delta), strip_src src)) in
	let ctx3 = Enum.init fn.return_size (fun i -> i + fp) |> Enum.fold usereg ctx in		
	arg_code @ pass_args @ [T.Call(name, fp + gap)], Src(Asm.Reg fp), ctx3
	
and compile_rvalue ctx = function
	| LV(Var name) -> [], Src(Asm.Reg (getvar ctx name)), ctx
	| Val n -> [], Src(Asm.Val n), ctx
	| LV(PVar name) -> [], Src(Asm.Pnt (getvar ctx name)), ctx
	| LV(LReg _) -> failwith "LReg in rvalue"
	| Arith(op, rv1, rv2) -> 
			let code1, src1, ctx1 = compile_rvalue ctx rv1 in
			let code2, src2, ctx2 = compile_rvalue ctx1 rv2 in
			let ctx3, asrc1 = use_src ctx2 src1 in
			let ctx4, asrc2 = use_src ctx3 src2 in
			let ctx5, r = newreg ctx4 in 
			code1 @ code2 @ [Triasm.Arith(op, Asm.RegDest r, asrc1, asrc2)], Tmp r, ctx5
	| FCall(name, rvals) ->	compile_call ctx name rvals 
	| Byte rv -> 
			let ctx1, r = newreg ctx in
			let code, src, ctx2 = compile_rvalue ctx1 rv in
			let ctx3, asrc = use_src ctx2 src in
			code @ [Triasm.Mov(Asm.RegDest r, Asm.Val 0); Triasm.Movb(Asm.RegDest r, asrc)], Tmp r, ctx3
	| PArith(op, rv1, rv2) ->
		 	let code, src, ctx1 = compile_rvalue ctx (Arith(op, rv1, rv2)) in
			let src1 = match src with Tmp r -> TmpPnt r | _ -> failwith "wrong src type for PArith" in
			code, src1, ctx1			
	
and compile_cond ctx = function
	| Less(rv1, rv2) ->
			let code1, src1, ctx1 = compile_rvalue ctx rv1 in
			let code2, src2, ctx2 = compile_rvalue ctx1 rv2 in
			ctx, T.Less(code1, strip_src src1, code2, strip_src src2)
	| Eq(rv1, rv2) ->
			let code1, src1, ctx1 = compile_rvalue ctx rv1 in
			let code2, src2, ctx2 = compile_rvalue ctx1 rv2 in
			ctx, T.Eq(code1, strip_src src1, code2, strip_src src2)
	| And(con1, con2) ->
			let ctx1, c1 = compile_cond ctx con1 in
			let ctx2, c2 = compile_cond ctx1 con2 in
		 	ctx2, T.And(c1, c2)
	| Or(con1, con2) ->
			let ctx1, c1 = compile_cond ctx con1 in
			let ctx2, c2 = compile_cond ctx1 con2 in
		 	ctx2, T.Or(c1, c2)
	| Not con -> ctx, T.Not(compile_cond ctx con |> snd)

let process prg = prg |> simp_code |> compile  |> Triasm.process;;
(****************************************************************)

let prg = [
	Defun("sumbytes", ["arr"; "len"], [
		DefVar "i";
		DefVar "sum";
		Assign(Var "i", Val 0);
		Assign(Var "sum", Val 0);
		While(Less(LV(Var "i"), LV(Var "len")), [
			Assign(Var "sum", Arith(Add, LV(Var "sum"), Byte( PArith(Add, LV(Var "arr"), LV(Var "i") ) ) ));
			Assign(Var "i", Arith(Add, LV(Var "i"), Val 1))
		]);
		Ret [ LV(Var "sum") ]
	]);	
	DefVar "m";
	Alloc(Var "m", Val 2);
	
	Assignb(PVar "m", Val 33);

	DefVar "p";
	Assign(Var "p", Arith(Add, LV(Var "m"), Val 1));
	Assignb(PVar "p", Val 44);
	
	Print(FCall("sumbytes", [ LV(Var "m"); Val 2 ]));
	(*Defun("sum", [| "x"; "y"; "z" |], [
		Ret [| Arith(Asm.Add, Arith(Asm.Add, Var "x", Var "y"), Var "z") |]
	]);	
	Print(FCall("sum", [| Val 5; Val 55; Val 555 |]));*)
	(*Defun("fib", [|"n"|], [
		If(Less(Var "n", Val 2), 
			[Ret [| Val 1 |]],
			[Ret [| Arith(Asm.Add, 
											FCall("fib",[| Arith(Asm.Sub, Var "n" , Val 1 ) |]), 
											FCall("fib",[| Arith(Asm.Sub, Var "n" , Val 2 ) |])
										) |]
			]
		)	 	
	]);
	Print(FCall("fib", [| Val 10 |]));*)
	(*DefVar "i";
	DefVar "x";
	Assign(Local "i",  Arith(Asm.Sub, Val 20, Arith(Asm.Sub, Val 20, Val 10)));
	While(Less(Var "i", Val 20), [
		(*If(Eq(Val 230, Arith(Asm.Add, Val 5, Arith(Asm.Mul, Var "i", Var "i"))), [Print(Var "i")], []);*) 
		
		Assign(Local "x", Arith(Asm.Mul, Var "i", Val 3));
		If(And(Less(Arith(Asm.Add, Var "x", Val 5), Val 50), Less(Val 300, Arith(Asm.Mul, Val 10, Var "x"))),
		[
			Print(Var "x")
		], []);
		Assign(Local "x", Var "i");
		Assign(Local "i", Arith(Asm.Add, Var "i", Val 1))
	])	*)
];;	
			
(*prg |> show_code 0 |> print_endline;;			
prg |> compile |> Triasm.process;;*)