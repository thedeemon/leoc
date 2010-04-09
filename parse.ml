open ExtLib
open Commons
open Parsercomb
open Tokens

type ttoken = 
	TT of token 
	* (Leo.expr, ttoken) Parsercomb.parse_result Lazy.t (* simple_expr *)
	* (Leo.expr, ttoken) Parsercomb.parse_result Lazy.t (* expr*)
	* (Leo.statement, ttoken) Parsercomb.parse_result Lazy.t (* stmt *)
	* (Leo.lvalue, ttoken) Parsercomb.parse_result Lazy.t (* lvalue *)

let get0 = function TT(v0,v1,v2,v3,v4) -> v0
let get1 = function TT(v0,v1,v2,v3,v4) -> v1
let get2 = function TT(v0,v1,v2,v3,v4) -> v2
let get3 = function TT(v0,v1,v2,v3,v4) -> v3
let get4 = function TT(v0,v1,v2,v3,v4) -> v4

let tok tk s = 
	match s with
	| h::tl when tk = get0 h -> log (show_tok tk); Parsed(tk, tl) 
	| _ -> Failed
	 	
let eol = tok Leol ||| (tok Lrem >>> tok Leol)	
let term = tok Lsemi ||| eol	
let terms = p_plus term
let opt_terms = p_opt () terms
let opt_eols = p_many eol
let atype = (tok Lbyte >>> return Leo.AByte) ||| (tok Ltint >>> return Leo.AInt) 
let then_ = tok Lthen ||| eol
let else_ = opt_eols >>> tok Lelse >>> opt_eols
let ident = function    
	| TT(Lident name, _,_,_,_) :: ts  -> log name; Parsed(name, ts)
	| _ -> Failed 

let int_ = function    
	| TT(Lint i, _,_,_,_) :: ts -> log (string_of_int i); Parsed(i, ts)
	| _ -> Failed
 			
let params = tok Llparen >>> p_list0 ident (tok Lcomma) >>= fun ps -> tok Lrparen >>> return ps
let opt_params = p_opt [] params 			

let rec (prepare : Tokens.token list -> ttoken list) = fun tok_list ->
	List.fold_right (fun tk res ->
		let rec v = TT(tk, lazy(simple_expr_r vlst), lazy(expr_r vlst), lazy(stmt_r vlst), lazy(lvalue_r vlst)) 
		and vlst = v::res in
		vlst) tok_list []   			
		
and simple_expr = function h::tl -> Lazy.force (get1 h)	| [] -> Failed	
					
and simple_expr_r s = ( 
	log "simple_expr"; (
	    (tok Lpipe >>> expr >>= fun e -> tok Lpipe >>> return (Leo.Length e))
	||| (tok Ldo >>> opt_terms >>> stmts >>= fun code -> opt_terms >>> tok Lend >>> return (Leo.Comp code))	
	||| (tok Llcurly >>> opt_terms >>> stmts >>= fun code -> opt_terms >>> tok Lrcurly >>> return (Leo.Comp code))
	||| (tok Lif >>> condition >>= fun con -> then_ >>> expr >>= fun e1 -> 
					p_opt None (else_ >>> expr >>= fun e2 -> return (Some e2)) >>= fun e2o -> return (Leo.If(con, e1, e2o)) )
	||| (tok Lnew >>> atype >>= fun ty -> tok Llbracket >>> p_list expr (tok Lcomma) >>= fun es -> tok Lrbracket >>> return (Leo.New(ty, es)))
	||| (tok Lbackslash >>> p_list ident (tok Lcomma) >>= fun ps -> tok Lfollow >>> expr >>= fun e -> return(Leo.Lambda(ps, e)))
	||| (ident >>= fun name -> tok Llparen >>> args >>= fun es -> tok Lrparen >>> 
				return (if name="print" then Leo.Comp [Leo.Print(List.hd es)] else Leo.Call(name, es)))	
	||| (lvalue >>= fun lv -> return (Leo.LV lv))
	||| (int_ >>= fun i -> return (Leo.Val i))
	||| (tok Llparen >>> expr >>= fun e -> tok Lrparen >>> return e)
	)) s			
	
and expr = function h::tl -> Lazy.force (get2 h) | [] -> Failed	
	
and expr_r s = (
	log "expr";
	product >>= fun e1 ->	p_seq0 
		((tok Lplus >>> product >>= fun e2 -> return (Add, e2)) ||| 
		 (tok Lminus >>> product >>= fun e2 -> return (Sub, e2))) 
	  >>= fun ps -> return (List.fold_left (fun e1 (op, e2) -> Leo.Arith(op, e1, e2)) e1 ps)
	) s

and product s = (
	log "product";
	value >>= fun e1 -> p_seq0
		((tok Lmul >>> value >>= fun e2 -> return (Mul, e2))|||
		 (tok Ldiv >>> value >>= fun e2 -> return (Div, e2))|||
     (tok Lmod >>> value >>= fun e2 -> return (Mod, e2))|||		
     (tok Lxor >>> value >>= fun e2 -> return (Xor, e2)))		
	  >>= fun ps -> return (List.fold_left (fun e1 (op, e2) -> Leo.Arith(op, e1, e2)) e1 ps)
	) s
			
and value s = (
	log "value";
			(simple_expr >>= fun e -> tok Lhead >>> return (Leo.Head(e)))
	|||	(simple_expr >>= fun e -> tok Ltail >>> return (Leo.Tail(e)))				
	||| simple_expr				
	) s
												
and stmts s = 	log "stmts"; p_list stmt terms s

and stmt = function h::tl -> Lazy.force (get3 h) | [] -> Failed
and stmt_r s = (
	log "stmt";
	(tok Lfor >>> p_list name_seq (tok Lcomma) >>= fun nsl -> opt_terms >>> stmts >>= fun code ->
		 opt_terms >>> tok Lend >>> return (Leo.For(nsl, code)))
	||| (tok Ltype >>> ident >>= fun tname -> tok Leq >>> tok Llcurly >>> opt_terms >>>
				p_list field_def terms >>= fun flds -> opt_terms >>> tok Lrcurly >>>
					return (Leo.Typedef(tname, flds))	)
	||| (tok Ltrash >>> return (Leo.Trash true)) 
	||| (tok Lnotrash >>> return (Leo.Trash false))
	|||	(tok Lwhile >>> condition >>= fun con -> opt_terms >>> stmts >>= fun code ->
		    opt_terms >>> tok Lend >>> return (Leo.While(con, code))) 
	||| (ident >>= fun name -> opt_params >>= fun ps -> tok Leq >>> opt_terms >>>
				expr >>= fun e1 -> tok Ldot2 >>> expr >>= fun e2 -> return (Leo.Def(name, ps, Leo.Range(e1,e2))))
	||| (ident >>= fun name -> opt_params >>= fun ps -> tok Leq >>> opt_terms >>>
				(expr ||| (stmt >>= fun st -> return (Leo.Comp[st]))) >>= fun e -> return (Leo.Def(name, ps, e)))
	||| (ident >>= fun vname -> tok Lcolon >>> ident >>= fun tname -> return (Leo.Typing(vname, tname)))
	||| (lvalue >>= fun lv -> tok Lwrite >>> expr >>= fun e1 -> tok Ldot2 >>> expr >>= fun e2 ->  
				return (Leo.Write(lv, Leo.Range(e1,e2))))
	||| (lvalue >>= fun lv -> tok Lwrite >>> expr >>= fun e -> return (Leo.Write(lv, e)))
	||| (expr >>= fun e -> return (Leo.Expr e))
	) s			
			
and lvalue = function h::tl -> Lazy.force (get4 h) | [] -> Failed		
and lvalue_r s = (
	log "lvalue";
	    (path >>= fun p -> tok Llbracket >>> expr >>= fun e -> tok Lrbracket >>> return (Leo.ArrElt(p, e)))
	||| (path >>= fun p -> tok Llbracket >>> seq >>= fun sq -> tok Lrbracket >>> return (Leo.ArrElt(p, sq)))
	||| (path >>= fun p -> return (Leo.Var p))	
	) s			
			
and path = p_list ident (tok Ldot) >>= fun ns -> return (List.hd ns, List.tl ns) 		
			
and seq s = (
	log "seq";
	(expr >>= fun e1 -> tok Ldot2 >>> expr >>= fun e2 -> return (Leo.Range(e1, e2)))
	||| (expr >>= fun e1 -> tok Lrange >>> return (Leo.Range(Leo.Val 0, Leo.Arith(Sub, Leo.Length e1, Leo.Val 1))))
	||| (path >>= fun p -> return (Leo.LV(Leo.Var p)))
	) s			
	
and range s =
	log "range";
	(expr >>= fun e1 -> tok Ldot2 >>> expr >>= fun e2 -> return (Leo.Range(e1, e2))) s 
	
and name_seq s =
	log "name_seq";
	(ident >>= fun name -> tok Lin >>> seq >>= fun sq -> return (name, sq)) s	
	
and args s = 	log "args"; p_list0 (range ||| expr) (tok Lcomma) s

and condition s = (
	log "condition";
	(conjunction >>= fun c1 -> tok Lor >>> condition >>= fun c2 -> return (Leo.Or(c1, c2))) ||| conjunction
	) s
			
and conjunction s = (
	log "conjunction";
	(comparison >>= fun c1 -> tok Land >>> conjunction >>= fun c2 -> return (Leo.And(c1, c2))) ||| comparison
	) s			
		
and comparison s = (
	log "comparison";
	    (expr >>= fun e1 -> tok Llt >>> expr >>= fun e2 -> return (Leo.Less(e1, e2)))
	||| (expr >>= fun e1 -> tok Lgt >>> expr >>= fun e2 -> return (Leo.Less(e2, e1)))
	||| (expr >>= fun e1 -> tok Leq >>> expr >>= fun e2 -> return (Leo.Eq(e1, e2)))
	||| (expr >>= fun e1 -> tok Lneq >>> expr >>= fun e2 -> return (Leo.Not(Leo.Eq(e1, e2))))
	||| (expr >>= fun e1 -> tok Lgeq >>> expr >>= fun e2 -> return (Leo.Not(Leo.Less(e1, e2))))
	||| (expr >>= fun e1 -> tok Lleq >>> expr >>= fun e2 -> return (Leo.Not(Leo.Less(e2, e1))))
	||| (tok Llparen >>> condition >>= fun c -> tok Lrparen >>> return c)
	) s		
	
and field_def s = (
		ident >>= fun name -> tok Lcolon >>> 
		 ((atype >>= fun aty -> tok Llbracket >>> 
		   ((int_ >>= fun i -> return (Leo.NVal i)) ||| (ident >>= fun nm -> return (Leo.NVar nm))) >>= fun n ->
			tok Lrbracket >>> return (Leo.FArray(aty, n)))
		 ||| (tok Ltint >>> return Leo.FInt)
		 ||| (ident >>= fun nm -> return (Leo.FStruct nm)))
		 >>= fun ftype -> return (name, ftype)   
	) s
					
let parse_program prg = prepare prg |> stmts 
	

