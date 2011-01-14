open ExtLib
open Commons

type name = string
type code = statement list
and statement = raw_statement * source_loc
and raw_statement = 
  | DefVar of name
  | Assign of arg_size * lvalue * rvalue
  | Call of name * rvalue list
  | Defun of name * name list * code
  | Ret of rvalue list
  | If of condition * code * code
  | While of condition * code
  | Print of rvalue
  | Prchar of rvalue
  | Alloc of lvalue * rvalue
  | Comp of code
  | Break
  | Trash of bool
	| PostMessage of int * rvalue * rvalue
  
and lvalue = Var of name | PVar of name  | LReg of int | Mem of rvalue
and rvalue = raw_rvalue * source_loc
and raw_rvalue =
  | Val of int
  | LV of lvalue
  | Arith of oper * rvalue * rvalue
  | FCall of name * rvalue list
  | Byte of rvalue

and condition = 
  | Less of rvalue * rvalue
  | Eq of rvalue * rvalue
  | And of condition * condition
  | Or of condition * condition
  | Not of condition;;

(*****************************pretty print ***********************************)
let assign_op = function
	| ASByte -> "<-b-" | ASInt -> "<-" | ASInt32 -> "<-d-"

let last_line = ref 0

let rec show_code n code = 
  code |> List.map (show_stmt n >> tab n) |> String.concat "\n" 
	
and show_stmt n (stmt, sl) = 
	let lstmt = if true || sl = !last_line then "" else begin
		last_line := sl;
		Printf.sprintf "//%s\n%s" (prog_source sl) (String.make n ' ')		
	end in
	let cstmt = match stmt with
  | DefVar name -> Printf.sprintf "var %s" name
  | Assign(sz, lv, rv) -> Printf.sprintf "%s %s %s" (show_lvalue lv) (assign_op sz) (show_rvalue rv)
  | Call(name, rvs) -> Printf.sprintf "%s(%s)" name (rvs |> List.map show_rvalue |> String.concat ", ")  
  | Defun(name, params, code) -> 
      Printf.sprintf "fun %s(%s)\n%s\n%s\n%s" name (params |> String.concat ", ") 
        (tab n "{") (show_code (n+1) code) (tab n "}") 
  | Ret rvs -> Printf.sprintf "return %s" (rvs |> List.map show_rvalue |> String.concat ", ")
  | If(con, code1, code2) ->
      Printf.sprintf "if %s {\n%s\n%s else {\n%s\n%s" (show_cond con) (show_code (n+1) code1) (tab n "}") (show_code (n+1) code2) (tab n "}")
  | While(con, code) -> Printf.sprintf "while %s {\n%s\n%s" (show_cond con) (show_code (n+1) code) (tab n "}")
  | Print rv -> Printf.sprintf "print(%s)" (show_rvalue rv) 
  | Prchar rv -> Printf.sprintf "prchar(%s)" (show_rvalue rv) 
  | Alloc(lv, rv) -> Printf.sprintf "%s <- new [%s]" (show_lvalue lv) (show_rvalue rv)
  | Comp code -> Printf.sprintf "{\n%s\n%s" (show_code (n+1) code) (tab n "}")
  | Break -> "break"
  | Trash on -> if on then "$trash" else "$notrash"
	| PostMessage(msg, rv1, rv2) -> Printf.sprintf "PostMessage(%d, %s, %s)" msg (show_rvalue rv1) (show_rvalue rv2)
	in lstmt ^ cstmt

and show_lvalue = function
  | Var name -> name 
  | PVar name  -> "*" ^ name
  | LReg r -> Printf.sprintf "$Reg[%d]" r
	| Mem rv -> Printf.sprintf "$Mem[%s]" (show_rvalue rv)
  (*| PArith(op, lv1, rv2) -> Printf.sprintf "$Mem[%s %s %s]" (show_lvalue lv1) (show_op op) (show_rvalue rv2)*)

and show_rvalue (rv, sl) = match rv with
  | LV lv -> show_lvalue lv
  | Val i -> string_of_int i
  | Arith(op, rv1, rv2) -> Printf.sprintf "(%s %s %s)" (show_rvalue rv1) (show_op op) (show_rvalue rv2)
  | FCall(name, rvs) -> Printf.sprintf "%s(%s)" name (rvs |> List.map show_rvalue |> String.concat ", ")
  | Byte rv -> Printf.sprintf "byte(%s)" (show_rvalue rv)  

and show_cond = function
  | Less(rv1, rv2) -> Printf.sprintf "%s < %s" (show_rvalue rv1) (show_rvalue rv2)
  | Eq(rv1, rv2) -> Printf.sprintf "%s = %s" (show_rvalue rv1) (show_rvalue rv2)
  | And(con1, con2) -> Printf.sprintf "%s && %s" (show_cond con1) (show_cond con2)
  | Or(con1, con2) -> Printf.sprintf "%s || %s" (show_cond con1) (show_cond con2)
  | Not con -> Printf.sprintf "not (%s)" (show_cond con);;

(**************************** map **************************************)
class mapper = 
  object(self)
    method map_code code = List.map self#map_stmt code
		
    method map_stmt (st,sl) = self#map_raw_stmt st, sl
    method map_raw_stmt = function
      | Break | Trash _ | DefVar _ as x -> x
      | Assign(sz, lv, rv) -> Assign(sz, self#map_lvalue lv, self#map_rvalue rv)
      | Call(name, rvs) -> Call(name, List.map self#map_rvalue rvs)  
      | Defun(name, params, code) -> Defun(name, params, self#map_code code) 
      | Ret rvs -> Ret(List.map self#map_rvalue rvs)
      | If(con, code1, code2) -> If(self#map_cond con, self#map_code code1, self#map_code code2)      
      | While(con, code) -> While(self#map_cond con, self#map_code code)
      | Print rv -> Print (self#map_rvalue rv) 
      | Prchar rv -> Prchar (self#map_rvalue rv) 
      | Alloc(lv, rv) -> Alloc(self#map_lvalue lv, self#map_rvalue rv)
      | Comp code -> Comp(self#map_code code)
			| PostMessage(msg, rv1, rv2) -> PostMessage(msg, self#map_rvalue rv1, self#map_rvalue rv2)
      
    method map_lvalue = function
      | Var _ | PVar _ | LReg _  as x -> x
			| Mem rv -> Mem(self#map_rvalue rv)

		method map_rvalue (rv, sl) = self#map_raw_rvalue rv, sl 
    method map_raw_rvalue = function
      | LV lv -> LV (self#map_lvalue lv) 
      | Val _ as x -> x
      | Arith(op, rv1, rv2) -> Arith(op, self#map_rvalue rv1, self#map_rvalue rv2)
      | FCall(name, rvs) -> FCall(name, List.map self#map_rvalue rvs)
      | Byte rv -> Byte(self#map_rvalue rv)

    method map_cond = function
      | Less(rv1, rv2) -> Less(self#map_rvalue rv1, self#map_rvalue rv2)
      | Eq(rv1, rv2) -> Eq(self#map_rvalue rv1, self#map_rvalue rv2)
      | And(con1, con2) -> And(self#map_cond con1, self#map_cond con2)
      | Or(con1, con2) -> Or(self#map_cond con1, self#map_cond con2)
      | Not con -> Not(self#map_cond con) 
  end

(**************************** subst ************************************)

let subst_code_by_lvalue smap code =
  let o = object
      inherit mapper as super
      method map_lvalue = function
        | Var name as x -> (try M.find name smap with Not_found -> x)
        | lv -> super#map_lvalue lv
    end 
  in o#map_code code;;

let subst_code_by_stmt f code = 
  let o = object
      inherit mapper as super
      method map_raw_stmt st = 
        match f st with Some st' -> st' | None -> super#map_raw_stmt st
    end 
  in o#map_code code;;    
    
(**************************** simplify *********************************)

class simpler = object(self)
		inherit mapper as super
		method map_raw_rvalue = function
  		| Arith(Mul, (Val a, _), (Val b, _)) -> Val (a * b) 
  		| Arith(Add, (Val a, _), (Val b, _)) -> Val (a + b) 
  		| Arith(Sub, (Val a, _), (Val b, _)) -> Val (a - b) 
  		| Arith(Div, (Val a, _), (Val b, _)) -> Val (a / b) 
  		| Arith(Mod, (Val a, _), (Val b, _)) -> Val (a mod b) 
  		| Arith(Xor, (Val a, _), (Val b, _)) -> Val (a lxor b) 
  		| Arith(Add, (rv1,_), (Val 0, _)) -> self#map_raw_rvalue rv1 
  		| Arith(Add, (Val 0,_), (rv2,_)) -> self#map_raw_rvalue rv2 
  		| Arith(Sub, (rv1,_), (Val 0, _)) -> self#map_raw_rvalue rv1 
  		| Arith(Mul, (rv1,_), (Val 1, _)) -> self#map_raw_rvalue rv1 
  		| Arith(Mul, (Val 1,_), (rv2,_)) -> self#map_raw_rvalue rv2 
  		| Arith(Div, (rv1,_), (Val 1,_)) -> self#map_raw_rvalue rv1 
  		| Arith(op, rv1, rv2) ->  
		      let s1 = self#map_rvalue rv1 and s2 = self#map_rvalue rv2 in
      		if s1 = rv1 && s2 = rv2 then Arith(op, rv1, rv2) else self#map_raw_rvalue (Arith(op, s1, s2))
			| rv -> super#map_raw_rvalue rv		      

		method map_lvalue = function
			| Mem(LV(Var v),_)  -> PVar v
			| lv -> super#map_lvalue lv
end

let simp_code code = 
	let simp = new simpler in simp#map_code code

let simp_rvalue rv =
	let simp = new simpler in simp#map_rvalue rv

(******************** register allocation ******************************)

module RA = struct
	type t = { free_land : int; free_list : int list }
	
	let newreg rs = match rs.free_list with 
		| [] -> rs.free_land, { rs with free_land = rs.free_land + 1 }
		| r::tl -> r, { rs with free_list = tl } 
	
	let freereg r rs =
		if r = rs.free_land - 1 then { rs with free_land = rs.free_land - 1 }
		else { rs with free_list = r::rs.free_list }
		
	let mark_as_used r rs = 
		if r >= rs.free_land then { rs with free_land = r + 1 }
		else { rs with free_list = List.remove rs.free_list r }	
		
	let empty = { free_land = 0; free_list = [] }
	
	let get_free_land rs = rs.free_land
end

(**************************** compile **********************************)

(*type src = Tmp of int | TmpPnt of int | Src of Asm.src
type dst = TmpPntDest of int | Dst of Asm.dst*)
type src = Asm.src
type dst = Asm.dst

module S = Set.Make(struct type t = int let compare = (-) end);;

type func_info = { 
  nparams : int;
  return_size : int;
  params : name list
}

type frame_context = { 
  vars : int M.t;
  regs : RA.t;
  funs : func_info M.t;
  thisfun : func_info;
  thisfunname : name
}
  
type context = frame_context list;;

let new_frame = { vars = M.empty; regs = RA.empty; funs = M.empty; thisfun = { nparams = 0; return_size = 0; params = []}; thisfunname = "" };;
 
(*let newregi regs = let rec loop i = if S.mem i regs then loop (i+1) else i in loop 0;;*)

let usereg r = function
  | f::cxs ->  {f with regs = RA.mark_as_used r f.regs}::cxs
  | [] -> failwith "empty context in usereg";; 

let newreg = function
  | f::cxs -> let r, rs = RA.newreg f.regs in {f with regs = rs}::cxs, r
  | [] -> failwith "empty context in newreg";;

(*let newreg x = Prof.prof1 "newreg" newreg_ x*)

let freereg r = function
  | f::cxs -> {f with regs = RA.freereg r f.regs}::cxs
  | [] -> failwith "empty context in freetmp";;  

let rec addvar ctx name =
  match ctx with 
  | f::ctxs ->  
      if M.mem name f.vars then failwith (Printf.sprintf "variable '%s' already defined." name);
      let r, rs = RA.newreg f.regs in
      { f with vars = M.add name r f.vars; regs = rs } :: ctxs
  | [] -> addvar [new_frame] name

let getvar ctx name =
  match ctx with
  | f::cxs -> (try M.find name f.vars with Not_found -> failwith (Printf.sprintf "variable '%s' not found." name))
  | [] -> failwith "empty context in getvar" 

(*let getvar ctx name = Prof.prof2 "getvar" getvar_ ctx name*)


let addfun ctx name params retsize = 
  match ctx with
  | f::cxs -> { f with funs = M.add name {nparams = List.length params; return_size = retsize; params = params} f.funs } :: cxs
  | [] -> failwith "empty context in addfun" 

let getfun ctx name = try M.find name (List.hd ctx).funs with Not_found -> failwith ("function not found: "^name)

let use_src ctx = function 
	| Asm.TmpReg r -> freereg r ctx, Asm.Reg r 
	| Asm.TmpPnt r -> freereg r ctx, Asm.Pnt r 
	| s -> ctx, s

let strip_src = function Asm.TmpReg r -> Asm.Reg r | Asm.TmpPnt r -> Asm.Pnt r | s -> s

let use_dst ctx = function Asm.TmpPntDest r -> freereg r ctx, Asm.PntDest r | d -> ctx, d
let strip_dst = function Asm.TmpPntDest r -> Asm.RegDest r |  d -> d

let used_params ctx rv =
  let rec gather_rv lst (rval,_) = match rval with
    | LV lv -> gather_lv lst lv
    | Val _ -> lst
    | Arith(op, rv1, rv2) -> gather_rv (gather_rv lst rv1) rv2       
    | FCall(name, rvals) -> rvals |> List.fold_left gather_rv lst 
    | Byte x -> gather_rv lst x  
  and gather_lv lst = function
    | Var name | PVar name -> if getvar ctx name < 0 then name::lst else lst
    (*| PArith(op, lv1, rv2) -> gather_rv (gather_lv lst lv1) rv2*)
		| Mem rv -> gather_rv lst rv
    | LReg _ -> lst in
  gather_rv [] rv;;

let rec calc_retsize name code =
  let update sz = function
    | None -> Some sz
    | Some z -> if sz <> z then failwith (Printf.sprintf "different return size in '%s': %d and %d" name z sz) 
                else Some z in 
  List.fold_left (fun szo (cmd,_) ->
    match cmd with
    | Ret rvs -> update (List.length rvs) szo
    | DefVar _   | Assign _ | Call _  | Defun _  | Print _ | Prchar _ | Alloc _ | Break 
		| Trash _ | PostMessage _ -> szo
    | If(cond, code1, code2) -> 
        let szo1 = Option.map_default (flip update szo) szo (calc_retsize name code1) in
        Option.map_default (flip update szo1) szo1 (calc_retsize name code2) 
    | While(_, code1) | Comp code1 -> Option.map_default (flip update szo) szo (calc_retsize name code1)
    ) None code;;

module T = Triasm;;

let lock_stmts = ref false
let lock_rv = ref false
let lock_lv = ref false
let n_compile_stmt = ref 0

let rec compile_code ctx prg =
  let ctx', ccode = (*Prof.rprof3 "compile_code: stmts" lock_stmts*) List.fold_left (fun (cx, compiled) cmd -> 
    let cx', cs = compile_stmt cx cmd in cx', cs::compiled) (ctx,[]) prg in
  ctx', List.rev ccode |> List.concat
  
and compile prg = 
  let frm = { new_frame with vars = M.add "args" 0 new_frame.vars; regs = RA.mark_as_used 0 new_frame.regs } in
  try compile_code [frm] prg |> snd
  with Failure s -> Printf.printf "LeoC error: %s\n" s; []

and compile_stmt ctx (stmt, sl) = 
	incr n_compile_stmt;
	match stmt with
  | DefVar name -> addvar ctx name, []
  | Print rv ->  
      let code, src, _ = compile_rvalue ctx rv in
      ctx, code @ [T.Print (strip_src src),sl]
  | Prchar rv ->  
      let code, src, _ = compile_rvalue ctx rv in
      ctx, code @ [T.Prchar (strip_src src),sl]
	| PostMessage(msg, rv1, rv2) ->
      let code1, src1, _ = compile_rvalue ctx rv1 in
      let code2, src2, _ = compile_rvalue ctx rv2 in
      ctx, code1 @ code2 @ [T.PostMessage(msg, strip_src src1, strip_src src2),sl]				
  | If(cond, then_code, else_code) -> 
      let _, ccond = compile_cond ctx cond in
      let _, cthen = compile_code ctx then_code in
      let _, celse = compile_code ctx else_code in
      ctx, [Triasm.If(ccond, cthen, celse),sl]                
  | While(cond, code) ->
      let _, ccond = compile_cond ctx cond in
      let _, ccode = compile_code ctx code in
      ctx, [Triasm.While(ccond, ccode),sl]
  | Comp code -> let _, ccode = compile_code ctx code in ctx, ccode
  | Assign(sz, lv, rv) ->
      let code1, dst, ctx1 = compile_lvalue ctx lv in
      let code2, src, ctx2 = compile_rvalue ctx1 rv in      
      let ctx3, adst = use_dst ctx2 dst in    
			(match sz, List.rev code2, src with
      | ASInt, (Triasm.Arith(op, d, a1, a2),sl) :: rest, Asm.TmpReg r ->        
         	freereg r ctx3, code1 @ (List.rev ((Triasm.Arith(op, adst, a1, a2),sl) :: rest))
      | _, _, _ -> 
         	let ctx4, asrc = use_src ctx3 src in
         	ctx4, code1 @ code2 @ [T.Mov(sz, adst, src),sl])
  | Defun(name, params, code) ->
      let nparams = List.length params in
      let retsize = Option.default 0 (calc_retsize name code) in
      let frame = List.enum params |> Enum.foldi (fun i par f -> 
        {f with vars = M.add par (i - nparams) f.vars }) new_frame in
      let fi = { nparams = nparams; return_size = retsize; params = params } in
      let ncx = { frame with thisfun = fi; thisfunname = name; funs = M.add name fi frame.funs } :: ctx in
      let ncx1, ccode = compile_code ncx code in      
      let ctx1 = addfun ctx name params retsize in 
      ctx1, [Triasm.Defun(name, ccode),sl]
  | Ret([FCall(name, rvs),sl]) when name = (List.hd ctx).thisfunname ->
      let affected_params = List.map (used_params ctx) rvs |> List.concat |> List.unique in
      let fi = getfun ctx name in
      let actions = List.map2 (fun par (rv,loc) -> if rv = LV(Var par) then None else Some(par, (rv,loc))) fi.params rvs 
        |> List.enum |> Enum.filter_map identity |> List.of_enum in
      let phase1, phase2 = List.map (fun (par, rv) ->
        if List.mem par affected_params then
          let tmp = Printf.sprintf "ret_tmp_%d" (uid()) in
          [DefVar tmp, sl; Assign(ASInt, Var tmp, rv), sl], (Assign(ASInt, Var par, (LV(Var tmp),sl)), sl)
        else
          [], (Assign(ASInt, Var par, rv),sl)  ) actions |> List.split in       
      let _, code = compile_code ctx ((List.concat phase1) @ phase2) in 
      ctx, code @ [Triasm.Goto name,sl]      
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
				let sl = snd rv in
        let dest_offs = i - delta in
        (try 
          let tmp = tmps.(find_offs dest_offs) in
          (Assign(ASInt, Var tmp, rv),sl)::ph1, (Assign(ASInt, LReg dest_offs, (LV(Var tmp),sl)),sl)::ph2  
        with Not_found -> 
          (Assign(ASInt, LReg dest_offs, rv),sl)::ph1, ph2)) ([],[]) in
      let ctx3, cph1 = compile_code ctx2 (List.rev phase1) in
      let ctx4, cph2 = compile_code ctx3 (List.rev phase2) in
      ctx4, cph1 @ cph2 @ [Triasm.Ret,sl]    
  | Call(name, rvs) -> 
      let code, src, ctx1 = compile_call ctx name rvs sl in 
      let fi = M.find name (List.hd ctx).funs in
      let r = match src with Asm.Reg x -> x | _ -> failc "strange funcall result" sl in
      let ctx2 = Enum.init fi.return_size ((+) r) |> Enum.fold freereg ctx in
      ctx2, code  
  | Alloc(lv, rv) ->
      let code1, dst, ctx1 = compile_lvalue ctx lv in
      let code2, src, ctx2 = compile_rvalue ctx1 rv in      
      let ctx3, adst = use_dst ctx2 dst in        
      let ctx4, asrc = use_src ctx3 src in
      ctx4, code1 @ code2 @ [T.New(adst, src),sl]    
  | Break -> ctx, [Triasm.Break, sl]  
  | Trash _ -> ctx, []    
  
and compile_call ctx name rvs sl =
  let fn = getfun ctx name in
  let nrvs = List.length rvs in
  if nrvs <> fn.nparams then 
    failc (Printf.sprintf "wrong parameters count for '%s': %d and %d" name fn.nparams nrvs) sl;        
  let arg_code, args, ctx2 = List.fold_left (fun (code, srcs, cx) rv ->
    let code', src, cx' = compile_rvalue cx rv in code @ code', src::srcs, cx') ([], [], ctx) rvs in
  (*let fp = try S.max_elt (List.hd ctx2).regs + 1 with Not_found -> 0 in*)
	let fp = RA.get_free_land (List.hd ctx2).regs in
  let gap = max fn.return_size fn.nparams in
  let delta = fp + gap - fn.nparams in  
  let pass_args = List.rev args |> List.mapi (fun i src -> T.Mov(ASInt, Asm.RegDest(i+delta), strip_src src),sl) in
  let ctx3 = Enum.init fn.return_size (fun i -> i + fp) |> Enum.fold usereg ctx in    
  arg_code @ pass_args @ [T.Call(name, fp + gap), sl], Asm.Reg fp, ctx3

(*and compile_rvalue ctx x = Prof.rprof2 "compile_rvalue" lock_rv compile_rvalue_ ctx x*)
  
and compile_rvalue ctx (rval,sl) = match rval with
  | LV(Var name) -> [], Asm.Reg (getvar ctx name), ctx
  | LV(PVar name) -> [], Asm.Pnt (getvar ctx name), ctx
  | LV(LReg _) -> failc "LReg in rvalue" sl
	| LV(Mem rv) -> 
			let code, src, ctx1 = compile_rvalue ctx rv in
			let src1 = match src with 
				| Asm.TmpReg r -> Asm.TmpPnt r
				| Asm.Reg r -> Asm.Pnt r  
				| _ -> failc "wrong src type for Mem" sl in
      code, src1, ctx1
  | Val n -> [], Asm.Val n, ctx
  | Arith(op, rv1, rv2) -> 
      let code1, src1, ctx1 = compile_rvalue ctx rv1 in
      let code2, src2, ctx2 = compile_rvalue ctx1 rv2 in
      let ctx3, asrc1 = use_src ctx2 src1 in
      let ctx4, asrc2 = use_src ctx3 src2 in
      let ctx5, r = newreg ctx4 in 
      code1 @ code2 @ [Triasm.Arith(op, Asm.RegDest r, src1, src2), sl], Asm.TmpReg r, ctx5
  | FCall(name, rvals) ->  compile_call ctx name rvals sl
  | Byte rv -> 
      let ctx1, r = newreg ctx in
      let code, src, ctx2 = compile_rvalue ctx1 rv in
      let ctx3, asrc = use_src ctx2 src in
      code @ [Triasm.Mov(ASInt, Asm.RegDest r, Asm.Val 0), sl; Triasm.Mov(ASByte, Asm.RegDest r, asrc), sl], Asm.TmpReg r, ctx3

and compile_lvalue ctx = function 
  | Var name  -> [], Asm.RegDest (getvar ctx name), ctx
  | PVar name -> [], Asm.PntDest (getvar ctx name), ctx
  | LReg r -> [], Asm.RegDest r, ctx
	|	Mem rv -> 
			let code,src,ctx1 = compile_rvalue ctx rv in
			let dst = match src with 
				| Asm.TmpReg r -> Asm.TmpPntDest r
				| Asm.Reg r -> Asm.PntDest r 
				| _ -> failc "wrong src type for Mem" (snd rv) in
      code, dst, ctx1
  
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

(****************************************************************)

(*let prg = [
  Defun("sumbytes", ["arr"; "len"], [
    DefVar "i";
    DefVar "sum";
    Assign(Var "i", Val 0);
    Assign(Var "sum", Val 0);
    While(Less(LV(Var "i"), LV(Var "len")), [
      Assign(Var "sum", Arith(Add, LV(Var "sum"), Byte( LV(PArith(Add, Var "arr", LV(Var "i") ) )) ));
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
  ])  *)
];;  
      *)
(*prg |> show_code 0 |> print_endline;;      
prg |> compile |> Triasm.process;;*)