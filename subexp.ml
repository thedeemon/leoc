open Commons
open Leoc

(****************************** canonicalization ******************************)

type svalue = SVar of name | SPVar of name  | SLReg of int | SMem of svalue 
  | SVal of int
  | SArith of oper * svalue * svalue
  | SFCall of name * svalue list
  | SByte of svalue

let rec sv_of_rv (rval, sl) = match rval with
	| Val i -> SVal i
  | LV lv -> sv_of_lv lv
  | Arith(Sub, rv1, (Val n,_)) -> SArith(Add, sv_of_rv rv1, SVal(0-n))
  | Arith(op, rv1, rv2) -> SArith(op, sv_of_rv rv1, sv_of_rv rv2)
  | FCall(name, rvlist) -> SFCall(name, List.map sv_of_rv rvlist)
  | Byte rv -> SByte(sv_of_rv rv) 

and sv_of_lv = function
	| Var name -> SVar name 
	| PVar name -> SPVar name  
	| LReg i -> SLReg i
	| Mem rv -> SMem (sv_of_rv rv)

let commute op1 op2 = op1=op2 && (op1=Add || op1=Mul || op1=Xor) 

let (@@) = Array.append;;
let tail a = Array.sub a 1 (Array.length a - 1)
let ari_fold op a = Array.fold_left (fun sv elt -> SArith(op, sv, elt)) a.(0) (tail a)

type ('elt, 'marker) mtree = Seq of 'marker * ('elt, 'marker) mtree array | Elt of 'elt

let rec linearize x = match x with
	| SVar _ 	| SPVar _ 	| SLReg _   | SVal _ -> Elt x
	| SArith(op, sv1, sv2) ->
			let tr1 = linearize sv1 and tr2 = linearize sv2 in
			let lst_of_branch tr = match tr with 
				| Seq(op1, lst) when commute op op1 -> lst 
				| _ -> [|tr|] in 
			let left = lst_of_branch tr1 and right = lst_of_branch tr2 in
			Seq(op, left @@ right)			  
  | SMem sv -> Elt (SMem(canon sv))
  | SFCall(name, svlist) -> Elt( SFCall(name, List.map canon svlist))
  | SByte sv -> Elt(SByte(canon sv))

and rv_of_sv sl = function
	| SVar v -> LV(Var v), sl	
	| SPVar v -> LV(PVar v), sl	
	| SLReg r -> LV(LReg r), sl 
	| SVal n -> Val n, sl
	| SArith(op, sv1, sv2) -> Arith(op, rv_of_sv sl sv1, rv_of_sv sl sv2), sl
  | SMem sv -> LV(Mem(rv_of_sv sl sv)),sl
  | SFCall(name, svlist) -> FCall(name, List.map (rv_of_sv sl) svlist),sl
  | SByte sv -> Byte(rv_of_sv sl sv),sl

and canon sv = linearize sv |> sort |> sv_of_tree

and sort = function
	| Elt _ as org -> org
	| Seq(op, ar) ->
			let a = Array.map sort ar in 
			if commute op op then Array.sort cmp_tree a;
			Seq(op, a)

and cmp_tree a b = match a, b with
	| Elt sv1, Elt sv2 -> cmp_sv sv1 sv2
	| Elt _, Seq _ -> 1
	| Seq _, Elt _ -> -1
	| Seq(op1, a1), Seq(op2, a2) -> cmp_tree a1.(0) a2.(0) 
	
and cmp_sv a b = match a,b with
	| SVal x, SVal y -> y - x
	| SVal _, _ -> 1
	| _, SVal _ -> -1
	| _, _ -> compare a b  	

and sv_of_tree = function
	| Elt sv -> sv
	| Seq(op, ar) -> 
			let a = Array.map sv_of_tree ar in
			if commute op op then 
				let rec loop i = 
					match a.(i-1) with
					| SVal _ -> if i > 1 then loop (i-1) else i
					| _ -> i in
				let alen = Array.length a in
				let vals_index = loop alen in				
				if vals_index < alen then
					let left, right = Array.sub a 0 vals_index, Array.sub a vals_index (alen-vals_index) in
					SArith(op, ari_fold op left, ari_fold op right)
				else ari_fold op a	
			else ari_fold op a
			
let canon_rv rv = sv_of_rv rv |> canon |> rv_of_sv (snd rv) |> Leoc.simp_rvalue 

let canonicalize code = 
	let o = object
			inherit mapper as super
			method map_rvalue rv = super#map_rvalue (canon_rv rv)
		end in
	o#map_code code
	
(*************************** search for common subexpressions *******************************)
		
type rvalue_info = Once of Leoc.statement * Leoc.raw_rvalue | More of name
type stmt_action = (name * Leoc.rvalue) list
type rvalue_action = Load of name 		
		
let raw_rvalue_compare a b =
	let o = object(self)
		inherit mapper as super
		method map_rvalue (rv,sl) = self#map_raw_rvalue rv, no_source
	end	in
	let a' = o#map_raw_rvalue a and b' = o#map_raw_rvalue b in
	compare a' b'	
		
module RM = Map.Make(struct type t = Leoc.raw_rvalue let compare = raw_rvalue_compare end)		
module RvHash = Hashtbl.Make(struct type t = Leoc.raw_rvalue let equal = (==) let hash = Hashtbl.hash end)
module StHash = Hashtbl.Make(struct type t = Leoc.statement  let equal = (==) let hash = Hashtbl.hash end)
			
type history = rvalue_info RM.t;;
		
let rv_actions = RvHash.create 100
let st_actions = StHash.create 100		

let show_info = function
	| Once _ -> "once"
	| More nm -> "more(" ^ nm ^ ")"

let show_history hist = 
	RM.fold (fun rrv inf lst -> (Printf.sprintf "%s => %s" (show_rvalue (rrv,1)) (show_info inf))::lst) hist []
	|> String.concat "; "		

let print_history = show_history >> print_endline
										
let add_to_history st hist ((rval,sl) as rv) = 
	try 
		match RM.find rval hist with
		| Once(st, rval1) -> 
				let vname = Printf.sprintf "shared_%d" (uid()) in
				let varname = 
					try
						let actions = StHash.find st_actions st in					
						try	List.find (fun (aname, arv) -> arv = rv) actions |> fst 
						with Not_found -> StHash.add st_actions st ((vname, rv)::actions);	vname
					with Not_found ->   StHash.add st_actions st [vname, rv];	vname in			
				RvHash.add rv_actions rval (Load varname);
				RvHash.add rv_actions rval1 (Load varname);
				RM.add rval (More varname) hist 
		| More nm -> 
				RvHash.add rv_actions rval (Load nm);
				hist	 
	with Not_found -> RM.add rval (Once(st,rval)) hist;;		
		
let rec rval_uses_lv rval lv = match rval with
	| LV lv' -> lv' = lv || lv_uses_lv lv' lv 
  | Val i -> false
  | Arith(op, rv1, rv2) -> rval_uses_lv (fst rv1) lv || rval_uses_lv (fst rv2) lv
  | FCall(name, rvs) -> List.exists (fst >> flip rval_uses_lv lv) rvs
  | Byte rv -> rval_uses_lv (fst rv) lv 		
		
and lv_uses_lv who whom = match who with
	| Var _ | PVar _ | LReg _ -> false
	| Mem rv ->	rval_uses_lv (fst rv) whom 	
		
let clear_lv_uses hist lv =
	RM.fold (fun rval info nhist ->	if rval_uses_lv rval lv then nhist else RM.add rval info nhist) hist RM.empty;;		
		
module LVSet = Set.Make(struct type t = Leoc.lvalue let compare = compare end)		
		
let gather_lvs code = 
	let lvs = ref LVSet.empty in
	let o = object
		inherit mapper as super
		method map_raw_stmt st = match st with
		| Assign(_, lv, _) 
		| Alloc(lv, _) -> lvs := LVSet.add lv !lvs; super#map_raw_stmt st
		| _ -> super#map_raw_stmt st
	end in		
	let _ = o#map_code code in
	!lvs
		
let rec exam_code hist code = 
	(*List.fold_left (fun h st -> let h' = exam_stmt h st in print_history h'; h') hist code*)
	List.fold_left exam_stmt hist code
	
and exam_stmt hist ((stmt, sl) as st) = match stmt with
  | Break 
  | Trash _  
  | DefVar _ -> hist
  | Alloc(lv, rv)  
  | Assign(_, lv, rv) -> 
			let h = exam_rvalue st (exam_lvalue st hist lv) rv in
			clear_lv_uses h lv  
  | Call(name, rvs) -> List.fold_left (exam_rvalue st) hist rvs  
  | Defun(name, params, code) -> hist
  | Ret rvs -> List.fold_left (exam_rvalue st) hist rvs
  | If(con, code1, code2) -> 
			let hist_con = exam_cond st hist con in 
			let _ = exam_code hist_con code1 and _ = exam_code hist_con code2 in
			let lvs1 = gather_lvs code1 and lvs2 = gather_lvs code2 in
			LVSet.fold (flip clear_lv_uses) (LVSet.union lvs1 lvs2) hist_con			
  | While(con, code) ->			
			let lvs = gather_lvs code in
			let h = LVSet.fold (flip clear_lv_uses) lvs hist in
			let hist_con = exam_cond st h con in
			let _ = exam_code hist_con code in
			hist_con		
  | Print rv 
  | Prchar rv -> exam_rvalue st hist rv 
  | Comp code -> ignore(exam_code hist code); hist
	| PostMessage(msg, rv1, rv2) -> List.fold_left (exam_rvalue st) hist [rv1; rv2]

and exam_lvalue st hist = function
  | Var _ | PVar _ | LReg _ -> hist
	| Mem rv -> exam_rvalue st hist rv

and exam_rvalue st hist ((rval, sl) as rv) = match rval with
  | LV (Mem rv') -> let h = add_to_history st hist rv in exam_rvalue st h rv'
	| LV _ | Val _ -> hist 
  | Arith(op, rv1, rv2) -> let h = add_to_history st hist rv in List.fold_left (exam_rvalue st) h [rv1; rv2]
  | FCall(name, rvs) -> List.fold_left (exam_rvalue st) hist rvs 
  | Byte rv' -> let h = add_to_history st hist rv in exam_rvalue st h rv'

and exam_cond st hist = function
  | Less(rv1, rv2) 
  | Eq(rv1, rv2) -> List.fold_left (exam_rvalue st) hist [rv1; rv2]
  | And(con1, con2) 
  | Or(con1, con2) -> exam_cond st (exam_cond st hist con1) con2
  | Not con -> exam_cond st hist con
	
let apply_changes code =
	let o = object(self)
		inherit mapper as super
		method map_raw_rvalue rv =
			try 
				(match RvHash.find rv_actions rv with
				| Load vname -> LV(Var vname)) 
			with Not_found -> super#map_raw_rvalue rv
			
    method map_code code = List.map self#map_stmt2 code |> List.concat	
		method map_stmt2 st =
			try
				let alist = StHash.find st_actions st in
				let sl = snd st in
				let calcs = List.map (fun (nm, rv) -> [DefVar nm, sl; Assign(ASInt, Var nm, rv), sl]) alist |> List.concat in
				calcs @ [super#map_stmt st]
			with Not_found -> [super#map_stmt st]
	end in
	o#map_code code
	
let dry code = 
	RvHash.clear rv_actions; StHash.clear st_actions;
	let _ = exam_code RM.empty code in 
	print_endline "\nrv_actions:";
	RvHash.iter (fun rv (Load name) -> Printf.printf "%s => %s\n" (show_rvalue (rv,1)) name) rv_actions;
	print_endline "\nst_actions:";
	StHash.iter (fun st alist -> Printf.printf "%s => %s\n" (show_stmt 0 st) 
		(List.map (fun (nm,rv) -> Printf.sprintf "%s=%s" nm (show_rvalue rv)) alist |> String.concat "; ")) st_actions;
	apply_changes code 