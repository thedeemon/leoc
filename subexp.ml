open ExtLib
open Commons
open Leoc

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
