open ExtLib
open Commons

type 'a tree = Elt of 'a | Seq of 'a tree list

type dst = Asm.dst
type src = Asm.src
type name = string
type code = statement list
and statement = raw_statement * source_loc
and raw_statement = 
	| Arith of oper * dst * src * src
	| Mov of arg_size * dst * src
	| Print of src
	| Prchar of src
	| New of dst * src
	| Call of name * int
	| Defun of name * code
	| If of condition * code * code
	| While of condition * code
	| Ret
	| Break
	| Goto of name
	| PostMessage of int * src * src

and condition = 
	| Less of code * src * code * src
	| Eq of code * src * code * src
	| And of condition * condition
	| Or of condition * condition
	| Not of condition;;

let da x = Elt x;;(* DynArray.of_list [x];;*)

let rec compile_stmt ctx (stmt,sl) = match stmt with
	| Arith(op, d, a1, a2) -> da (Asm.Arith(op, d, a1, a2),sl)
	| Mov(sz, d, a1) -> da (Asm.Mov(sz, d, a1),sl)
	| Print a1 -> da (Asm.Print a1, sl)
	| Prchar a1 -> da (Asm.Prchar a1, sl)
	| New(d, a1) -> da (Asm.New(d, a1), sl)
	| Call(lab, a1) -> da (Asm.Call(lab, Asm.Val(Int64.of_int a1)), sl)
	| Defun(name, code) -> 
			let end_lab = Printf.sprintf "endproc_%d" (uid ()) in
			Seq [Elt (Asm.Jmp end_lab, sl);
			Elt (Asm.Label name, sl);
			compile [] code;
			Elt (Asm.Ret, sl);
			Elt (Asm.Label end_lab, sl)]
	| If(cond, then_code, else_code) ->	compile_if ctx cond (compile ctx then_code) (compile ctx else_code) sl
	| While(cond, code) ->
			let k = uid () in
			let while_lab = Printf.sprintf "while_cond_%d" k and body_lab = Printf.sprintf "while_body_%d" k 
			and end_lab = Printf.sprintf "endloop_%d" k in
			Seq [Elt (Asm.Jmp while_lab, sl);
			Elt(Asm.Label body_lab, sl);
			compile (end_lab::ctx) code;
			Elt (Asm.Label while_lab, sl);
			compile_if ctx cond (Elt (Asm.Jmp body_lab, sl)) (Seq []) sl;
			Elt (Asm.Label end_lab, sl)]	  
	| Ret -> da (Asm.Ret, sl) 
	| Break -> (match ctx with end_lab::rest -> da(Asm.Jmp end_lab, sl) | [] -> failc "break from not a loop" sl)
	| Goto name -> da (Asm.Jmp name, sl)
	| PostMessage(msg, a1, a2) -> da (Asm.PostMessage(msg, a1, a2), sl)

and compile ctx prg = 
	(*prg |> List.enum |> Enum.map (compile_stmt ctx >> DynArray.enum) |> Enum.concat |> DynArray.of_enum*)
	Seq (List.map (compile_stmt ctx) prg)
	
and compile_if ctx cond then_ else_ sl =
	let comp_condjmp c1 c2 jmp_cmd =
	  let id = uid () in
		let then_lab = Printf.sprintf "then_%d" id and end_lab = Printf.sprintf "endif_%d" id in
		Seq [compile ctx c1;
		compile ctx c2;
		Elt(jmp_cmd then_lab); 
		else_ ;
		Elt (Asm.Jmp end_lab, sl);
		Elt (Asm.Label then_lab, sl);
		then_;
		Elt (Asm.Label end_lab, sl)] in
	match cond with
	| Less(c1, a1, c2, a2) -> comp_condjmp c1 c2 (fun then_lab -> Asm.Jmple(then_lab, a1, a2), sl)
	| Eq(c1, a1, c2, a2)   -> comp_condjmp c1 c2 (fun then_lab -> Asm.Jmpeq(then_lab, a1, a2), sl)
	| Not con -> compile_if ctx con else_ then_ sl
	| And(con1, con2) -> 
			let else_lab = Printf.sprintf "else_%d" (uid ()) in
			let else2 = Seq [Elt (Asm.Label else_lab, sl); else_] in
			let code2 = compile_if ctx con2 then_ else2 sl in
			compile_if ctx con1 code2 (da (Asm.Jmp else_lab, sl)) sl
	| Or(con1, con2) -> 
			let then_lab = Printf.sprintf "then_%d" (uid ()) in
			let then1 = Seq[ Elt (Asm.Label then_lab, sl); then_] in
			let then2 = da (Asm.Jmp then_lab, sl) in
			let else1 = compile_if ctx con2 then2 else_ sl in
			compile_if ctx con1 then1 else1 sl;;

let rec iter_tree f = function
	| Elt x -> f x
	| Seq lst -> List.iter (iter_tree f) lst 

let to_dynarr da tr = iter_tree (DynArray.add da) tr; da;;

let process quiet prg = prg |> Prof.prof2 "Triasm.compile" compile []
  |> to_dynarr (DynArray.make 1000) 
	|> Prof.prof2 "Asm.process" Asm.process quiet;;

(*module A = Asm;;
let prg = [
	(*If(Or(Less(A.Val 1, A.Val 2), Eq(A.Val 3, A.Val 3)), [Print(A.Val 1)], [Print(A.Val 0)])*)
	Mov(A.RegDest 0, A.Val 0);
	While(Less([],A.Reg 0, [],A.Val 20), 
	[
		Arith(A.Mul, A.RegDest 1, A.Reg 0, A.Reg 0);
		Arith(A.Mod, A.RegDest 2, A.Reg 1, A.Val 2);
		Arith(A.Mod, A.RegDest 3, A.Reg 1, A.Val 5);
		If(Or(Eq([],A.Reg 2, [],A.Val 0), Eq([],A.Reg 3, [], A.Val 0)),
			[Print(A.Reg 1)], []);		
		Arith(A.Add, A.RegDest 0, A.Reg 0, A.Val 1)	
	])
];;

prg |> compile |> Asm.process;;*)				