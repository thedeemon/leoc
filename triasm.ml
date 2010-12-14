open ExtLib
open Commons

type dst = Asm.dst
type src = Asm.src
type name = string
type code = statement list
and statement = 
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

let da x = DynArray.of_list [x];;

let rec compile_stmt ctx = function
	| Arith(op, d, a1, a2) -> da (Asm.Arith(op, d, a1, a2))
	| Mov(sz, d, a1) -> da (Asm.Mov(sz, d, a1))
	| Print a1 -> da (Asm.Print a1)
	| Prchar a1 -> da (Asm.Prchar a1)
	| New(d, a1) -> da (Asm.New(d, a1))
	| Call(lab, a1) -> da (Asm.Call(lab, Asm.Val a1))
	| Defun(name, code) -> 
			let end_lab = Printf.sprintf "endproc_%d" (uid ()) in
			let asm = da (Asm.Jmp end_lab) in
			DynArray.add asm (Asm.Label name);
			DynArray.append (compile [] code) asm;
			DynArray.add asm Asm.Ret;
			DynArray.add asm (Asm.Label end_lab);
			asm
	| If(cond, then_code, else_code) ->	compile_if ctx cond (compile ctx then_code) (compile ctx else_code)
	| While(cond, code) ->
			let k = uid () in
			let while_lab = Printf.sprintf "while_cond_%d" k and body_lab = Printf.sprintf "while_body_%d" k 
			and end_lab = Printf.sprintf "endloop_%d" k in
			let asm = da (Asm.Jmp while_lab) in
			DynArray.add asm (Asm.Label body_lab);
			DynArray.append (compile (end_lab::ctx) code) asm;
			DynArray.add asm (Asm.Label while_lab);
			DynArray.append (compile_if ctx cond (da (Asm.Jmp body_lab)) (DynArray.make 0)) asm;
			DynArray.add asm (Asm.Label end_lab);
			asm	  
	| Ret -> da Asm.Ret 
	| Break -> (match ctx with end_lab::rest -> da(Asm.Jmp end_lab) | [] -> failwith "break from not a loop")
	| Goto name -> da (Asm.Jmp name)
	| PostMessage(msg, a1, a2) -> da (Asm.PostMessage(msg, a1, a2))

and compile ctx prg = 
	prg |> List.enum |> Enum.map (compile_stmt ctx >> DynArray.enum) |> Enum.concat |> DynArray.of_enum 
	
and compile_if ctx cond then_ else_ =
	let comp_condjmp c1 c2 jmp_cmd =
	  let id = uid () in
		let then_lab = Printf.sprintf "then_%d" id and end_lab = Printf.sprintf "endif_%d" id in
		let asm = compile ctx c1 in
		DynArray.append (compile ctx c2) asm;
		DynArray.add asm (jmp_cmd then_lab); 
		DynArray.append else_ asm;
		DynArray.add asm (Asm.Jmp end_lab);
		DynArray.add asm (Asm.Label then_lab);
		DynArray.append then_ asm;
		DynArray.add asm (Asm.Label end_lab);
		asm in
	match cond with
	| Less(c1, a1, c2, a2) -> comp_condjmp c1 c2 (fun then_lab -> Asm.Jmple(then_lab, a1, a2))
	| Eq(c1, a1, c2, a2)   -> comp_condjmp c1 c2 (fun then_lab -> Asm.Jmpeq(then_lab, a1, a2))
	| Not con -> compile_if ctx con else_ then_
	| And(con1, con2) -> 
			let else_lab = Printf.sprintf "else_%d" (uid ()) in
			let else2 = da (Asm.Label else_lab) in
			DynArray.append else_ else2;  
			let code2 = compile_if ctx con2 then_ else2 in
			compile_if ctx con1 code2 (da (Asm.Jmp else_lab))
	| Or(con1, con2) -> 
			let then_lab = Printf.sprintf "then_%d" (uid ()) in
			let then1 = da (Asm.Label then_lab) in
			DynArray.append then_ then1;
			let then2 = da (Asm.Jmp then_lab) in
			let else1 = compile_if ctx con2 then2 else_ in
			compile_if ctx con1 then1 else1;;

let process quiet prg = prg |> compile [] |> Asm.process quiet;;

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