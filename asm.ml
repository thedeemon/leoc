open ExtLib;;
open Commons;;
(*
#define ADD   (1  << CMDSHIFT) | DAA   //d, a1, a2 : d = a1 + a2
#define MUL   (2  << CMDSHIFT) | DAA   //d, a1, a2 : d = a1 * a2
#define MOD   (3  << CMDSHIFT) | DAA   //d, a1, a2 : d = a1 % a2
#define SUB   (4  << CMDSHIFT) | DAA   //d, a1, a2 : d = a1 - a2
#define DIV   (5  << CMDSHIFT) | DAA   //d, a1, a2 : d = a1 / a2
#define XOR   (6  << CMDSHIFT) | DAA   //d, a1, a2 : d = a1 ^ a2
#define MOV   (7  << CMDSHIFT) | DA    //d, a1 : d = a1
#define MOVB  (8  << CMDSHIFT) | DA    //d, a1 : d = a1
#define JMPLE (9  << CMDSHIFT) | XAA   //addr, a1, a2 : if a1 < a2  ip = addr
#define JMPEQ (10 << CMDSHIFT) | XAA   //addr, a1, a2 : if a1 == a2 ip = addr
#define JMP   (11 << CMDSHIFT)         //addr : ip = addr
#define PRINT (12 << CMDSHIFT) | XA    //x, a1 : print a1
#define PRCHAR (23 << CMDSHIFT) | XA    //x, a1 : print a1
#define NEW   (13 << CMDSHIFT) | DA    //d, a1 : d = malloc(a)
#define CALL  (14 << CMDSHIFT) | XA    //addr, a1 : fp += a1, call addr
#define RET   (15 << CMDSHIFT)         //: return from call
*)

type dst = RegDest of int | PntDest of int
type src = Reg of int | Pnt of int | Val of int

type 'loc command = 'loc raw_command * source_loc
and 'loc raw_command = 
	| Arith of oper * dst * src * src
	| Mov of arg_size * dst * src
	| Jmple of 'loc * src * src
	| Jmpeq of 'loc * src * src
	| Jmp of 'loc
	| Print of src
	| Prchar of src
	| New of dst * src
	| Call of 'loc * src
	| Ret
	| Label of 'loc
	| PostMessage of int * src * src;;

type 'loc program = 'loc command DynArray.t;;

let dst_pr = function RegDest _ -> 'R' | PntDest _ -> 'P';;
let src_pr = function Reg _ -> 'R' | Pnt _ -> 'P' | Val _ -> 'V';;
let dst_n = function RegDest n | PntDest n -> n;;
let src_n = function Reg n | Pnt n | Val n -> n;;
let dst_of_src = function Reg r -> RegDest r | Pnt r -> PntDest r | Val _ -> failwith "Val in Asm.dst_of_src";;

let oper_s = function
	| Add->"ADD" | Mul->"MUL" | Mod->"MOD" | Div->"DIV" | Sub->"SUB" | Xor->"XOR"

let arg_size_suff = function
	| ASByte -> "B" | ASInt32 -> "D" | ASInt -> ""

let cmd_to_text = function
	| Arith(op, d, a1, a2) -> Printf.sprintf "%s|%c%c%c, %d, %d, %d,\n" (oper_s op) (dst_pr d) (src_pr a1) (src_pr a2) (dst_n d) (src_n a1) (src_n a2)  
	| Mov(sz, d, a1) -> Printf.sprintf "MOV%s|%c%cR, %d, %d,\n" (arg_size_suff sz) (dst_pr d) (src_pr a1) (dst_n d) (src_n a1)
	| Jmple(addr, a1, a2) -> Printf.sprintf "JMPLE|R%c%c, %d, %d, %d, //%s\n" (src_pr a1) (src_pr a2) (fst addr) (src_n a1) (src_n a2) (snd addr)
	| Jmpeq(addr, a1, a2) -> Printf.sprintf "JMPEQ|R%c%c, %d, %d, %d, //%s\n" (src_pr a1) (src_pr a2) (fst addr) (src_n a1) (src_n a2) (snd addr)
	| Jmp addr -> Printf.sprintf "JMP, %d, //%s\n" (fst addr)  (snd addr)
	| Print a1 -> Printf.sprintf "PRINT|R%cR, 0, %d,\n" (src_pr a1) (src_n a1)
	| Prchar a1 -> Printf.sprintf "PRCHAR|R%cR, 0, %d,\n" (src_pr a1) (src_n a1)
	| New(d, a1) -> Printf.sprintf "NEW|%c%cR, %d, %d,\n" (dst_pr d) (src_pr a1) (dst_n d) (src_n a1)
	| Call(addr, a1) -> Printf.sprintf "CALL|R%cR, %d, %d, //%s\n" (src_pr a1) (fst addr) (src_n a1) (snd addr)
	| Ret -> "RET,\n"
	| Label addr -> Printf.sprintf "//%d - %s:\n" (fst addr) (snd addr)
	| PostMessage(msg, a1, a2) -> Printf.sprintf "POSTMSG|R%c%c, %d, %d, %d,\n" (src_pr a1) (src_pr a2) msg (src_n a1) (src_n a2) 
	
let cmd_size (cmd,sl) = match cmd with 
	(*String.enum (to_s cmd) |> Enum.filter_map (function ',' -> Some 1 | _ -> None) |> Enum.count*) 
	| Arith _ | Jmple _ | Jmpeq _ | PostMessage _ -> 4
	| Mov _ | New _ | Print _ | Prchar _ | Call _ -> 3
	| Jmp _ -> 2	| Ret -> 1	| Label _ -> 0;;

module M = Map.Make(String);;

let resolve_labels sizer prg = 
	let labels = DynArray.fold_left (fun (m, len) cmd ->
		let len' = len + sizer cmd in 
		match cmd with
		| Label str, _ -> M.add str len m, len'
		| _ -> m, len') (M.empty, 0) prg |> fst in
  let find lab = try M.find lab labels, lab with Not_found -> (Printf.printf "Error: label '%s' not found.\n" lab; 0, lab) in
	let cmd_map = function
		| Jmple(lab, a1, a2) -> Jmple(find lab, a1, a2)
		| Jmpeq(lab, a1, a2) -> Jmpeq(find lab, a1, a2)
		| Jmp lab -> Jmp (find lab)
		| Call(lab, a1) -> Call(find lab, a1)
		| Label lab -> Label (find lab)
		| Arith(op, d, a1, a2) -> Arith(op, d, a1, a2) 
		| Mov(sz, d, a1) -> Mov(sz, d, a1) 
		| New(d, a1) -> New(d, a1) 
		| Print x -> Print x 
		| Prchar x -> Prchar x 
		| Ret -> Ret
		| PostMessage(msg, a1, a2) -> PostMessage(msg, a1, a2) in	
	DynArray.map (fun (cmd,sl) -> cmd_map cmd, sl) prg
		  
let label_indices prg = 
	DynArray.enum prg |> Enum.foldi (fun i cmd m -> match cmd with Label str, _ -> M.add str i m | _ -> m) M.empty;;

let remove_dead_code prg =
	let labels = label_indices prg in
	let live = Array.make (DynArray.length prg) false in
	let threads = Queue.create () in
	Queue.add 0 threads;
	let rec go ip =
		if ip >= DynArray.length prg || live.(ip) then () else
			(live.(ip) <- true;
			match DynArray.get prg ip with
			| Jmp lab, _ ->
					let target = M.find lab labels in
					(*if !verbose then Printf.printf "# code[%d]: jmp %d\n" ip target;*)
					if target = ip + 1 then (live.(ip) <- false; go (ip+1)) else Queue.add target threads
			| Ret,_ -> ()
			| Jmple(lab, _, _),_ | Call(lab, _),_ 	
			| Jmpeq(lab, _, _),_ -> Queue.add (M.find lab labels) threads; go (ip+1)
			| _ -> go (ip+1)) in				
	while not (Queue.is_empty threads) do
		go (Queue.take threads)
	done;
	prg |> DynArray.enum |> Enum.mapi (fun i cmd -> live.(i), cmd) 
	|> Enum.filter_map (fun (live, cmd) -> if live then Some cmd else None) |> DynArray.of_enum;;	

let optimize_jumps prg =
	let labels = label_indices prg in
	let find_idx lab = try M.find lab labels with Not_found -> failwith (Printf.sprintf "Error: label '%s' not found.\n" lab) in 
	let rec label_target lab =
		let start = find_idx lab in
		let rec loop i curlab =
			if i >= DynArray.length prg then curlab else
			match DynArray.get prg i with
			| Jmp l,_ -> label_target l
			| Label _, _ -> loop (i+1) curlab
			| _ -> curlab  in
		loop (start+1) lab  in
	prg |> DynArray.map (function
		| Jmple(lab, a1, a2),sl -> Jmple(label_target lab, a1, a2),sl
		| Jmpeq(lab, a1, a2),sl -> Jmpeq(label_target lab, a1, a2),sl
		| Jmp lab,sl -> Jmp (label_target lab),sl
		| x -> x) 
	|> remove_dead_code |> remove_dead_code;;  
		
		
let cmd_to_micro = function
	| Arith(op, d, a1, a2) -> 
			Printf.sprintf "LOAD_SRC2_%c, %d,\nLOAD_DEST_%c, %d,\nLOAD_SRC1_%c, %d,\n%s,\n" 
				(src_pr a2) (src_n a2) (dst_pr d) (dst_n d) (src_pr a1) (src_n a1) (oper_s op)
	| Mov(sz, d, a1) -> Printf.sprintf "LOAD_DEST_%c, %d,\nLOAD_SRC1_%c, %d,\nMOV%s,\n" (dst_pr d) (dst_n d) (src_pr a1) (src_n a1) (arg_size_suff sz)
	| Jmple(addr, a1, a2) -> 
			Printf.sprintf "LOAD_SRC2_%c, %d,\nLOAD_SRC1_%c, %d,\nJMPLE, %d, //%s\n"
				(src_pr a2) (src_n a2) (src_pr a1) (src_n a1) (fst addr) (snd addr)
	| Jmpeq(addr, a1, a2) -> 
			Printf.sprintf "LOAD_SRC2_%c, %d,\nLOAD_SRC1_%c, %d,\nJMPEQ, %d, //%s\n"
				(src_pr a2) (src_n a2) (src_pr a1) (src_n a1) (fst addr) (snd addr)		
	| Jmp addr -> Printf.sprintf "JMP, %d, //%s\n" (fst addr)  (snd addr)
	| Print a1 -> Printf.sprintf "LOAD_SRC1_%c, %d,\nPRINT,\n" (src_pr a1) (src_n a1)
	| Prchar a1 -> Printf.sprintf "LOAD_SRC1_%c, %d,\nPRCHAR,\n" (src_pr a1) (src_n a1)
	| New(d, a1) -> Printf.sprintf "LOAD_DEST_%c, %d,\nLOAD_SRC1_%c, %d,\nNEW,\n" 
										(dst_pr d) (dst_n d) (src_pr a1) (src_n a1)
	| Call(addr, a1) -> Printf.sprintf "LOAD_SRC1_%c, %d,\nCALL, %d, //%s\n" (src_pr a1) (src_n a1) (fst addr) (snd addr)
	| Ret -> "RET,\n"
	| Label addr -> Printf.sprintf "//%d, %s:\n" (fst addr) (snd addr)
	| PostMessage _ -> ""		
		
let cmd_size_micro = function 
	| Arith _ -> 7 
	| Jmple _ | Jmpeq _ -> 6
	| Mov _  -> 5
	| New _ -> 5 
	| Print _ | Prchar _ -> 3 
	| Call _ -> 4
	| Jmp _ -> 2	| Ret -> 1	| Label _ | PostMessage _ -> 0;;
		
let last_line = ref 0
		
let cmd_to_lvm2 (cmd, sl) = 
	let ls = if sl = !last_line then "" else begin
		last_line := sl;
		Printf.sprintf "/// %s\n" (prog_source sl)
	end in
	let cs = match cmd with
	| Arith(Add, RegDest dr, Reg r1, Val 1) when dr = r1 -> Printf.sprintf "INC, %d, 0, 0," dr  
	| Arith(Add, RegDest dr, Reg r1, Val 4) when dr = r1 -> Printf.sprintf "INC4, %d, 0, 0," dr   
	| Arith(Add, RegDest dr, Reg r1, Val 8) when dr = r1 && not !int32_is_int -> Printf.sprintf "INC8, %d, 0, 0," dr   
	| Arith(Add, RegDest dr, Reg r1, Val v2) -> Printf.sprintf "ADD_RRV, %d, %d, %d," dr r1 v2  
	| Arith(Add, RegDest dr, Reg r1, Reg r2) -> Printf.sprintf "ADD_RRR, %d, %d, %d," dr r1 r2  
	| Arith(op, d, a1, a2) -> Printf.sprintf "%s|%c%c%c, %d, %d, %d," (oper_s op) (dst_pr d) (src_pr a1) (src_pr a2) (dst_n d) (src_n a1) (src_n a2)  
	| Mov(ASInt, RegDest dr, Reg r1) -> Printf.sprintf "MOV_RR, %d, %d," dr r1
	| Mov(ASInt, RegDest dr, Val v1) -> Printf.sprintf "MOV_RV, %d, %d," dr v1
	| Mov(sz, d, a1) -> Printf.sprintf "MOV%s|%c%cR, %d, %d," (arg_size_suff sz) (dst_pr d) (src_pr a1) (dst_n d) (src_n a1)
	| Jmple(addr, Reg r1, Reg r2) -> Printf.sprintf "JMPLE_RR, %d, %d, %d, //%s"  (fst addr) r1 r2 (snd addr)
	| Jmple(addr, a1, a2) -> Printf.sprintf "JMPLE|R%c%c, %d, %d, %d, //%s" (src_pr a1) (src_pr a2) (fst addr) (src_n a1) (src_n a2) (snd addr)
	| Jmpeq(addr, a1, a2) -> Printf.sprintf "JMPEQ|R%c%c, %d, %d, %d, //%s" (src_pr a1) (src_pr a2) (fst addr) (src_n a1) (src_n a2) (snd addr)
	| Jmp addr -> Printf.sprintf "JMP, %d, //%s" (fst addr)  (snd addr)
	| Print a1 -> Printf.sprintf "PRINT|R%cR, 0, %d," (src_pr a1) (src_n a1)
	| Prchar a1 -> Printf.sprintf "PRCHAR|R%cR, 0, %d," (src_pr a1) (src_n a1)
	| New(d, a1) -> Printf.sprintf "NEW|%c%cR, %d, %d," (dst_pr d) (src_pr a1) (dst_n d) (src_n a1)
	| Call(addr, a1) -> Printf.sprintf "CALL|R%cR, %d, %d, //%s" (src_pr a1) (fst addr) (src_n a1) (snd addr)
	| Ret -> "RET,"
	| Label addr -> Printf.sprintf "//%d - %s:" (fst addr) (snd addr)
	| PostMessage(msg, a1, a2) -> Printf.sprintf "POSTMSG|R%c%c, %d, %d, %d," (src_pr a1) (src_pr a2) msg (src_n a1) (src_n a2)
	in ls ^ cs		
	
let cmd n = n (* n << CMDSHIFT, CMDSHIFT = 0 now *)
let modshift = 8
				
let op_add_rrr = cmd 0
let op_mov = cmd 7
let op_movb = cmd 8
let op_jmple = cmd 9
let op_jmpeq = cmd 10
let op_jmp = cmd 11
let op_print = cmd 12
let op_new = cmd 13
let op_call = cmd 14
let op_ret = cmd 15

let op_jmple_rr = cmd 17
let op_add_rrv = cmd 18
let op_mov_rv = cmd 19
let op_mov_rr = cmd 20
let op_inc = cmd 21 
let op_inc4 = cmd 22
let op_prchar = cmd 23
let op_postmsg = cmd 24
let op_inc8 = cmd 25
let op_movd = cmd 26
 
let oper_code = function
	| Add-> cmd 1 | Mul-> cmd 2 | Mod-> cmd 3 | Div-> cmd 5 | Sub-> cmd 4 | Xor-> cmd 6;;

let dst_mod = function RegDest _ -> 0 | PntDest _ -> 1
let src_mod = function Reg _ -> 0 | Pnt _ -> 1 | Val _ -> 2

let modi d a1 a2 = 
	(((dst_mod d) lsl 4) + ((src_mod a1) lsl 2) + (src_mod a2)) lsl modshift 
		
let mov_op = function
	| ASByte -> op_movb | ASInt32 -> op_movd | ASInt -> op_mov		
		
let cmd_to_bc (cmd,sl) = match cmd with		
	| Arith(Add, RegDest dr, Reg r1, Val 1) when dr = r1 -> [op_inc; dr; 0; 0]  
	| Arith(Add, RegDest dr, Reg r1, Val 4) when dr = r1 -> [op_inc4; dr; 0; 0]   
	| Arith(Add, RegDest dr, Reg r1, Val 8) when dr = r1 -> [op_inc8; dr; 0; 0]   
	| Arith(Add, RegDest dr, Reg r1, Val v2) -> [op_add_rrv; dr; r1; v2]  
	| Arith(Add, RegDest dr, Reg r1, Reg r2) -> [op_add_rrr; dr; r1; r2]  
	| Arith(op, d, a1, a2) -> [oper_code op lor modi d a1 a2; dst_n d; src_n a1; src_n a2]  
	| Mov(ASInt, RegDest dr, Reg r1) -> [op_mov_rr; dr; r1]
	| Mov(ASInt, RegDest dr, Val v1) -> [op_mov_rv; dr; v1]
	| Mov(sz, d, a1) -> [(mov_op sz) lor modi d a1 (Reg 0); dst_n d; src_n a1]
	| Jmple(addr, Reg r1, Reg r2) -> [op_jmple_rr; (fst addr); r1; r2]
	| Jmple(addr, a1, a2) -> [op_jmple lor modi (RegDest 0) a1 a2; fst addr; src_n a1; src_n a2]
	| Jmpeq(addr, a1, a2) -> [op_jmpeq lor modi (RegDest 0) a1 a2; fst addr; src_n a1; src_n a2]
	| Jmp addr -> [op_jmp; fst addr]
	| Print a1 -> [op_print lor modi (RegDest 0) a1 (Reg 0); 0; src_n a1]
	| Prchar a1 -> [op_prchar lor modi (RegDest 0) a1 (Reg 0); 0; src_n a1]
	| New(d, a1) -> [op_new lor modi d a1 (Reg 0); dst_n d; src_n a1]
	| Call(addr, a1) -> [op_call lor modi (RegDest 0) a1 (Reg 0); fst addr; src_n a1]
	| Ret -> [op_ret]
	| Label addr -> []	
	| PostMessage(msg, a1, a2) -> [op_postmsg lor modi (RegDest 0) a1 a2; msg; src_n a1; src_n a2]	
				
let process quiet prg =
	let cmds = prg |> optimize_jumps |> resolve_labels cmd_size in  
	if not quiet then DynArray.fold_left (fun ip cmd -> 
		Printf.printf "%s //IP: %d\n" (cmd_to_lvm2 cmd) ip;
		ip + cmd_size cmd) 0  cmds |> ignore;
	cmds |> (DynArray.to_list >> List.map cmd_to_bc >> List.concat);;
				
(*let process_micro prg = prg |> optimize_jumps |> resolve_labels cmd_size_micro 
  |> DynArray.iter (cmd_to_micro >> print_string);;*)

(*let prg_fib = [
	Mov(RegDest 0, Val 1);
	Label "mainloop";
	Jmple("go", Reg 0, Val 21);
	Ret;
	Label "go";
	Mov(RegDest 1, Reg 0);
	Call("fib", Val 2);
	Print(Reg 1);
	Arith(Add, RegDest 0, Reg 0, Val 1);
	Jmp "mainloop";
	Label "fib";
	Jmple("one", Reg (-1), Val 2);
	Arith(Sub, RegDest 0, Reg(-1), Val 1);
	Call("fib", Val 1);
	Arith(Sub, RegDest 1, Reg(-1), Val 2);
	Call("fib", Val 2);
	Arith(Add, RegDest(-1), Reg 0, Reg 1);
	Ret;
	Label "one";
	Mov(RegDest(-1), Val 1);
	Ret
	];;

prg_fib |> DynArray.of_list |> resolve_labels |> DynArray.iter (cmd_to_text >> print_string);;*)