open ExtLib
open Commons

let encode_signs numbers =
	let rec loop lst res =
		match lst with
		| [] -> List.rev res 
		| x::tail -> 
				if x > 0 then loop tail (  x::1::res) else
				if x = 0 then loop tail (     2::res) else 
					            loop tail (0-x::3::res) in
	loop numbers []

let decode_signs numbers = 
	let rec loop lst res = 
		match lst with
		| [] -> List.rev res
		| 1::x::tail -> loop tail (x::res)
		| 2::tail -> loop tail (0::res)
		| 3::x::tail -> loop tail (0-x :: res) 
		| _ -> failwith "decode_signs: bad data" in
	loop numbers [] 
	
let fibs = DynArray.make 100;;
DynArray.add fibs 1;
DynArray.add fibs 2;;

let make_fibs x =
	while DynArray.last fibs < x do
		let z = DynArray.last fibs + DynArray.get fibs (DynArray.length fibs - 2) in
		DynArray.add fibs z
	done
	
let fib_encode x =
	make_fibs x;
	let rec loop n i res st =
		if i < 0 then res else
		let f = DynArray.get fibs i in 
		if f <= n then loop (n-f) (i-1) (1::res) true else loop n (i-1) (if st then 0::res else res) st in
	loop x (DynArray.length fibs - 1) [1] false	 	
	
let tobytes bits = 
	let rec loop lst res =
		if lst=[] then List.rev res else
		let l8, tail = 
			try ExtList.List.split_nth 8 lst 
			with List.Invalid_index _ -> lst @ (List.make (8 - List.length lst) 0), [] in
		let b = List.fold_right (fun bit (k,sum) -> (k*2, sum + bit*k)) l8 (1, 0) |> snd in
		loop tail (b::res) in
	loop bits []
	
let compress lst =
	lst |> encode_signs |> List.map fib_encode |> List.concat |> tobytes

let tobits bytes =
	List.map (fun x -> [x lsr 7; (x lsr 6) land 1; (x lsr 5) land 1; (x lsr 4) land 1;
	 									(x lsr 3) land 1; (x lsr 2) land 1; (x lsr 1) land 1; x  land 1]) bytes |> List.concat
	
let rec fib n = 
	if n < 2 then n+1 else 
	if n < DynArray.length fibs then DynArray.get fibs n else	
	let z = fib (n-1) + fib (n-2) in
	make_fibs z; z					
																												
let fib_decode lst =
	let rec loop bits n last_bit res =
		match bits with
		| [] -> 0, []
		| 1::tail -> if last_bit=1 then res, tail else loop tail (n+1) 1 (res + fib n)
		| 0::tail -> loop tail (n+1) 0 res
		| _ -> failwith "bad bits" in
	loop lst 0 0 0
	
let decompress bytes =
	let bits = tobits bytes in
	let rec loop bitlist res =
		if bitlist=[] then List.rev res else 
		let x, tail = fib_decode bitlist in
		loop tail (if x=0 then res else x::res) in
	loop bits [] |> decode_signs
			
let test () =
	let lst = [2061; 1; 80; 20; 2; 1; 19; 3; 0; 11; 26; 514; 4; 3; 8; 0; 
4; 2; 4; 4103; 4; 3; 21; 3; 0; 0; 521; 11; 3; 10; 2061; 3; 
10; 20; 4; 3; 18; 5; 3; 10; 20; 6; 1; 18; 7; 1; 80; 11; 
60; 5128; 4; 6; 21; 4; 0; 0; 25; 6; 0; 0; 17; 66; 4; 5; 
11; 70; 17; 49; 6; 7; 2061; 4; 40; 20; 5; 4; 18; 6; 4; 40; 
20; 7; 3; 18; 8; 3; 10; 11; 106; 19; 9; 0; 1032; 9; 7; 4122; 
5; 9; 22; 5; 0; 0; 21; 7; 0; 0; 17; 112; 5; 6; 11; 116; 
17; 89; 7; 8; 20; 5; 1; 18; 6; 1; 80; 11; 132; 1036; 0; 5; 
25; 5; 0; 0; 17; 125; 5; 6; 20; 5; 3; 18; 6; 3; 10; 11; 
158; 19; 7; 0; 1032; 7; 5; 12; 0; 7; 21; 5; 0; 0; 17; 145; 
5; 6; 20; 5; 4; 18; 6; 4; 40; 11; 178; 1036; 0; 5; 22; 5; 
0; 0; 17; 171; 5; 6] in
	let enc = encode_signs lst in
	List.iter (Printf.printf "%d ") enc;
	print_endline "\ndec:";
	let dec = decode_signs enc in
	List.iter (Printf.printf "%d ") dec;
	print_endline "\ncompr:";
	let com = compress lst in
	List.iter (Printf.printf "%d ") com;
	Printf.printf "\nlen: org=%d com=%d\n" (List.length lst) (List.length com);
	let decom = decompress com in
	List.iter (Printf.printf "%d ") decom;
	(*print_endline "\nfibs:";
	make_fibs 100;
	DynArray.iter (Printf.printf "%d ") fibs;
	let f = fib_encode 42 in
	print_endline "\nf 42:";
	List.iter (Printf.printf "%d ") f*);;
		
