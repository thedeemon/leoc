open ExtLib;;
open Commons;;

let echolexer lbuf =
	let l = Leolex.lexer lbuf in Printf.eprintf "[%s] " (Lexing.lexeme lbuf); flush stderr; l;;

(*let prog_ast_of_string str =
	let lbuf = Lexing.from_string str in
	Leo_parser.program echolexer (*Leolex.lexer*) lbuf;;*)

let write_le f i =
        output_byte f (i land 0xff);
        output_byte f ((i lsr 8) land 0xff);
        output_byte f ((i lsr 16) land 0xff);
        output_byte f (let t = ((i lsr 24) land 0xff) in if i >= 0 then t else t lor 0x80)

let process prg bc_handler quiet trash =
	let maybe = if !verbose then (fun f x -> f x) else (fun f x -> ()) in 
	prg |> maybe(Leo.show_code 0 >> print_endline);
	maybe print_endline "\nexpanded Leo:\n";
	let eprg = prg |> Leo.expand_code M.empty |> snd in
	eprg |> maybe(Leo.show_code 0 >> print_endline);
	maybe print_endline "\nLeoC:\n";
	let _, ccode = Leo.compile_code Leo.empty_context eprg in
	ccode |> maybe(Leoc.show_code 0 >> print_endline);
	maybe print_endline "\nsimplified LeoC:\n";
	let scode = ccode |> Optim.optimize |> Leoc.simp_code in 
	scode |> maybe (Leoc.show_code 0 >> print_endline);
	maybe print_endline "\nnoisy LeoC:\n";
	let noisy_ccode = Noise.add_noise (Leoc.Trash trash :: scode) in
	maybe (Leoc.show_code 0 >> print_endline) noisy_ccode;
	maybe print_endline "\nasm:\n";
	let bytecode = noisy_ccode |> Leoc.compile |> Triasm.process quiet in
	bc_handler bytecode;;

let end_tokens = [Tokens.Leol; Tokens.Leof; Tokens.Lrem]

let main () =
	if Array.length Sys.argv < 2 then Printf.printf "Usage: leoc <program.leo> [-v] [-q] [-tr] [-bc bytecode_file]" else
	begin 
		verbose := Array.mem "-v" Sys.argv;
		let quiet = Array.mem "-q" Sys.argv in
		let trash = Array.mem "-tr" Sys.argv in
		let make_bc = try 
				let i = Array.findi ((=) "-bc") Sys.argv in
				if i+1 < Array.length Sys.argv then Some(Sys.argv.(i+1)) else None
	  	with Not_found -> None in		
		let bc_dump = Array.mem "-bcdump" Sys.argv in
		let bc_handler =
			match make_bc with
			| Some fname -> (fun bytecode ->				
					let f = open_out_bin fname in 
					List.iter (write_le f) bytecode;
					close_out f)
			| None -> if bc_dump then (fun bytecode -> 
					Printf.printf "bytecode:\n[";
					List.iter (Printf.printf "%d, ") bytecode;
					Printf.printf "]\n") else (fun bytecode -> ()) in
  	let prog_text = Std.input_file Sys.argv.(1) in
		let tokens =
			let lbuf = Lexing.from_string prog_text in
			let rec loop res =
				let token = Leolex.lexer lbuf in
				let res' = token::res in
				if token = Tokens.Leof then List.rev res' else loop res' in
			loop [] in				 
    match Parse.parse_program tokens with
		| Parsercomb.Parsed(ast, unparsed) when 
				List.for_all (Parse.get0 >> flip List.mem end_tokens) unparsed 	
			->  process ast bc_handler quiet trash
		| Parsercomb.Parsed(ast, unparsed) -> 
				List.take 30 unparsed |> List.map (Parse.get0 >> Tokens.show_tok) |> String.concat " " 
				|>	Printf.printf "Parsing problem near: %s\n" 
		| Parsercomb.Failed -> print_endline "parsing failed"
  end;; 
         
(*Noise.test_shape 20;;
Noise.test_shape 20;;
Noise.test_shape 20;;
Noise.test_shape 30;;
Noise.test_shape 30;;
Noise.test_shape 30;;
Noise.test_shape 10;;
Noise.test_shape 10;;
Noise.test_shape 10;;
Noise.test_shape 4;;				
Noise.test_shape 4;;				
Noise.test_shape 4;;*)				
main();;