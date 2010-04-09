open ExtLib;;
open Commons;;

let echolexer lbuf =
	let l = Leolex.lexer lbuf in Printf.eprintf "[%s] " (Lexing.lexeme lbuf); flush stderr; l;;

(*let prog_ast_of_string str =
	let lbuf = Lexing.from_string str in
	Leo_parser.program echolexer (*Leolex.lexer*) lbuf;;*)

let process prg show =
	let maybe = if show then (fun f x -> f x) else (fun f x -> ()) in 
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
	let noisy_ccode = Noise.add_noise scode in
	maybe (Leoc.show_code 0 >> print_endline) noisy_ccode;
	maybe print_endline "\nasm:\n";
	noisy_ccode |> Leoc.compile |> Triasm.process;;

let main () =
	if Array.length Sys.argv < 2 then Printf.printf "Usage: %s <program.leo>" Sys.argv.(0) else
	begin 
  	let prog_text = Std.input_file Sys.argv.(1) in
		let tokens =
			let lbuf = Lexing.from_string prog_text in
			let rec loop res =
				let token = Leolex.lexer lbuf in
				let res' = token::res in
				if token = Tokens.Leof then List.rev res' else loop res' in
			loop [] in				 
    match Parse.parse_program tokens with
		| Parsercomb.Parsed(ast, cs) -> 
				(*cs |> String.implode |> print_endline*) 
				process ast true
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