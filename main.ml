open ExtLib;;
open Commons;;

let echolexer lbuf =
	let l = Leolex.lexer lbuf in Printf.eprintf "[%s] " (Lexing.lexeme lbuf); flush stderr; l;;

(*let prog_ast_of_string str =
	let lbuf = Lexing.from_string str in
	Leo_parser.program echolexer (*Leolex.lexer*) lbuf;;*)

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
				Leo.process ast true
		| Parsercomb.Failed -> print_endline "parsing failed"
  end;; 
         
main();;