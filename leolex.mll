{
	open Tokens
	open ExtLib

	let mkstring s = 
		let s1 = String.sub s 1 (String.length s - 2) in
		let rec loop s = 
			let r, s' = String.replace ~str:s ~sub:"\\n" ~by:"\n" in
			if r then loop s' else s in
		Lstring (loop s1);;

	let hex s =
		let s = String.sub s 2 (String.length s - 2) in
		Scanf.sscanf s "%x" Std.identity;;

	let getpos : Lexing.lexbuf -> Commons.source_loc = fun lb -> lb.Lexing.lex_curr_p.Lexing.pos_lnum;;

	module L = Lexing;;
}

rule lexer = parse
   [' ' '\t']            { lexer lexbuf }
 | '\n'                  { lexbuf.L.lex_curr_p <- {lexbuf.L.lex_curr_p with L.pos_lnum = lexbuf.L.lex_curr_p.L.pos_lnum+1}; Leol, getpos lexbuf }
 | '|'                   { Lpipe, getpos lexbuf }
 | '='                   { Leq, getpos lexbuf }
 | '%'                   { Lmod, getpos lexbuf }
 | '^'                   { Lxor, getpos lexbuf } 
 | '+'                   { Lplus, getpos lexbuf }
 | '-'                   { Lminus, getpos lexbuf }
 | '*'                   { Lmul, getpos lexbuf }
 | '/'                   { Ldiv, getpos lexbuf }
 | '<'                   { Llt, getpos lexbuf }
 | '>'                   { Lgt, getpos lexbuf }
 | "<="                  { Lleq, getpos lexbuf }
 | ">="                  { Lgeq, getpos lexbuf }
 | "!="                  { Lneq, getpos lexbuf }
 | "<-"                  { Lwrite, getpos lexbuf }
 | "->"									 { Lfollow, getpos lexbuf }
 | '('									 { Llparen, getpos lexbuf }
 | ')'									 { Lrparen, getpos lexbuf }
 | "&&"                  { Land, getpos lexbuf }
 | "||"                  { Lor, getpos lexbuf }
 | ".."                  { Ldot2, getpos lexbuf }
 | '.'                   { Ldot, getpos lexbuf }
 | '{'                   { Llcurly, getpos lexbuf }
 | '}'                   { Lrcurly, getpos lexbuf }
 | '#' [^ '\n']*         { Lrem, getpos lexbuf }
 | '"' [^ '"'] * '"'     { mkstring(Lexing.lexeme lexbuf), getpos lexbuf } 
 | "do"                  { Ldo, getpos lexbuf }
 | "end"                 { Lend, getpos lexbuf }
 | "for"                 { Lfor, getpos lexbuf }
 | "in"                  { Lin, getpos lexbuf }
 | "if"                  { Lif, getpos lexbuf }
 | "then"                { Lthen, getpos lexbuf }
 | "else"                { Lelse, getpos lexbuf }
 | "return"              { Lreturn, getpos lexbuf } 
 | "while"               { Lwhile, getpos lexbuf } 
 | ".head"               { Lhead, getpos lexbuf }
 | ".tail"               { Ltail, getpos lexbuf }
 | ".range"              { Lrange, getpos lexbuf }
 | "new"                 { Lnew, getpos lexbuf }
 | "byte"                { Lbyte, getpos lexbuf }
 | "int"                 { Ltint, getpos lexbuf }
 | "int32"               { Ltint32, getpos lexbuf }
 | "type"                { Ltype, getpos lexbuf }
 | "$trash"              { Ltrash, getpos lexbuf }
 | "$notrash"            { Lnotrash, getpos lexbuf } 
 | ','                   { Lcomma, getpos lexbuf }
 | '\\'                  { Lbackslash, getpos lexbuf }
 | ':'									 { Lcolon, getpos lexbuf }
 | ';'									 { Lsemi, getpos lexbuf }  
 | '?'								   { Lquestion, getpos lexbuf }
 | '['                   { Llbracket, getpos lexbuf }
 | ']'                   { Lrbracket, getpos lexbuf }

 | "0x" ['0'-'9' 'A'-'F' 'a'-'f']* { Lint(hex (Lexing.lexeme lexbuf)), getpos lexbuf }
 | ['0'-'9']+            { Lint (int_of_string (Lexing.lexeme lexbuf)), getpos lexbuf }
 | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*            { Lident (Lexing.lexeme lexbuf), getpos lexbuf }
 | eof                   { Leof, getpos lexbuf }