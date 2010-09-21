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
}

rule lexer = parse
   [' ' '\t']            { lexer lexbuf }
 | '\n'                  { Leol }
 | '|'                   { Lpipe }
 | '='                   { Leq }
 | '%'                   { Lmod }
 | '^'                   { Lxor } 
 | '+'                   { Lplus }
 | '-'                   { Lminus }
 | '*'                   { Lmul }
 | '/'                   { Ldiv }
 | '<'                   { Llt }
 | '>'                   { Lgt }
 | "<="                  { Lleq }
 | ">="                  { Lgeq }
 | "!="                  { Lneq }
 | "<-"                  { Lwrite }
 | "->"									 { Lfollow }
 | '('									 { Llparen }
 | ')'									 { Lrparen }
 | "&&"                  { Land }
 | "||"                  { Lor }
 | ".."                  { Ldot2 }
 | '.'                   { Ldot }
 | '{'                   { Llcurly }
 | '}'                   { Lrcurly }
 | '#' [^ '\n']*         { Lrem }
 | '"' [^ '"'] * '"'     { mkstring(Lexing.lexeme lexbuf) } 
 | "do"                  { Ldo }
 | "end"                 { Lend }
 | "for"                 { Lfor }
 | "in"                  { Lin }
 | "if"                  { Lif }
 | "then"                { Lthen }
 | "else"                { Lelse }
 | "return"              { Lreturn } 
 | "while"               { Lwhile } 
 | ".head"               { Lhead }
 | ".tail"               { Ltail }
 | ".range"              { Lrange }
 | "new"                 { Lnew }
 | "byte"                { Lbyte }
 | "int"                 { Ltint }
 | "type"                { Ltype }
 | "$trash"              { Ltrash }
 | "$notrash"            { Lnotrash } 
 | ','                   { Lcomma }
 | '\\'                  { Lbackslash }
 | ':'									 { Lcolon }
 | ';'									 { Lsemi }  
 | '?'								   { Lquestion }
 | '['                   { Llbracket }
 | ']'                   { Lrbracket }

 | "0x" ['0'-'9' 'A'-'F' 'a'-'f']* { Lint(hex (Lexing.lexeme lexbuf)) }
 | ['0'-'9']+            { Lint (int_of_string (Lexing.lexeme lexbuf)) }
 | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*            { Lident (Lexing.lexeme lexbuf) }
 | eof                   { Leof }