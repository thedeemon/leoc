{
	open Tokens;;
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
 | "do"                  { Ldo }
 | "end"                 { Lend }
 | "for"                 { Lfor }
 | "in"                  { Lin }
 | "if"                  { Lif }
 | "then"                { Lthen }
 | "else"                { Lelse }
 | "return"              { Lreturn } 
 | ".head"               { Lhead }
 | ".tail"               { Ltail }
 | ".range"              { Lrange }
 | "new"                 { Lnew }
 | "byte"                { Lbyte }
 | "int"                 { Ltint }
 | "type"                { Ltype }
 | ','                   { Lcomma }
 | '\\'                  { Lbackslash }
 | ':'									 { Lcolon }
 | ';'									 { Lsemi }  
 | '?'								   { Lquestion }
 | '['                   { Llbracket }
 | ']'                   { Lrbracket }

 | ['0'-'9']+            { Lint (int_of_string (Lexing.lexeme lexbuf)) }
 | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*            { Lident (Lexing.lexeme lexbuf) }
 | eof                   { Leof }