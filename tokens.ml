type token =
  | Leol  | Leof  | Lpipe  | Lcolon  | Lsemi  | Lquestion  | Lmod  | Lplus  | Lminus  | Lmul  | Ldiv  | Lxor
  | Llt  | Lgt  | Lleq  | Lgeq  | Leq  | Lneq  | Llparen  | Lrparen
  | Land  | Lor  | Ldot | Ldot2  | Lcomma  | Llcurly  | Lrcurly
  | Lrem  | Lwrite  | Lfollow  | Lbackslash  | Llbracket  | Lrbracket
  | Ldo  | Lend  | Lfor  | Lin  | Lif  | Lthen  | Lelse  | Lreturn  | Lhead  | Ltail	| Lrange  
	| Lbyte  | Ltint  | Lnew	| Ltype
  | Lint of (int)
  | Lident of (string);;

let show_tok = function
	| Leol -> "eol"  | Leof -> "eof"  | Lpipe -> "|"  | Lcolon -> ":"  | Lsemi -> ";"  | Lquestion -> "?"
  | Lmod -> "%"  | Lplus -> "+"  | Lminus -> "-"  | Lmul -> "*"  | Ldiv -> "/"  | Lxor -> "^"  | Llt -> "<"
  | Lgt -> ">"  | Lleq -> "<="  | Lgeq -> ">="  | Leq -> "="  | Lneq -> "!="  | Llparen -> "("  | Lrparen -> ")"
  | Land -> "&&"  | Lor -> "||"  | Ldot -> "." | Ldot2 -> ".."  | Lcomma -> ","  | Llcurly -> "{"  | Lrcurly -> "}"
  | Lrem -> "rem"  | Lwrite -> "<-"  | Lfollow -> "->"  | Lbackslash -> "\\"  | Llbracket -> "["  | Lrbracket -> "]"
  | Ldo -> "do"  | Lend -> "end"  | Lfor -> "for"  | Lin -> "in"  | Lif -> "if"  | Lthen -> "then"  | Lelse -> "else"
  | Lreturn -> "return"  | Lhead -> ".head"  | Ltail -> ".tail"	| Lrange -> ".range"  | Lbyte -> "byte"
	| Ltype -> "type"  | Ltint -> "int"  | Lnew -> "new"  | Lint i -> string_of_int i  | Lident s -> s 
  ;;