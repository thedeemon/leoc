%{
open ExtLib;;
open Commons;;

let parse_error s =
  Printf.eprintf "Parse error: %s\n" s;
  flush stderr;; 

%}

%token Leol Leof
%token Lpipe Lcolon Lsemi Lquestion
%token Lasgn 
%token Lmod Lplus Lminus Lmul Ldiv Lxor
%token Llt Lgt Lleq Lgeq Leq Lneq 
%token Llparen Lrparen 
%token Land Lor 
%token Ldot2 Lcomma
%token Llcurly Lrcurly Llparen Lrparen
%token Lrem Lwrite Lfollow
%token Lbackslash Llbracket Lrbracket
%token Ldo Lend Lfor Lin Lif Lthen Lelse Lreturn Lhead Ltail Lbyte Ltint Lnew
%token<int> Lint
%token<string> Lident 

%right Lasgn 
%nonassoc Ldot2  
%left  Lor
%left  Land
%nonassoc  Leq Lneq 
%left  Lgt Lgeq Llt Lleq
%left  Lplus Lminus
%left  Lmul Ldiv Lmod

%start program
%type <Leo.code> program

%%

program       : stmts opt_terms Leof { log "program"; $1 }
							;

stmts         : stmt  { log "stmt"; [$1] }
              | stmts terms stmt { log "more stmt"; $1 @ [$3] }
							;
							
stmt          : Lident params_opt Leq expr { log "Def"; Leo.Def($1, $2, $4) }
							| lvalue Lwrite expr { log "Write"; Leo.Write($1, $3) }
							| Lfor name_seq_list opt_terms stmts opt_terms Lend {  log "For"; Leo.For($2, $4) }
							| Lreturn expr { log "Ret"; Leo.Ret $2 }
              | expr { log "Expr_as_stmt"; Leo.Expr $1 }
							;
					
params_opt    : { [] }
							| Llparen params Lrparen { $2 }
							;
							
params        : Lident { [$1] }
							| Lident Lcomma params { $1 :: $3 }
							;
							
lvalue        : Lident { log "LVar"; Leo.LVar $1 }
              | Lident Llbracket expr Lrbracket { log "LArr"; Leo.LArr($1, $3) }
							| Lident Llbracket seq Lrbracket { log "LSubArr"; Leo.LSubArr($1, $3) }
							;						
								
name_seq_list :	name_seq { [$1] }
              | name_seq_list Lcomma name_seq { $1 @ [ $3 ] }							
							;	
							
name_seq      : Lident Lin seq { log "Name_seq"; ($1, $3) }
							;
														
seq           :	Lident { log "SVal"; Leo.SVal $1 }
							| expr Ldot2 expr { log "SRange"; Leo.SRange($1, $3) }						
							;
							
expr          : Lint   { log "Int"; Leo.Val $1 }
							| Lident   { log "Id"; Leo.Var $1 }
							| Lif condition then_ expr if_tail   { log "If"; Leo.If($2, $4, $5) } 
							| Ldo opt_terms stmts opt_terms Lend   { log "Comp"; Leo.Comp $3 }
							| Llcurly opt_terms stmts opt_terms Lrcurly   { log "Comp"; Leo.Comp $3 }
							| Lident Llparen args Lrparen { if $1="print" then ( log "Print"; Leo.Comp [Leo.Print(List.hd $3)] ) 
																							else ( log "Call"; Leo.Call($1, $3) ) }
							| Lident Llbracket expr Lrbracket { log "Arr"; Leo.Arr($1, $3) }
							| Lident Llbracket seq Lrbracket { log "SubArr"; Leo.SubArr($1, $3) }
							| Lpipe expr Lpipe { log "Length"; Leo.Length $2 }
							| expr Lhead { log "Head"; Leo.Head $1 }
							| expr Ltail { log "Tail"; Leo.Tail $1 }
							| arith { log "Arith"; $1 } 
							| Lnew atype Llbracket expr Lrbracket { log "New"; Leo.New($2, $4) }
							| Lbackslash params Lfollow expr { log "Lambda"; Leo.Lambda($2, $4) }
							; 							

arith         : expr Lplus expr   { Leo.Arith(Add, $1, $3) }
							| expr Lminus expr   { Leo.Arith(Sub, $1, $3) }
							| expr Lmul expr   { Leo.Arith(Mul, $1, $3) }
							| expr Ldiv expr   { Leo.Arith(Div, $1, $3) }
							| expr Lmod expr   { Leo.Arith(Mod, $1, $3) }
							| expr Lxor expr  { Leo.Arith(Xor, $1, $3) }
							| expr Ldot2 expr { log "Range_as_expr"; Leo.Seq(Leo.SRange($1, $3)) }
							| Llparen arith Lrparen   {  log "expr_in_parens"; $2 }
							;

condition     : expr Llt expr   { Leo.Less($1, $3) }
							| expr Lgt expr   { Leo.Less($3, $1) }
							| expr Leq expr   { Leo.Eq($1, $3) } 						
							| expr Lneq expr   { Leo.Not(Leo.Eq($1, $3)) }
							| condition Land condition { Leo.And($1, $3) }
							| condition Lor condition { Leo.Or($1, $3) }
							;

args          :  { log "empty_args"; [] }
              | expr { log "expr_as_Arg"; [ $1 ] }
							| expr Lcomma args { log "More arg"; $1 :: $3 }
							;							
																					
then_         : Lthen { }
              | eol { }
 							;							
							
if_tail       : { None }
              | Lelse expr	{ Some $2 }
							;		
							
atype         : Lbyte { Leo.AByte }
              | Ltint { Leo.AInt }
							;
							
term          : Lsemi { }
              | eol { }
							;
							
eol           : Leol { }
							| Lrem Leol { }
							;

terms         : term { }
              | terms term	{ }					
							;

opt_terms     : /* none */ { } 
							| terms { }
							;
%%