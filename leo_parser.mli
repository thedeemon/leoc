type token =
  | Leol
  | Leof
  | Lpipe
  | Lcolon
  | Lsemi
  | Lquestion
  | Lasgn
  | Lmod
  | Lplus
  | Lminus
  | Lmul
  | Ldiv
  | Lxor
  | Llt
  | Lgt
  | Lleq
  | Lgeq
  | Leq
  | Lneq
  | Llparen
  | Lrparen
  | Land
  | Lor
  | Ldot2
  | Lcomma
  | Llcurly
  | Lrcurly
  | Lrem
  | Lwrite
  | Lfollow
  | Lbackslash
  | Llbracket
  | Lrbracket
  | Ldo
  | Lend
  | Lfor
  | Lin
  | Lif
  | Lthen
  | Lelse
  | Lreturn
  | Lhead
  | Ltail
  | Lbyte
  | Ltint
  | Lnew
  | Lint of (int)
  | Lident of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Leo.code
