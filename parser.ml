type token =
  | LAMBDA
  | TRUE
  | FALSE
  | IF
  | THEN
  | ELSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | LETREC
  | IN
  | CONCAT
  | BOOL
  | NAT
  | STRING
  | UNITV
  | LBRACKET
  | RBRACKET
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LIST
  | NIL
  | CONS
  | ISNIL
  | HEAD
  | TAIL
  | COMMA
  | SEMICOLON
  | DOT
  | EQ
  | COLON
  | ARROW
  | EOF
  | QM
  | INTV of (int)
  | IDV of (string)
  | STRINGV of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 48 "parser.ml"
let yytransl_const = [|
  257 (* LAMBDA *);
  258 (* TRUE *);
  259 (* FALSE *);
  260 (* IF *);
  261 (* THEN *);
  262 (* ELSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* LET *);
  267 (* LETREC *);
  268 (* IN *);
  269 (* CONCAT *);
  270 (* BOOL *);
  271 (* NAT *);
  272 (* STRING *);
  273 (* UNITV *);
  274 (* LBRACKET *);
  275 (* RBRACKET *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* LBRACE *);
  279 (* RBRACE *);
  280 (* LIST *);
  281 (* NIL *);
  282 (* CONS *);
  283 (* ISNIL *);
  284 (* HEAD *);
  285 (* TAIL *);
  286 (* COMMA *);
  287 (* SEMICOLON *);
  288 (* DOT *);
  289 (* EQ *);
  290 (* COLON *);
  291 (* ARROW *);
    0 (* EOF *);
  292 (* QM *);
    0|]

let yytransl_block = [|
  293 (* INTV *);
  294 (* IDV *);
  295 (* STRINGV *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\006\000\005\000\005\000\005\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\008\000\
\008\000\009\000\009\000\010\000\010\000\004\000\004\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\013\000\013\000\014\000\014\000\015\000\015\000\016\000\016\000\
\000\000"

let yylen = "\002\000\
\002\000\004\000\001\000\006\000\006\000\004\000\006\000\008\000\
\001\000\002\000\002\000\002\000\003\000\002\000\003\000\008\000\
\005\000\005\000\004\000\001\000\003\000\003\000\001\000\003\000\
\001\000\001\000\001\000\001\000\001\000\003\000\003\000\001\000\
\003\000\000\000\001\000\003\000\005\000\001\000\003\000\003\000\
\001\000\001\000\001\000\003\000\003\000\004\000\001\000\003\000\
\000\000\001\000\003\000\005\000\001\000\003\000\003\000\005\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\028\000\000\000\029\000\057\000\000\000\
\000\000\000\000\023\000\000\000\027\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\000\000\000\000\030\000\031\000\041\000\042\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\000\000\021\000\022\000\006\000\000\000\000\000\
\000\000\000\000\000\000\033\000\000\000\000\000\000\000\000\000\
\000\000\050\000\000\000\019\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\040\000\000\000\
\000\000\044\000\045\000\000\000\039\000\000\000\000\000\000\000\
\005\000\004\000\007\000\000\000\000\000\037\000\000\000\048\000\
\046\000\000\000\000\000\000\000\000\000\008\000\052\000\016\000"

let yydgoto = "\002\000\
\023\000\039\000\025\000\087\000\026\000\000\000\027\000\040\000\
\041\000\042\000\070\000\088\000\089\000\090\000\000\000\000\000"

let yysindex = "\009\000\
\103\255\000\000\236\254\000\000\000\000\142\255\057\255\057\255\
\057\255\240\254\241\254\057\255\142\255\181\255\007\255\011\255\
\012\255\016\255\252\254\000\000\004\255\000\000\000\000\038\000\
\057\255\008\255\000\000\233\254\000\000\034\255\008\255\008\255\
\008\255\009\255\014\255\030\255\020\255\010\255\015\255\023\255\
\032\255\000\000\224\255\224\255\224\255\224\255\021\255\142\255\
\000\000\008\255\246\254\142\255\224\255\142\255\142\255\224\255\
\008\255\000\000\142\255\142\255\000\000\000\000\000\000\000\000\
\000\000\224\255\213\255\038\255\045\255\031\255\036\255\046\255\
\053\255\000\000\076\000\000\000\000\000\000\000\048\255\072\255\
\069\255\050\255\058\255\000\000\080\255\059\255\073\255\079\255\
\085\255\000\000\224\255\000\000\224\255\057\255\057\255\057\255\
\000\000\142\255\142\255\142\255\142\255\071\255\000\000\224\255\
\224\255\000\000\000\000\096\255\000\000\243\254\008\255\008\255\
\000\000\000\000\000\000\106\255\010\255\000\000\089\255\000\000\
\000\000\057\255\142\255\082\255\245\254\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\098\255\000\000\000\000\
\000\000\000\000\000\000\000\000\159\000\000\000\000\000\000\000\
\165\000\001\000\000\000\000\000\000\000\000\000\006\000\044\000\
\049\000\000\000\000\000\000\000\000\000\193\255\101\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\087\000\000\000\000\000\000\000\000\000\000\000\000\000\
\092\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\104\255\000\000\000\000\052\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\110\255\000\000\000\000\000\000\111\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\130\000\135\000\
\000\000\000\000\000\000\000\000\000\000\000\000\113\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\226\255\251\255\000\000\000\000\066\000\
\000\000\035\000\000\000\033\000\000\000\023\000\000\000\000\000"

let yytablesize = 454
let yytable = "\024\000\
\009\000\031\000\032\000\033\000\030\000\010\000\036\000\128\000\
\052\000\001\000\053\000\037\000\069\000\071\000\072\000\073\000\
\122\000\028\000\051\000\050\000\051\000\034\000\079\000\035\000\
\043\000\082\000\076\000\077\000\044\000\045\000\057\000\004\000\
\005\000\046\000\047\000\085\000\048\000\049\000\054\000\051\000\
\058\000\055\000\059\000\011\000\060\000\061\000\075\000\056\000\
\012\000\013\000\078\000\014\000\080\000\081\000\062\000\091\000\
\074\000\083\000\004\000\005\000\108\000\051\000\109\000\092\000\
\095\000\093\000\020\000\029\000\022\000\094\000\038\000\096\000\
\038\000\119\000\038\000\097\000\013\000\099\000\014\000\098\000\
\100\000\038\000\101\000\038\000\038\000\038\000\014\000\102\000\
\110\000\111\000\112\000\013\000\104\000\020\000\029\000\022\000\
\113\000\114\000\115\000\116\000\103\000\106\000\105\000\003\000\
\004\000\005\000\006\000\107\000\117\000\007\000\008\000\009\000\
\010\000\011\000\121\000\012\000\125\000\123\000\124\000\086\000\
\034\000\126\000\013\000\032\000\014\000\084\000\049\000\015\000\
\016\000\017\000\017\000\018\000\036\000\047\000\018\000\051\000\
\118\000\120\000\019\000\020\000\021\000\022\000\003\000\004\000\
\005\000\006\000\127\000\000\000\007\000\008\000\009\000\010\000\
\011\000\000\000\012\000\000\000\000\000\000\000\027\000\000\000\
\000\000\013\000\000\000\014\000\003\000\000\000\015\000\016\000\
\000\000\017\000\018\000\000\000\000\000\000\000\000\000\000\000\
\000\000\019\000\020\000\029\000\022\000\003\000\004\000\005\000\
\006\000\000\000\000\000\007\000\008\000\009\000\010\000\011\000\
\000\000\012\000\027\000\027\000\000\000\000\000\000\000\000\000\
\013\000\000\000\014\000\000\000\000\000\015\000\016\000\000\000\
\017\000\018\000\000\000\000\000\027\000\000\000\027\000\027\000\
\019\000\020\000\038\000\022\000\000\000\000\000\027\000\000\000\
\027\000\000\000\063\000\064\000\065\000\027\000\027\000\027\000\
\066\000\000\000\067\000\000\000\068\000\063\000\064\000\065\000\
\000\000\000\000\000\000\066\000\000\000\067\000\000\000\068\000\
\000\000\000\000\086\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\009\000\000\000\009\000\009\000\010\000\
\010\000\000\000\010\000\010\000\009\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\009\000\009\000\009\000\009\000\
\000\000\010\000\010\000\010\000\010\000\000\000\009\000\000\000\
\000\000\000\000\000\000\010\000\000\000\009\000\009\000\009\000\
\000\000\000\000\010\000\010\000\010\000\011\000\011\000\000\000\
\011\000\011\000\012\000\012\000\000\000\012\000\012\000\011\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\011\000\
\011\000\011\000\011\000\000\000\012\000\012\000\012\000\012\000\
\000\000\011\000\000\000\000\000\000\000\000\000\012\000\000\000\
\011\000\011\000\011\000\000\000\000\000\012\000\012\000\012\000\
\014\000\014\000\000\000\014\000\014\000\013\000\013\000\000\000\
\013\000\013\000\014\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\014\000\014\000\014\000\014\000\000\000\013\000\
\013\000\013\000\013\000\000\000\014\000\000\000\000\000\000\000\
\000\000\013\000\000\000\014\000\014\000\014\000\000\000\000\000\
\013\000\013\000\013\000\017\000\017\000\000\000\017\000\017\000\
\018\000\018\000\000\000\018\000\018\000\017\000\000\000\000\000\
\000\000\000\000\018\000\000\000\000\000\017\000\017\000\017\000\
\017\000\000\000\018\000\018\000\018\000\018\000\000\000\017\000\
\027\000\027\000\000\000\000\000\018\000\000\000\017\000\017\000\
\017\000\003\000\003\000\018\000\018\000\018\000\000\000\000\000\
\003\000\000\000\027\000\000\000\027\000\000\000\000\000\000\000\
\000\000\003\000\000\000\003\000\000\000\000\000\027\000\000\000\
\000\000\000\000\003\000\027\000\027\000\027\000"

let yycheck = "\001\000\
\000\000\007\000\008\000\009\000\006\000\000\000\012\000\019\001\
\032\001\001\000\034\001\013\000\043\000\044\000\045\000\046\000\
\030\001\038\001\032\001\025\000\032\001\038\001\053\000\039\001\
\018\001\056\000\037\001\038\001\018\001\018\001\036\000\002\001\
\003\001\018\001\039\001\066\000\033\001\000\000\005\001\032\001\
\021\001\033\001\033\001\000\000\030\001\023\001\048\000\034\001\
\000\000\020\001\052\000\022\001\054\000\055\000\023\001\018\001\
\036\001\059\000\002\001\003\001\091\000\032\001\093\000\019\001\
\019\001\035\001\037\001\038\001\039\001\034\001\019\001\019\001\
\021\001\104\000\023\001\000\000\020\001\006\001\022\001\032\001\
\012\001\030\001\033\001\032\001\033\001\034\001\000\000\030\001\
\094\000\095\000\096\000\000\000\034\001\037\001\038\001\039\001\
\098\000\099\000\100\000\101\000\021\001\023\001\030\001\001\001\
\002\001\003\001\004\001\023\001\038\001\007\001\008\001\009\001\
\010\001\011\001\019\001\013\001\122\000\012\001\030\001\038\001\
\023\001\123\000\020\001\023\001\022\001\060\000\023\001\025\001\
\026\001\000\000\028\001\029\001\023\001\023\001\000\000\023\001\
\102\000\105\000\036\001\037\001\038\001\039\001\001\001\002\001\
\003\001\004\001\124\000\255\255\007\001\008\001\009\001\010\001\
\011\001\255\255\013\001\255\255\255\255\255\255\000\000\255\255\
\255\255\020\001\255\255\022\001\000\000\255\255\025\001\026\001\
\255\255\028\001\029\001\255\255\255\255\255\255\255\255\255\255\
\255\255\036\001\037\001\038\001\039\001\001\001\002\001\003\001\
\004\001\255\255\255\255\007\001\008\001\009\001\010\001\011\001\
\255\255\013\001\002\001\003\001\255\255\255\255\255\255\255\255\
\020\001\255\255\022\001\255\255\255\255\025\001\026\001\255\255\
\028\001\029\001\255\255\255\255\020\001\255\255\022\001\023\001\
\036\001\037\001\038\001\039\001\255\255\255\255\030\001\255\255\
\032\001\255\255\014\001\015\001\016\001\037\001\038\001\039\001\
\020\001\255\255\022\001\255\255\024\001\014\001\015\001\016\001\
\255\255\255\255\255\255\020\001\255\255\022\001\255\255\024\001\
\255\255\255\255\038\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\002\001\
\003\001\255\255\005\001\006\001\012\001\255\255\255\255\255\255\
\255\255\012\001\255\255\255\255\020\001\021\001\022\001\023\001\
\255\255\020\001\021\001\022\001\023\001\255\255\030\001\255\255\
\255\255\255\255\255\255\030\001\255\255\037\001\038\001\039\001\
\255\255\255\255\037\001\038\001\039\001\002\001\003\001\255\255\
\005\001\006\001\002\001\003\001\255\255\005\001\006\001\012\001\
\255\255\255\255\255\255\255\255\012\001\255\255\255\255\020\001\
\021\001\022\001\023\001\255\255\020\001\021\001\022\001\023\001\
\255\255\030\001\255\255\255\255\255\255\255\255\030\001\255\255\
\037\001\038\001\039\001\255\255\255\255\037\001\038\001\039\001\
\002\001\003\001\255\255\005\001\006\001\002\001\003\001\255\255\
\005\001\006\001\012\001\255\255\255\255\255\255\255\255\012\001\
\255\255\255\255\020\001\021\001\022\001\023\001\255\255\020\001\
\021\001\022\001\023\001\255\255\030\001\255\255\255\255\255\255\
\255\255\030\001\255\255\037\001\038\001\039\001\255\255\255\255\
\037\001\038\001\039\001\002\001\003\001\255\255\005\001\006\001\
\002\001\003\001\255\255\005\001\006\001\012\001\255\255\255\255\
\255\255\255\255\012\001\255\255\255\255\020\001\021\001\022\001\
\023\001\255\255\020\001\021\001\022\001\023\001\255\255\030\001\
\002\001\003\001\255\255\255\255\030\001\255\255\037\001\038\001\
\039\001\005\001\006\001\037\001\038\001\039\001\255\255\255\255\
\012\001\255\255\020\001\255\255\022\001\255\255\255\255\255\255\
\255\255\021\001\255\255\023\001\255\255\255\255\032\001\255\255\
\255\255\255\255\030\001\037\001\038\001\039\001"

let yynames_const = "\
  LAMBDA\000\
  TRUE\000\
  FALSE\000\
  IF\000\
  THEN\000\
  ELSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  LET\000\
  LETREC\000\
  IN\000\
  CONCAT\000\
  BOOL\000\
  NAT\000\
  STRING\000\
  UNITV\000\
  LBRACKET\000\
  RBRACKET\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LIST\000\
  NIL\000\
  CONS\000\
  ISNIL\000\
  HEAD\000\
  TAIL\000\
  COMMA\000\
  SEMICOLON\000\
  DOT\000\
  EQ\000\
  COLON\000\
  ARROW\000\
  EOF\000\
  QM\000\
  "

let yynames_block = "\
  INTV\000\
  IDV\000\
  STRINGV\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 56 "parser.mly"
      ( Eval _1 )
# 349 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 58 "parser.mly"
      ( Bind (_1, _3) )
# 357 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 62 "parser.mly"
      ( _1 )
# 364 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 64 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 373 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 66 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 382 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 68 "parser.mly"
      ( TmAbs (_2, TyNat, _4) )
# 390 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 70 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 399 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 72 "parser.mly"
      ( TmLetRec (_2, _4, _6, _8) )
# 409 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 78 "parser.mly"
      ( _1 )
# 416 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 80 "parser.mly"
      ( TmSucc _2 )
# 423 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 82 "parser.mly"
      ( TmPred _2 )
# 430 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 84 "parser.mly"
      ( TmIsZero _2 )
# 437 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'projectionTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 86 "parser.mly"
      ( TmConcat (_2, _3) )
# 445 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 88 "parser.mly"
      ( TmApp (_1, _2) )
# 453 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 90 "parser.mly"
      ( TmString _2 )
# 460 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 3 : 'projectionTerm) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'projectionTerm) in
    Obj.repr(
# 92 "parser.mly"
      ( TmCons (_3, _5, _7) )
# 469 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 94 "parser.mly"
      ( TmHead (_3, _5) )
# 477 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'projectionTerm) in
    Obj.repr(
# 96 "parser.mly"
      ( TmTail (_3, _5) )
# 485 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 98 "parser.mly"
      ( TmNil (_3) )
# 492 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 106 "parser.mly"
      ( _1 )
# 499 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'projectionTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 111 "parser.mly"
      ( TmProjection (_1, (string_of_int _3)) )
# 507 "parser.ml"
               : 'projectionTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'projectionTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "parser.mly"
      ( TmProjection (_1, _3) )
# 515 "parser.ml"
               : 'projectionTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 115 "parser.mly"
      ( _1 )
# 522 "parser.ml"
               : 'projectionTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 119 "parser.mly"
      ( _2 )
# 529 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
      ( TmTrue )
# 535 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
      ( TmFalse )
# 541 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "parser.mly"
      ( TmVar _1 )
# 548 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 127 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 558 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "parser.mly"
      ( TmString _1 )
# 565 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuple) in
    Obj.repr(
# 134 "parser.mly"
      ( TmTuple _2 )
# 572 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'record) in
    Obj.repr(
# 136 "parser.mly"
      ( TmRecord _2 )
# 579 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 139 "parser.mly"
         ( [_1] )
# 586 "parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 140 "parser.mly"
                     ( _1::_3 )
# 594 "parser.ml"
               : 'tuple))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
    ( [] )
# 600 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'noEmptyRecord) in
    Obj.repr(
# 144 "parser.mly"
                  ( _1 )
# 607 "parser.ml"
               : 'record))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 147 "parser.mly"
                ( [_1,_3] )
# 615 "parser.ml"
               : 'noEmptyRecord))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'noEmptyRecord) in
    Obj.repr(
# 148 "parser.mly"
                                    ( (_1,_3)::_5 )
# 624 "parser.ml"
               : 'noEmptyRecord))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 152 "parser.mly"
      ( _1 )
# 631 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 154 "parser.mly"
      ( TyArr (_1, _3) )
# 639 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 158 "parser.mly"
      ( _2 )
# 646 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 160 "parser.mly"
      ( TyBool )
# 652 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 162 "parser.mly"
      ( TyNat )
# 658 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 164 "parser.mly"
      ( TyString )
# 664 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tupleTy) in
    Obj.repr(
# 166 "parser.mly"
      ( TyTuple _2 )
# 671 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'recordTy) in
    Obj.repr(
# 168 "parser.mly"
      ( TyRecord _2 )
# 678 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 170 "parser.mly"
      ( TyList _3 )
# 685 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 173 "parser.mly"
         ( [_1] )
# 692 "parser.ml"
               : 'tupleTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'tupleTy) in
    Obj.repr(
# 174 "parser.mly"
                       ( _1::_3 )
# 700 "parser.ml"
               : 'tupleTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 177 "parser.mly"
      ( [] )
# 706 "parser.ml"
               : 'recordTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'noEmptyRecordTy) in
    Obj.repr(
# 178 "parser.mly"
                      ( _1 )
# 713 "parser.ml"
               : 'recordTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 181 "parser.mly"
                   ( [_1,_3] )
# 721 "parser.ml"
               : 'noEmptyRecordTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'noEmptyRecordTy) in
    Obj.repr(
# 182 "parser.mly"
                                         ( (_1,_3)::_5 )
# 730 "parser.ml"
               : 'noEmptyRecordTy))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 185 "parser.mly"
         ( [_1] )
# 737 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 186 "parser.mly"
                   ( _1::_3 )
# 745 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 189 "parser.mly"
                 ( [_1,_3] )
# 753 "parser.ml"
               : 'noEmptyVar))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'noEmptyVar) in
    Obj.repr(
# 190 "parser.mly"
                                  ( (_1,_3)::_5 )
# 762 "parser.ml"
               : 'noEmptyVar))
(* Entry s *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let s (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lambda.command)
