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
  | BOOL
  | NAT
  | STRING
  | UNIT
  | UNITV
  | LCOR
  | RCOR
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
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
  | CONCAT
  | ID of (string)
  | INTV of (int)
  | STRINGV of (string)
  | STRINGT of (string)

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
  open Lambda;;
# 50 "parser.ml"
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
  269 (* BOOL *);
  270 (* NAT *);
  271 (* STRING *);
  272 (* UNIT *);
  273 (* UNITV *);
  274 (* LCOR *);
  275 (* RCOR *);
  276 (* LPAREN *);
  277 (* RPAREN *);
  278 (* LBRACKET *);
  279 (* RBRACKET *);
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
  293 (* CONCAT *);
    0|]

let yytransl_block = [|
  294 (* ID *);
  295 (* INTV *);
  296 (* STRINGV *);
  297 (* STRINGT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\004\000\004\000\004\000\006\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\005\000\005\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\000\000"

let yylen = "\002\000\
\004\000\002\000\001\000\003\000\001\000\006\000\006\000\006\000\
\008\000\001\000\002\000\002\000\002\000\003\000\002\000\003\000\
\006\000\005\000\005\000\005\000\004\000\001\000\003\000\003\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\003\000\
\003\000\003\000\001\000\001\000\001\000\001\000\004\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\025\000\026\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\000\000\028\000\040\000\
\000\000\003\000\000\000\000\000\022\000\000\000\027\000\000\000\
\011\000\012\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\015\000\000\000\000\000\000\000\000\000\000\000\023\000\035\000\
\036\000\037\000\038\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\024\000\000\000\004\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\034\000\033\000\000\000\032\000\000\000\
\018\000\019\000\020\000\007\000\006\000\008\000\000\000\039\000\
\017\000\000\000\009\000"

let yydgoto = "\002\000\
\024\000\025\000\026\000\027\000\063\000\028\000\029\000\064\000"

let yysindex = "\004\000\
\095\255\000\000\220\254\000\000\000\000\136\255\013\255\013\255\
\013\255\233\254\234\254\000\000\136\255\006\255\007\255\008\255\
\009\255\018\255\236\254\254\254\000\000\004\255\000\000\000\000\
\002\000\000\000\013\255\001\255\000\000\010\255\000\000\035\255\
\000\000\000\000\000\000\012\255\015\255\021\255\043\255\043\255\
\043\255\043\255\043\255\024\255\136\255\136\255\136\255\000\000\
\000\000\013\255\043\255\136\255\136\255\043\255\000\000\000\000\
\000\000\000\000\000\000\043\255\043\255\044\255\045\255\030\255\
\047\255\049\255\050\255\051\255\000\000\000\000\003\000\000\000\
\000\000\039\255\066\255\061\255\041\255\056\255\055\255\043\255\
\000\000\043\255\013\255\013\255\013\255\013\255\000\000\136\255\
\136\255\136\255\136\255\000\000\000\000\058\255\000\000\013\255\
\000\000\000\000\000\000\000\000\000\000\000\000\067\255\000\000\
\000\000\136\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000\000\000\043\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\032\000\250\255\000\000\224\255\005\000\000\000\000\000"

let yytablesize = 336
let yytable = "\032\000\
\010\000\048\000\087\000\030\000\001\000\027\000\038\000\065\000\
\066\000\067\000\068\000\033\000\034\000\035\000\004\000\005\000\
\036\000\037\000\074\000\044\000\031\000\077\000\031\000\039\000\
\040\000\041\000\042\000\078\000\079\000\012\000\045\000\049\000\
\013\000\031\000\031\000\043\000\046\000\050\000\070\000\052\000\
\072\000\055\000\005\000\051\000\053\000\075\000\076\000\094\000\
\054\000\095\000\020\000\021\000\031\000\023\000\073\000\056\000\
\057\000\058\000\059\000\069\000\060\000\080\000\061\000\081\000\
\082\000\083\000\062\000\084\000\085\000\086\000\088\000\089\000\
\090\000\091\000\092\000\093\000\104\000\071\000\106\000\000\000\
\000\000\100\000\101\000\102\000\103\000\000\000\000\000\096\000\
\097\000\098\000\099\000\000\000\000\000\000\000\000\000\003\000\
\004\000\005\000\006\000\107\000\105\000\007\000\008\000\009\000\
\010\000\011\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\000\000\013\000\000\000\000\000\000\000\000\000\014\000\
\015\000\016\000\017\000\018\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\020\000\021\000\022\000\023\000\
\003\000\004\000\005\000\006\000\000\000\000\000\007\000\008\000\
\009\000\010\000\011\000\000\000\000\000\000\000\000\000\000\000\
\012\000\000\000\000\000\013\000\000\000\000\000\000\000\000\000\
\014\000\015\000\016\000\017\000\018\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\000\000\020\000\021\000\031\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\000\010\000\000\000\010\000\010\000\027\000\
\027\000\000\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\010\000\010\000\027\000\000\000\
\000\000\027\000\000\000\000\000\000\000\000\000\000\000\010\000\
\047\000\047\000\000\000\000\000\027\000\000\000\010\000\010\000\
\010\000\010\000\027\000\027\000\027\000\027\000\027\000\005\000\
\005\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\005\000\000\000\000\000\000\000\000\000\000\000\005\000"

let yycheck = "\006\000\
\000\000\000\000\000\000\040\001\001\000\000\000\013\000\040\000\
\041\000\042\000\043\000\007\000\008\000\009\000\002\001\003\001\
\040\001\040\001\051\000\040\001\019\001\054\000\021\001\018\001\
\018\001\018\001\018\001\060\000\061\000\017\001\033\001\027\000\
\020\001\032\001\033\001\018\001\033\001\037\001\045\000\005\001\
\047\000\021\001\000\000\034\001\033\001\052\000\053\000\080\000\
\034\001\082\000\038\001\039\001\040\001\041\001\050\000\013\001\
\014\001\015\001\016\001\036\001\018\001\018\001\020\001\019\001\
\035\001\019\001\024\001\019\001\019\001\019\001\032\001\006\001\
\012\001\033\001\019\001\021\001\019\001\046\000\012\001\255\255\
\255\255\088\000\089\000\090\000\091\000\255\255\255\255\083\000\
\084\000\085\000\086\000\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\004\001\106\000\096\000\007\001\008\001\009\001\
\010\001\011\001\255\255\255\255\255\255\255\255\255\255\017\001\
\255\255\255\255\020\001\255\255\255\255\255\255\255\255\025\001\
\026\001\027\001\028\001\029\001\255\255\255\255\255\255\255\255\
\255\255\255\255\036\001\255\255\038\001\039\001\040\001\041\001\
\001\001\002\001\003\001\004\001\255\255\255\255\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\017\001\255\255\255\255\020\001\255\255\255\255\255\255\255\255\
\025\001\026\001\027\001\028\001\029\001\255\255\255\255\255\255\
\255\255\255\255\255\255\036\001\255\255\038\001\039\001\040\001\
\041\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\003\001\255\255\005\001\006\001\002\001\
\003\001\255\255\255\255\255\255\012\001\255\255\255\255\255\255\
\255\255\017\001\255\255\255\255\020\001\021\001\017\001\255\255\
\255\255\020\001\255\255\255\255\255\255\255\255\255\255\031\001\
\031\001\031\001\255\255\255\255\031\001\255\255\038\001\039\001\
\040\001\041\001\037\001\038\001\039\001\040\001\041\001\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\021\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\031\001\255\255\255\255\255\255\255\255\255\255\037\001"

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
  BOOL\000\
  NAT\000\
  STRING\000\
  UNIT\000\
  UNITV\000\
  LCOR\000\
  RCOR\000\
  LPAREN\000\
  RPAREN\000\
  LBRACKET\000\
  RBRACKET\000\
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
  CONCAT\000\
  "

let yynames_block = "\
  ID\000\
  INTV\000\
  STRINGV\000\
  STRINGT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'termS) in
    Obj.repr(
# 59 "parser.mly"
      ( Bind (_1, _3))
# 312 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'termS) in
    Obj.repr(
# 61 "parser.mly"
      ( Eval _1 )
# 319 "parser.ml"
               : Lambda.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 65 "parser.mly"
       ( _1 )
# 326 "parser.ml"
               : 'termS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'termS) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 67 "parser.mly"
      (TmApp(TmAbs("_",TyUnit, _3), _1))
# 334 "parser.ml"
               : 'termS))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'appTerm) in
    Obj.repr(
# 72 "parser.mly"
      ( _1 )
# 341 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 74 "parser.mly"
      ( TmIf (_2, _4, _6) )
# 350 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 76 "parser.mly"
      ( TmAbs (_2, _4, _6) )
# 359 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 78 "parser.mly"
      ( TmLetIn (_2, _4, _6) )
# 368 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ty) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 80 "parser.mly"
      ( TmLetRec (_2, _4, _6, _8) )
# 378 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 86 "parser.mly"
      ( _1 )
# 385 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 88 "parser.mly"
      ( TmSucc _2 )
# 392 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 90 "parser.mly"
      ( TmPred _2 )
# 399 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 92 "parser.mly"
      ( TmIsZero _2 )
# 406 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'pathTerm) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 94 "parser.mly"
      ( TmConcat (_1, _3) )
# 414 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'appTerm) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 96 "parser.mly"
      ( TmApp (_1, _2) )
# 422 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 98 "parser.mly"
      ( TmString _2 )
# 429 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'pathTerm) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 100 "parser.mly"
     ( TmCons (_3,_5,_6) )
# 438 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 102 "parser.mly"
     ( TmIsNil (_3,_5) )
# 446 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 104 "parser.mly"
     ( TmHead (_3,_5) )
# 454 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'pathTerm) in
    Obj.repr(
# 106 "parser.mly"
     ( TmTail (_3,_5) )
# 462 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 108 "parser.mly"
     ( TmNil (_3) )
# 469 "parser.ml"
               : 'appTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTerm) in
    Obj.repr(
# 118 "parser.mly"
      ( _1 )
# 476 "parser.ml"
               : 'pathTerm))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 123 "parser.mly"
      ( _2 )
# 483 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 125 "parser.mly"
      ( _3 )
# 491 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "parser.mly"
      (TmTrue)
# 497 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
      ( TmFalse )
# 503 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
      ( TmVar _1 )
# 510 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 133 "parser.mly"
      (TmString _1)
# 517 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 135 "parser.mly"
      ( let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f _1 )
# 527 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
      ( TmUnit )
# 533 "parser.ml"
               : 'atomicTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomicTy) in
    Obj.repr(
# 145 "parser.mly"
      ( _1 )
# 540 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'atomicTy) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ty) in
    Obj.repr(
# 147 "parser.mly"
      ( TyArr (_1, _3) )
# 548 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 151 "parser.mly"
      ( _2 )
# 555 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 153 "parser.mly"
      ( _2 )
# 562 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 155 "parser.mly"
      ( TyBool )
# 568 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 157 "parser.mly"
      ( TyNat )
# 574 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser.mly"
      ( TyString )
# 580 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "parser.mly"
      ( TyUnit )
# 586 "parser.ml"
               : 'atomicTy))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 163 "parser.mly"
      ( TyList _3 )
# 593 "parser.ml"
               : 'atomicTy))
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
