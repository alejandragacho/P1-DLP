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

val s :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Lambda.command
