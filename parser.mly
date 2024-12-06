
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token BOOL
%token NAT
%token STRING
%token UNIT
%token UNITV
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LIST
%token NIL
%token CONS
%token ISNIL
%token HEAD
%token TAIL
%token COMMA
%token SEMICOLON
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF
%token QM
%token CONCAT

%token <int> INTV
%token <string> IDV
%token <string> STRINGV

%start s
%type <Lambda.command> s

%%

s :
  term EOF
      { Eval $1 }
  | IDV EQ term EOF
      { Bind ($1, $3) }

term :
  appTerm
      { $1 }  
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LAMBDA IDV DOT term
      { TmAbs ($2, TyNat, $4) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | CONCAT pathTerm pathTerm
      { TmConcat ($2, $3) }
  | appTerm pathTerm
      { TmApp ($1, $2) }
  | CONS LBRACKET ty COLON pathTerm COMMA pathTerm RBRACKET
      { TmCons ($3, $5, $7) }
  | HEAD LBRACKET ty RBRACKET pathTerm
      { TmHead ($3, $5) }
  | TAIL LBRACKET ty RBRACKET pathTerm
      { TmTail ($3, $5) }
  | NIL LBRACKET ty RBRACKET
      { TmNil ($3) }

pathTerm :
  | pathTerm DOT INTV
      { TmProjection ($1, (string_of_int $3)) }
  | pathTerm DOT IDV
      { TmProjection ($1, $3) }
  | atomicTerm
      { $1 }

atomicTerm :
  LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | STRINGV
      { TmString $1 }
  | LBRACE tuple RBRACE
      { TmTuple $2 }
  | LBRACE record RBRACE
      { TmRecord $2 }

tuple :
  | term { [$1] }
  | term COMMA tuple { $1::$3 }

record :
  | { [] }
  | noEmptyRecord { $1 }

noEmptyRecord :
  | IDV EQ term { [$1,$3] }
  | IDV EQ term COMMA noEmptyRecord { ($1,$3)::$5 }

ty :
  atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
  LPAREN ty RPAREN
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }
  | LBRACE tupleTy RBRACE
      { TyTuple $2 }
  | LBRACE recordTy RBRACE
      { TyRecord $2 }
  | LIST LBRACKET ty RBRACKET
      { TyList $3 }

tupleTy :
    | ty { [$1] }
    | ty COMMA tupleTy { $1::$3 }

recordTy : 
    | { [] }
    | noEmptyRecordTy { $1 }

noEmptyRecordTy:
    | IDV COLON ty { [$1,$3] }
    | IDV COLON ty COMMA noEmptyRecordTy { ($1,$3)::$5 }

var :
  | term { [$1] }
  | term COMMA var { $1::$3 }

noEmptyVar :
  | IDV COLON ty { [$1,$3] }
  | IDV COLON ty COMMA noEmptyVar { ($1,$3)::$5 }
