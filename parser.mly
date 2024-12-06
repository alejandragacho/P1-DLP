
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

%token <string> ID
%token <int> INTV
%token <string> STRINGV
%token <string> STRINGT

%start s
%type <Lambda.command> s

%%

s :
    STRINGV EQ termS EOF 
      { Bind ($1, $3)}
  | termS EOF
      { Eval $1 }

termS:
     term
       { $1 }
     | termS SEMICOLON term
       { TmApp(TmAbs("_",TyUnit, $3), $1) }

term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
    { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) } 
  | STRINGV
      { TmString $1 }  
  | term CONCAT term
      { TmConcat ($1, $3) }  

appTerm :
    pathTerm
      { $1 }
  | SUCC pathTerm
      { TmSucc $2 }
  | PRED pathTerm
      { TmPred $2 }
  | ISZERO pathTerm
      { TmIsZero $2 }
  | pathTerm CONCAT pathTerm
      { TmConcat ($1, $3) }
  | appTerm pathTerm
      { TmApp ($1, $2) }
  | QM STRINGV QM 
      { TmString $2 }
  | CONS LBRACKET ty RBRACKET pathTerm pathTerm
      { TmCons ($3,$5,$6) }
  | ISNIL LBRACKET ty RBRACKET pathTerm
      { TmIsNil ($3,$5) }
  | HEAD LBRACKET ty RBRACKET pathTerm
      { TmHead ($3,$5) }
  | TAIL LBRACKET ty RBRACKET pathTerm
      { TmTail ($3,$5) }
  | NIL LBRACKET ty RBRACKET
      { TmNil ($3) }

pathTerm :
   | pathTerm DOT INTV
      { TmProjection ($1, (string_of_int $3)) }
      
   | pathTerm DOT STRINGV
      { TmProjection ($1,$3) }

   | atomicTerm
      { $1 } 

atomicTerm :
    LPAREN term RPAREN
      { $2 }   
  | ID EQ term
      { $3 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | STRINGT 
      { TmString $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | UNITV 
     { TmUnit }
  | LBRACE recordTM RBRACE
     { TmRecord $2 }
  | LBRACE tupleTM RBRACE
     { TmTuple $2 }

recordTM:
   | { [] } 
   | noemptyrecordTM { $1 }

noemptyrecordTM:
   | STRINGV EQ term {[$1,$3]}
   | STRINGV EQ term COMMA noemptyrecordTM {($1,$3)::$5}

tupleTM:
   | term { [$1] }
   | term COMMA tupleTM { $1::$3 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | LBRACKET ty RBRACKET 
      { $2 }
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING 
      { TyString }
  | UNIT
      { TyUnit }
  | LBRACE recordTY RBRACE
      { TyRecord $2 }
  | LBRACE tupleTY RBRACE
      { TyTuple $2 }
  | LIST LBRACKET ty RBRACKET
      { TyList $3 }

recordTY:
  | { [] }
  | noemptyrecordTY { $1 }

noemptyrecordTY:
  | STRINGV COLON ty {[$1,$3]}
  | STRINGV COLON ty COMMA noemptyrecordTY {($1,$3)::$5}

tupleTY:
  | ty { [$1] }
  | ty COMMA tupleTY { $1::$3 }   
