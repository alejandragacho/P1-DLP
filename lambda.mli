


type ty =
  TyBool
  | TyNat
  | TyUnit
  | TyArr of ty * ty
  | TyList of ty
  | TyTuple of ty list
  | TyString
  ;;
  
  type 'a context =
  (string * 'a) list;; (* Contexto polimórfico 'a es alfa *)
  
  type term =
  TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term (** constructor de abstracción *)
  | TmApp of term * term
  | TmLetIn of string * term * term (** constructor de let in *)
  | TmFix of term (** constructor de fix *)
  | TmLetRec of string * ty * term * term
  | TmUnit
  | TmNil of ty
  | TmCons of ty * term * term
  | TmIsNil of ty * term
  | TmHead of ty * term
  | TmTail of ty * term
  | TmString of string
  | TmConcat of term * term 
  ;;
  
  type command = 
  Eval of term
  | Bind of string * term
  ;;
  
  val emptyctx : 'a context;;
  val addbinding : 'a context -> string -> 'a  -> 'a context;;
  val getbinding : 'a context -> string -> 'a;;
  
  val string_of_ty : ty -> string;;
  exception Type_error of string;;
  val typeof : ty context -> term -> ty;;
  
  val string_of_term : term -> string;;
  exception NoRuleApplies;;
  val eval : term context -> term -> term;;
  
  val execute : term context * ty context -> command -> term context * ty context;;