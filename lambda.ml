
(* TYPE DEFINITIONS *)

type ty =
  TyBool
| TyNat
| TyArr of ty * ty
| TyList of ty
| TyRecord of (string * ty) list
| TyTuple of ty list
| TyString

;;

type 'a context =
(string * 'a) list  (* Contexto polimórfico 'a es alfa *)
;;


exception Type_error of string
;;

type term =
  TmTrue
| TmFalse
| TmIf of term * term * term
| TmZero
| TmSucc of term
| TmPred of term
| TmIsZero of term
| TmVar of string
| TmAbs of string * ty * term
| TmApp of term * term
| TmLetIn of string * term * term
| TmFix of term
| TmLetRec of string * ty * term * term
| TmProjection of term * string
| TmRecord of (string * term) list
| TmNil of ty
| TmCons of ty * term * term
| TmIsNil of ty * term
| TmHead of ty * term
| TmTail of ty * term
| TmTuple of term list
| TmString of string
| TmConcat of term * term 
;;

type command = 
Eval of term
| Bind of string * term
;;


(* CONTEXT MANAGEMENT *)

let emptyctx =
[]
;;

let addbinding ctx x bind =
(x, bind) :: ctx
;;

let getbinding ctx x =
List.assoc x ctx
;;

(* GLOBAL DEFINITIONS *)

let emptydef = 
[]
;;

let adddef def x bind =
(x, bind) :: def
;;

let getdef def x = 
List.assoc x def
;;

let global_context : (string, term) Hashtbl.t = Hashtbl.create 100;;

let add_global_def id term =
Hashtbl.replace global_context id term
;;

let get_global_def id =
    try Hashtbl.find global_context id
    with Not_found -> failwith ("Undefined global: " ^ id)
  ;;

(* LISTS IMPLEMENTATION *)
let is_nil t = match t with
  | TmNil _ -> true
  | _ -> false

let get_head t = match t with
  | TmCons (_, h, _) -> h
  | _ -> raise (Type_error "Cannot get head of a non-list")

let get_tail t = match t with
  | TmCons (_, _, t) -> t
  | _ -> raise (Type_error "Cannot get tail of a non-list")



(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
  TyBool ->
    "Bool"
| TyNat ->
    "Nat"
| TyString -> 
    "String"
| TyArr (ty1, ty2) ->
    "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"      

| TyTuple tyr -> 
  let rec print = function
    [] -> ""
    |(ty::[]) -> (string_of_ty ty)
    |(ty::t) -> (string_of_ty ty) ^ ", " ^ print t
  in "{" ^ (print tyr) ^ "}"
    
| TyList ty -> "List[" ^ string_of_ty ty ^ "]"
| _ -> "Unknown type"  (* Caso por defecto *)
;;

(* Definición de prioridades para operadores y términos *)
let priority_of_term = function
  | TmApp _ -> 1  (* La aplicación tiene la prioridad más baja *)
  | TmAbs _ -> 2  (* La abstracción lambda tiene una prioridad media *)
  | TmIf _ -> 3   (* Los condicionales tienen una prioridad alta *)
  | _ -> 4         (* Otros términos no necesitan paréntesis *)


(* Función recursiva principal del pretty-printer *)
let rec pretty_print ?(parent_priority=0) term =
  match term with
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmZero -> "0"
  | TmString s -> "\"" ^ s ^ "\""
  | TmSucc t ->
    let rec count_succ t count =
      match t with
      | TmZero -> string_of_int count  (* Termina contando los sucesores *)
      | TmSucc inner -> count_succ inner (count + 1)  (* Recurre incrementando el contador *)
      | _ -> "succ (" ^ pretty_print t ^ ")"  (* Maneja casos no canónicos *)
    in
    count_succ t 1

  | TmPred t -> 
      "pred " ^ pretty_print ~parent_priority:(priority_of_term term) t
  | TmIsZero t ->
      "iszero " ^ pretty_print ~parent_priority:(priority_of_term term) t
  | TmVar x -> x
  | TmAbs (param, ty, body) ->
      let abs_str = 
        "lambda " ^ param ^ ":" ^ string_of_ty ty ^ ". " ^ 
        pretty_print ~parent_priority:(priority_of_term term) body
      in if parent_priority > priority_of_term term then "(" ^ abs_str ^ ")" else abs_str
  | TmApp (t1, t2) ->
      let app_str = 
        pretty_print ~parent_priority:(priority_of_term term) t1 ^ " " ^
        pretty_print ~parent_priority:(priority_of_term term) t2
      in if parent_priority > priority_of_term term then "(" ^ app_str ^ ")" else app_str
  | TmIf (cond, t_then, t_else) ->
      let if_str = 
        "if " ^ pretty_print ~parent_priority:(priority_of_term term) cond ^
        " then " ^ pretty_print ~parent_priority:(priority_of_term term) t_then ^
        " else " ^ pretty_print ~parent_priority:(priority_of_term term) t_else
      in if parent_priority > priority_of_term term then "(" ^ if_str ^ ")" else if_str
  | TmFix t ->
      "fix " ^ pretty_print ~parent_priority:(priority_of_term term) t
  |TmLetRec (f, tyF, bodyF, body) ->
      "letrec " ^ f ^ " : " ^ string_of_ty tyF ^ " = " ^
      pretty_print ~parent_priority:(priority_of_term term) bodyF ^
      " in " ^ pretty_print ~parent_priority:(priority_of_term term) body
  | TmConcat (t1, t2) ->
      pretty_print ~parent_priority:(priority_of_term term) t1 ^ " ^ " ^
      pretty_print ~parent_priority:(priority_of_term term) t2
  | TmLetIn (var, t1, t2) ->
      let let_str =
        "let " ^ var ^ " = " ^ pretty_print ~parent_priority:(priority_of_term term) t1 ^
        " in " ^ pretty_print ~parent_priority:(priority_of_term term) t2
      in if parent_priority > priority_of_term term then "(" ^ let_str ^ ")" else let_str
  | TmHead (ty, t) -> "head[" ^ string_of_ty ty ^ "](" ^ pretty_print t ^ ")"
  | TmTail (ty, t) -> "tail[" ^ string_of_ty ty ^ "](" ^ pretty_print t ^ ")"
  | TmNil ty ->
      "nil[" ^ string_of_ty ty ^ "]"
  | TmCons (ty, head, tail) ->
      "cons[" ^ string_of_ty ty ^ "] (" ^ pretty_print head ^ ", " ^ pretty_print tail ^ ")"
  | _ -> raise (Failure "Unhandled case in pretty_print")
;;


(*let rec subtypeof tm1 tm2 = match (tm1, tm2) with
  | (TyArr(s1, s2), TyArr(t1, t2)) -> ((subtypeof s1 t1) && (subtypeof t2 s2))
  | (tm1, tm2) -> tm1 = tm2
  | TyTuple tyr ->
      let rec print = function
          [] -> ""
          | ty::[] -> string_of_ty ty
          | ty::t -> string_of_ty ty ^ ", " ^ print t
      in "{" ^ print tyr ^ "}"
  | TyRecord tyr ->
      let rec print = function
          [] -> ""
          | (s, ty)::[] -> s ^ ":" ^ string_of_ty ty
          | (s, ty)::t -> s ^ ":" ^ string_of_ty ty ^ "," ^ print t
      in "{" ^ print tyr ^ "}"
  | TyList ty -> "List[" ^ string_of_ty ty ^ "]"
;;
*)

(* Subtyping *)
let rec subtypeof tm1 tm2 = 
  match (tm1, tm2) with
  | (TyArr (s1, s2), TyArr (t1, t2)) ->
      subtypeof_functions s1 s2 t1 t2
  | (TyRecord l1, TyRecord l2) ->
      subtypeof_records l1 l2
  | (tm1, tm2) ->
      tm1 = tm2

(* Subtyping for functions *)
and subtypeof_functions s1 s2 t1 t2 =
  subtypeof t1 s1 && subtypeof s2 t2

(* Subtyping for records *)
and subtypeof_records l1 l2 =
  let rec check_field (field, ty) l =
    match List.assoc_opt field l with
    | Some ty2 -> subtypeof ty ty2
    | None -> false
  in
  List.for_all (fun field_ty -> check_field field_ty l2) l1
;;

let rec typeof ctx tm = match tm with
  (* T-True *)
  TmTrue ->
    TyBool

  (* T-False *)
| TmFalse ->
    TyBool


  (* T-If *)
| TmIf (t1, t2, t3) ->
    if typeof ctx t1 = TyBool then
      let tyT2 = typeof ctx t2 in
      if typeof ctx t3 = tyT2 then tyT2
      else raise (Type_error "arms of conditional have different types")
    else
      raise (Type_error "guard of conditional not a boolean")

    
  (* T-Zero *)
| TmZero ->
    TyNat

  (* T-Succ *)
| TmSucc t1 ->
    if typeof ctx t1 = TyNat then TyNat
    else raise (Type_error "argument of succ is not a number")

  (* T-Pred *)
| TmPred t1 ->
    if typeof ctx t1 = TyNat then TyNat
    else raise (Type_error "argument of pred is not a number")

  (* T-Iszero *)
| TmIsZero t1 ->
    if typeof ctx t1 = TyNat then TyBool
    else raise (Type_error "argument of iszero is not a number")

  (* T-Var *)
| TmVar x ->
    (try getbinding ctx x with
     _ -> raise (Type_error ("no binding type for variable " ^ x)))

  (* T-Abs *)
| TmAbs (x, tyT1, t2) ->
    let ctx' = addbinding ctx x tyT1 in
    let tyT2 = typeof ctx' t2 in
    TyArr (tyT1, tyT2)

  (* T-App *)
| TmApp (t1, t2) ->
    let tyT1 = typeof ctx t1 in
    let tyT2 = typeof ctx t2 in
    (match tyT1 with
         TyArr (tyT11, tyT12) ->
           if subtypeof tyT11 tyT2 then tyT12
           else raise (Type_error "parameter type mismatch")
       | _ -> raise (Type_error "arrow type expected"))

  (* T-Let *)
| TmLetIn (x, t1, t2) ->
    let tyT1 = typeof ctx t1 in
    let ctx' = addbinding ctx x tyT1 in
    typeof ctx' t2


  (* T-Fix *)
(*| TmFix t ->
  (match typeof ctx t with
  | TyArr (tyT1, tyT2) when subtypeof tyT1 tyT2 -> tyT2
  | _ -> raise (Type_error "TmFix must be applied to a function with matching input and output types"))*)

| TmString _ -> 
  TyString

(* T-Concat *)
| TmConcat (t1,t2) -> 
if (typeof ctx t1 = TyString) && (typeof ctx t2 = TyString) then
  TyString
else 
  raise (Type_error "terms of concatenate are not strings")


| TmLetRec (f, tyF, bodyF, body) ->
  let ctx' = addbinding ctx f tyF in
  let tyBodyF = typeof ctx' bodyF in
  if subtypeof tyF tyBodyF then
    typeof ctx' body
  else
    raise (Type_error "Type mismatch in letrec")

      
| TmNil ty -> TyList ty
      
      
  (* T-Cons *)
| TmCons (ty,h,t) ->
    let tyTh = typeof ctx h in
      let tyTt = typeof ctx t in
         if (subtypeof tyTh ty) && (subtypeof tyTt (TyList(ty))) then 
            TyList(ty) else raise (Type_error "elements of list have 
          different types")

     
	  
    (* T-Fix *)
| TmFix t1 ->
      let tyT1 = typeof ctx t1 in
	  (match tyT1 with
	       TyArr (tyT11, tyT12) ->
		     if tyT11 = tyT12 then tyT12
			 else raise (Type_error "result of body not compatible with domain")
	  | _ -> raise (Type_error "arrow type expected"))


    (* T-Tuple *)
| TmTuple tmt ->
      let rec get_types = function
          [] -> []
          | tm::t -> typeof ctx tm :: get_types t
      in TyTuple (get_types tmt)
      
    (* T-Projection *)
| TmProjection (t, n) ->
      (match (typeof ctx t, n) with
          | TyRecord tyr, s -> 
              (try List.assoc s tyr with
              _ -> raise (Type_error ("Projection error. Key " ^ s ^ " doesn't exist in the record")))
          | TyTuple tyr, s ->
              (try List.nth tyr (int_of_string s - 1) with
              _ -> raise (Type_error ("Projection error. Key " ^ s ^ " doesn't exist in the tuple")))
          | _ -> raise (Type_error ("Projection error. Type can't be projected")))
 
    (* T-Record *)
| TmRecord tmr ->
       let rec get_types = function
           [] -> []
           | (s, tm)::t -> (s, typeof ctx tm) :: get_types t
       in TyRecord (get_types tmr)
    (* T-Cons *)
          
    (* T-IsNil *)
| TmIsNil (ty,t) ->
      if typeof ctx t = TyList(ty) then TyBool
      else raise (Type_error ("argument of is empty is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))

   (* T-Head *)
| TmHead (ty,t) ->
      if typeof ctx t = TyList(ty) then ty
      else raise (Type_error ("argument of head is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))
    
   (* T-Tail *)
| TmTail (ty,t) ->
      if typeof ctx t = TyList(ty) then TyList(ty)
      else raise (Type_error ("argument of tail is not a " ^ "List[" ^ (string_of_ty ty) ^ "]"))
;;

let rec term_to_int tm =
  match tm with
  | TmZero -> 0
  | TmSucc t -> 1 + term_to_int t
  | _ -> raise (Type_error "Not a numeric value")

(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
  TmTrue ->
    "true"
| TmFalse ->
    "false"
| TmString (s) -> 
  "\"" ^ s ^ "\""
| TmConcat (t1,t2) ->
    string_of_term t1 ^ " ^ " ^ string_of_term t2
| TmIf (t1,t2,t3) ->
    "if " ^ "(" ^ string_of_term t1 ^ ")" ^
    " then " ^ "(" ^ string_of_term t2 ^ ")" ^
    " else " ^ "(" ^ string_of_term t3 ^ ")"
| TmZero ->
    "0"
| TmSucc t ->
   let rec f n t' = match t' with
        TmZero -> string_of_int n
      | TmSucc s -> f (n+1) s
      | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
    in f 1 t
| TmPred t ->
    "pred " ^ "(" ^ string_of_term t ^ ")"
| TmIsZero t ->
    "iszero " ^ "(" ^ string_of_term t ^ ")"
| TmVar s ->
    s
| TmAbs (s, tyS, t) ->
    "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
| TmApp (t1, t2) ->
    "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
| TmLetIn (s, t1, t2) ->
    "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
| TmFix t ->
    "(fix " ^ string_of_term t ^ ")"

| TmLetRec (f, ty, bodyF, body) ->
    "letrec " ^ f ^ " : " ^ string_of_ty ty ^ " = " ^ 
    string_of_term bodyF ^ " in " ^ string_of_term body


| TmNil ty -> "nil[" ^string_of_ty ty ^ "]"
| TmCons (ty,h,t) -> "cons[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term h ^ " " ^ (string_of_term t) ^ ")"
| TmIsNil (ty,t) -> "isnil[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")" 
| TmHead (ty,t) -> "head[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")" 
| TmTail (ty,t) -> "tail[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"

| _ -> "Unknown term"  (* Maneja cualquier caso no cubierto *)

;;

let rec ldif l1 l2 = match l1 with
  [] -> []
| h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
  [] -> l2
| h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with (* variables libres *)
  TmTrue ->
    []
| TmFalse ->
    []
| TmString _ -> 
    []
| TmIf (t1, t2, t3) ->
    lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
| TmZero ->
    []
| TmSucc t ->
    free_vars t
| TmPred t ->
    free_vars t
| TmIsZero t ->
    free_vars t
| TmVar s ->
    [s]
| TmAbs (s, _, t) ->
    ldif (free_vars t) [s]
| TmApp (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
| TmLetIn (s, t1, t2) ->
    lunion (ldif (free_vars t2) [s]) (free_vars t1)
| TmFix t ->
    free_vars t

| TmLetRec (f, _, bodyF, body) ->
    let free_in_bodyF = free_vars bodyF in
    let free_in_body = free_vars body in
    lunion free_in_bodyF (ldif free_in_body [f])

| TmConcat (t1, t2) -> 
    lunion (free_vars t1) (free_vars t2)

| TmTuple tmt ->
      let rec get_free = function
          [] -> []
          | tm::t -> lunion (free_vars tm) (get_free t)
      in get_free tmt
| TmProjection (t, n) ->
      free_vars t
| TmRecord tmr ->
      let rec get_free = function
          [] -> []
          | (_, tm)::t -> lunion (free_vars tm) (get_free t)
      in get_free tmr
| TmNil ty ->
      []
| TmCons (ty,t1,t2) ->
      lunion (free_vars t1) (free_vars t2)
| TmIsNil (ty,t) ->
      free_vars t
| TmHead (ty,t) ->
      free_vars t
| TmTail (ty,t) ->
      free_vars t
;;




let rec fresh_name x l =
if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
  
let rec subst x s tm = match tm with (* sustituir las apariciones de x en tm por s *)
  TmTrue ->
    TmTrue
| TmFalse ->
    TmFalse
| TmString _ -> 
    tm
| TmConcat (t1, t2) ->
    TmConcat (subst x s t1, subst x s t2)
| TmIf (t1, t2, t3) ->
    TmIf (subst x s t1, subst x s t2, subst x s t3)
| TmZero ->
    TmZero
| TmSucc t ->
    TmSucc (subst x s t)
| TmPred t ->
    TmPred (subst x s t)
| TmIsZero t ->
    TmIsZero (subst x s t)
| TmVar y ->
    if y = x then s else tm
| TmAbs (y, tyY, t) -> 
    if y = x then tm
    else let fvs = free_vars s in
         if not (List.mem y fvs)
         then TmAbs (y, tyY, subst x s t)
         else let z = fresh_name y (free_vars t @ fvs) in
              TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
| TmApp (t1, t2) ->
    TmApp (subst x s t1, subst x s t2)
| TmLetIn (y, t1, t2) ->
    if y = x then TmLetIn (y, subst x s t1, t2)
    else let fvs = free_vars s in
         if not (List.mem y fvs)
         then TmLetIn (y, subst x s t1, subst x s t2)
         else let z = fresh_name y (free_vars t2 @ fvs) in
              TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
| TmFix t ->
    TmFix (subst x s t)
    
| TmLetRec (f, ty, bodyF, body) ->
    if f = x then
      TmLetRec (f, ty, bodyF, body)  (* No sustituir dentro de `f` *)
    else
      let bodyF' = subst x s bodyF in
      let body' = subst x s body in
      TmLetRec (f, ty, bodyF', body')

| TmTuple tmt ->
      let rec sub_t = function
          [] -> []
          | tm::t -> (subst x s tm)::sub_t t
      in TmTuple (sub_t tmt)
| TmProjection (t, n) ->
      TmProjection (subst x s t, n)
| TmRecord tmr ->
      let rec sub_r = function
          [] -> []
          | (str, tm)::t -> (str, (subst x s tm))::sub_r t
      in TmRecord (sub_r tmr)
| TmNil ty ->
    tm
| TmCons (ty,t1,t2) ->
      TmCons (ty, (subst x s t1), (subst x s t2))
| TmIsNil (ty,t) ->
     TmIsNil (ty, (subst x s t))
| TmHead (ty,t) ->
     TmHead (ty, (subst x s t))
     
     
| TmTail (ty,t) ->
     TmTail (ty, (subst x s t))
     
let rec isnumericval tm = match tm with
  TmZero -> true
| TmSucc t -> isnumericval t
| _ -> false
;;



exception NoRuleApplies;;

let rec isval tm = match tm with
  TmTrue  -> true
| TmFalse -> true
| TmAbs _ -> true
| TmString _ -> true
| TmNil _ -> true
| TmCons(_,h,t) -> (isval h) && (isval t)
| t when isnumericval t -> true
| TmTuple l -> List.for_all(fun t -> isval(t)) l
| TmRecord [] -> true
| TmRecord l -> List.for_all(fun (s, t) -> isval(t)) l
| _ -> false
;;


let concat t1 t2 = t1 ^ t2;;



let rec eval1 vctx tm = match tm with
  (* E-IfTrue *)
  TmIf (TmTrue, t2, _) ->
    t2

  (* E-IfFalse *)
| TmIf (TmFalse, _, t3) ->
    t3

  (* E-If *)
| TmIf (t1, t2, t3) ->
    let t1' = eval1 vctx t1 in
    TmIf (t1', t2, t3)

  (* E-Succ *)
| TmSucc t1 ->
    let t1' = eval1 vctx t1 in
    TmSucc t1'

    
  (* E-PredZero *)
| TmPred TmZero ->
    TmZero

  (* E-PredSucc *)
| TmPred (TmSucc nv1) when isnumericval nv1 ->
    nv1

  (* E-Pred *)
| TmPred t1 ->
    let t1' = eval1 vctx t1 in
    TmPred t1'

  (* E-IszeroZero *)
| TmIsZero TmZero ->
    TmTrue

  (* E-IszeroSucc *)
| TmIsZero (TmSucc nv1) when isnumericval nv1 ->
    TmFalse

  (* E-Iszero *)
| TmIsZero t1 ->
    let t1' = eval1 vctx t1 in
    TmIsZero t1'

  (* E-AppAbs *)
| TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
    subst x v2 t12

  (* E-App2: evaluate argument before applying function *)
| TmApp (v1, t2) when isval v1 ->
    let t2' = eval1 vctx t2 in
    TmApp (v1, t2')

  (* E-App1: evaluate function before argument *)
| TmApp (t1, t2) ->
    let t1' = eval1 vctx t1 in
    TmApp (t1', t2)

  (* E-LetV *)
| TmLetIn (x, v1, t2) when isval v1 ->
    subst x v1 t2

  (* E-Let *)
| TmLetIn(x, t1, t2) ->
    let t1' = eval1 vctx t1 in
    TmLetIn (x, t1', t2) 

  (* E-FixBeta *)
| TmFix (TmAbs (x, ty, t)) ->
  subst x (TmFix (TmAbs (x, ty, t))) t
| TmFix t ->
  let t' = eval1 vctx t in
  TmFix t'


   (* E-Concat *) 
| TmConcat (TmString(s1),TmString(s2)) ->  
   TmString(s1^s2)

 (* E-Concat2 *)
| TmConcat (TmString(s),t1) -> 
   let t1' = eval1 vctx t1 
   in TmConcat (TmString(s),t1')

 (* E-Concat1 *)
| TmConcat (t1,t2) -> 
   let t1' = eval1 vctx t1 
   in TmConcat (t1',t2)


| TmVar s ->
    getbinding vctx s

| TmLetRec (f, tyF, bodyF, body) ->
    let rec_func = TmFix (TmAbs (f, tyF, bodyF)) in
    eval1 vctx (TmLetIn (f, rec_func, body))

    
  (*E-Cons2*)
|TmCons(ty,h,t) when isval h -> TmCons(ty,h,(eval1 vctx t))

  (* E-Cons1 *)
| TmCons(ty, h, t) -> TmCons(ty,(eval1 vctx h),t)

  (* E-IsNilNil *)
| TmIsNil(ty, TmNil(_)) -> TmTrue

  (* E-IsNilCons *)
| TmIsNil(ty, TmCons(_, _, _)) -> TmFalse

  (* E-IsNil *)
| TmIsNil(ty, t) -> TmIsNil(ty, eval1 vctx t)

         
  (* E-HeadCons *)
| TmHead (_, t) when isval t ->
  get_head t

  (* E-TailCons *)
| TmTail (_, t) when isval t ->
  get_tail t


| TmProjection (TmRecord l as v , s) when isval(v) -> 
    List.assoc s l 

 (* E-ProjRecord *)
| TmProjection (TmRecord tmr, n) ->
    List.assoc n tmr 
   
 (* E-Proj *)
| TmProjection (TmTuple l, s) ->
    let evaluated_tuple = List.map (eval1 vctx) l in
    List.nth evaluated_tuple (int_of_string s - 1)

| TmProjection (t, s) ->
    let evaluated_t = eval1 vctx t in
    (match evaluated_t with
    | TmTuple l -> List.nth l (int_of_string s - 1)
    | _ -> TmProjection (evaluated_t, s))

| TmTuple tml ->
  let rec eval_rcd = function
    [] -> raise NoRuleApplies
    | tm::t when isval tm -> tm::(eval_rcd t)
    | tm::t -> (eval1 vctx tm)::t
  in TmTuple (eval_rcd tml)
   
 (*E-Record*)
| TmRecord tmr ->
    let rec evalrecord = function
      [] -> raise NoRuleApplies
      | (str,tm)::t when isval tm -> (str,tm)::(evalrecord t)
      | (str,tm)::t -> (str, (eval1 vctx tm))::t
    in TmRecord (evalrecord tmr)

| _ ->
    raise NoRuleApplies

;;


let apply_ctx ctx tm = 
List.fold_left (fun t x -> subst x (getbinding ctx x) t) tm (free_vars tm)
;;

let rec eval vctx tm =
try
  let tm' = eval1 vctx tm in
  eval vctx tm'
with
  NoRuleApplies -> apply_ctx vctx tm
;;

let execute (vctx, tctx) = function
  | Eval tm ->
      let tyTm = typeof tctx tm in  (* Determina el tipo del término *)
      let tm' = eval vctx tm in     (* Evalúa el término *)
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ pretty_print tm');  (* Usa el pretty-printer *)
      (vctx, tctx)
  | Bind (s, tm) ->
      let tyTm = typeof tctx tm in  (* Determine the type of term *)
      let tm' = eval vctx tm in  (* Evaluate the term *)
      add_global_def s tm';  (* Add to global context *)
      let tctx' = addbinding tctx s tyTm in  (* Update the type context *)
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (vctx, tctx')  (* Return the new type context *)
      ;;
