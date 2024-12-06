
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyUnit
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  | TyList of ty 
;;

type 'a context =
  (string * 'a) list
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
| TmString of string
| TmUnit
| TmProjection of term * string
| TmRecord of (string * term) list
| TmNil of ty
| TmCons of ty * term * term
| TmIsNil of ty * term
| TmHead of ty * term
| TmTail of ty * term
| TmTuple of term list
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
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyString ->
      "String"
  | TyUnit ->
    "Unit"
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
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding ctx x tyT1 in
      typeof ctx' t2
	  
    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
	  (match tyT1 with
	       TyArr (tyT11, tyT12) ->
		     if tyT11 = tyT12 then tyT12
			 else raise (Type_error "result of body not compatible with domain")
	  | _ -> raise (Type_error "arrow type expected"))

  | TmUnit ->
    TyUnit
	  
    (* New rules for string *)
  | TmString _ ->
      TyString
      
    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
	  else raise (Type_error "argument of concat is not a string")

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

  | TmNil ty -> TyList ty
    (* T-Cons *)
  | TmCons (ty,h,t) ->
      let tyTh = typeof ctx h in
          let tyTt = typeof ctx t in
             if (subtypeof tyTh ty) && (subtypeof tyTt (TyList(ty))) then 
              TyList(ty) else raise (Type_error "elements of list have different types")
          
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

(* Pretty-printer for terms *)
let rec string_of_term ?(prec=0) = function
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t1, t2, t3) ->
      let cond = string_of_term ~prec:0 t1 in
      let then_branch = string_of_term ~prec:0 t2 in
      let else_branch = string_of_term ~prec:0 t3 in
      "if " ^ cond ^ " then " ^ then_branch ^ " else " ^ else_branch
  | TmZero ->
      (try
          string_of_int (term_to_int TmZero)
      with Type_error _ -> "0")    
  | TmSucc t ->
      (try
          string_of_int (term_to_int (TmSucc t))
      with Type_error _ ->
          let inner = string_of_term ~prec:3 t in
          if prec > 2 then
              "(succ " ^ inner ^ ")"
          else
              "succ " ^ inner)
  | TmPred t ->
      let inner = string_of_term ~prec:3 t in
      "pred " ^ inner
  | TmUnit ->
    "()"
  | TmIsZero t ->
      let inner = string_of_term ~prec:3 t in
      "iszero " ^ inner
  | TmVar s -> s
  | TmAbs (s, tyS, t) ->
      let body = string_of_term ~prec:0 t in
      "(λ" ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ body ^ ")"
  |  TmApp (t1, t2) ->
      let left = string_of_term ~prec:2 t1 in
      let right = string_of_term ~prec:3 t2 in
      if prec > 1 then "(" ^ left ^ " " ^ right ^ ")" else left ^ " " ^ right
  | TmLetIn (s, TmFix (TmAbs (id, ty, body)), t2) ->
     "letrec " ^ s ^ " : " ^ string_of_ty ty ^ " = " ^ string_of_term ~prec:0 body ^
     " in\n" ^ string_of_term ~prec:0 t2
  | TmFix t ->
      let inner = string_of_term ~prec:3 t in
      "fix " ^ inner
  | TmString s -> "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      let left = string_of_term ~prec:2 t1 in
      let right = string_of_term ~prec:2 t2 in
      "concat(" ^ left ^ ", " ^ right ^ ")"
  | TmTuple tmt ->
      let rec print = function
          [] -> ""
          | tm::[] -> string_of_term tm
          | tm::t -> string_of_term tm ^ ", " ^ print t
      in "{" ^ print tmt ^ "}"
  | TmProjection (TmTuple l, n) ->
      string_of_term (List.nth l (int_of_string n - 1))
  | TmProjection (t, n) ->
      string_of_term t ^ "." ^ n
  | TmRecord tmr ->
      let rec print = function
          [] -> ""
          | (s, tm)::[] -> s ^ " = " ^ string_of_term tm
          | (s, tm)::t -> s ^ " = " ^ string_of_term tm ^ ", " ^ print t
      in "{" ^ print tmr ^ "}"
  | TmNil ty -> "nil[" ^string_of_ty ty ^ "]"
  | TmCons (ty, h, t) -> "cons[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term h ^ " " ^ (string_of_term t) ^ ")"
  | TmIsNil (ty, t) -> "isnil[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")" 
  | TmHead (ty, t) -> "head[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")" 
  | TmTail (ty, t) -> "tail[" ^string_of_ty ty ^ "] " ^ "(" ^ string_of_term t ^ ")"

  | _ -> "<unknown term>"

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmUnit ->
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
  | TmString _ ->
      []
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

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
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
  | TmString st ->
      TmString st
  | TmUnit ->
      tm
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
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
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmUnit -> true
  | t when isnumericval t -> true
  | TmTuple l -> List.for_all(fun t -> isval(t)) l
  | TmRecord [] -> true
  | TmNil _ -> true
  | TmRecord l -> List.for_all(fun (s, t) -> isval(t)) l
  | TmString _ -> true
  | _ -> false
;;

exception NoRuleApplies
;;
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

  (* E-Concat *)
| TmConcat (TmString s1, TmString s2) -> 
    TmString (s1 ^ s2)

| TmConcat (TmString s1, t2) ->
    let t2' = eval1 vctx t2 in
        TmConcat (TmString s1, t2')

| TmConcat (t1, t2) ->
    let t1' = eval1 vctx t1 in
        TmConcat (t1', t2)

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
| TmFix (TmAbs (x, _, t2)) ->
    subst x tm t2 
  
  (* E-Fix *)
| TmFix t1 ->   (* when t1 isn't yet an abstraction, we evaluate t1 to get t1' *)
    let t1' = eval1 vctx t1 in
    TmFix t1'

| TmVar id ->
    (try get_global_def id
    with Type_error _ -> raise (Type_error ("Unbound variable: " ^ id)))

  (* E-Cons2 *)
| TmCons(ty, h, t) when isval h -> TmCons(ty, h, (eval1 vctx t))

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

  (* E-Head *)
| TmHead (ty, t) ->
  TmHead (ty, eval1 vctx t)

  (* E-TailCons *)
| TmTail (_, t) when isval t ->
  get_tail t

  (* E-Tail *)
| TmTail(ty,t) -> TmTail(ty,eval1 vctx t)

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

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;


let execute (ctx, tctx) = function
  | Eval tm ->
      let tyTm = typeof tctx tm in
      let tm' = eval ctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (ctx, tctx)
  | Bind (s, tm) ->
      let tyTm = typeof tctx tm in  (* Determine the type of term *)
      let tm' = eval ctx tm in  (* Evaluate the term *)
      add_global_def s tm';  (* Add to global context *)
      let tctx' = addbinding tctx s tyTm in  (* Update the type context *)
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (ctx, tctx')  (* Return the new type context *)
      ;;
