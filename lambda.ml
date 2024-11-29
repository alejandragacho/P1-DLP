
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyRecord of (string * ty) list
  (* | TyList of ty *)
;;

type 'a context =
  (string * 'a) list
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
  | TmConcat of term * term
  | TmY (*fixed-point combinator*)
  (* Tuples *)
  | TmTuple of term list
  | TmProjection of term*string
  | TmRecord of (string * term) list
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
  | TyTuple tyr ->
      let rec print = function
          [] -> ""
          | (ty::[]) -> (string_of_ty ty)
          | (ty::t) -> (string_of_ty ty) ^ ", " ^ print t
      in "{" ^ (print tyr) ^ "}"
  | TyRecord tyr ->
      let rec print = function
          [] -> ""
          | ((s, ty)::[]) -> s ^ ":" ^ (string_of_ty ty)
          | ((s, ty)::t) -> s ^ ":" ^ (string_of_ty ty) ^ "," ^ print t
      in "{" ^ (print tyr) ^ "}"
;;

exception Type_error of string
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
       
  | TmY ->
       TyArr (TyNat, TyNat)

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
	  
	(* New rules for string *)
  | TmString _ ->
      TyString
	  
  | TmConcat (t1, t2) ->
      if typeof ctx t1 = TyString && typeof ctx t2 = TyString then TyString
	  else raise (Type_error "argument of concat is not a string")

    (* T-Tuple *)
  | TmTuple tmt ->
      let rec get_types = function
          [] -> []
          | (tm::t) -> ((typeof ctx tm)::(get_types t))
      in TyTuple (get_types tmt)
      
    (* T-Projection *)
   | TmProjection (t, n) ->
      (match (typeof ctx t, n) with
          | (TyRecord (tyr), s) -> 
              (try List.assoc s tyr with
              _ -> raise (Type_error ("Projection error. Key " ^ s ^ " doesn't exist in the record")))
          | (TyTuple (tyr), s) ->
              (try List.nth tyr (int_of_string s - 1) with
              _ -> raise (Type_error ("Projection error. Key " ^ s ^ " doesn't exist in the tuple")))
          | _ -> raise (Type_error ("Projection error. Type can't be projected")))
    
    (* T-Record *)
   | TmRecord tmr ->
       let rec get_types = function
           [] -> []
           | ((s, tm)::t) -> ((s, typeof ctx tm)::(get_types t))
       in TyRecord (get_types tmr) 
;;


(* Pretty-printer for terms *)
let rec string_of_term ?(prec=0) = function
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (t1, t2, t3) ->
      let cond = string_of_term ~prec:0 t1 in
      let then_branch = string_of_term ~prec:0 t2 in
      let else_branch = string_of_term ~prec:0 t3 in
      "if " ^ cond ^ " then " ^ then_branch ^ " else " ^ else_branch
  | TmZero -> "0"
  | TmSucc t ->
    let inner = string_of_term ~prec:3 t in
    if prec > 2 then
      "(succ " ^ inner ^ ")"
    else
      "succ " ^ inner
  | TmPred t ->
      let inner = string_of_term ~prec:3 t in
      "pred " ^ inner
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
  | TmY -> "Y"
  | TmTuple tmt ->
      let rec print = function
          [] -> ""
          | (tm::[]) -> (string_of_term tm)
          | (tm::t) -> (string_of_term tm) ^ ", " ^ print t
      in "{" ^ (print tmt) ^ "}"
  | TmProjection (t, n) ->
      string_of_term t ^ "."  ^ n
  | TmRecord tmr ->
      let rec print = function
          [] -> ""
          | ((s, tm)::[]) -> s ^ "=" ^ (string_of_term tm)
          | ((s, tm)::t) -> s ^ "=" ^ (string_of_term tm) ^ "," ^ print t
      in "{" ^ (print tmr) ^ "}"
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

  | TmY -> []

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
          | (tm::t) -> lunion (free_vars tm) (get_free t)
      in get_free tmt
  | TmProjection (t, n) ->
      free_vars t
  | TmRecord tmr ->
      let rec get_free = function
          [] -> []
          | ((_, tm)::t) -> lunion (free_vars tm) (get_free t)
      in get_free tmr
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

  | TmY -> TmY

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
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple tmt ->
      let rec sub_t = function
          [] -> []
          | (tm::t) -> (subst x s tm)::(sub_t t)
      in TmTuple (sub_t tmt)
  | TmProjection (t, n) ->
      TmProjection (subst x s t, n)
  | TmRecord tmr ->
      let rec sub_r = function
          [] -> []
          | ((str, tm)::t) -> (str, (subst x s tm))::(sub_r t)
      in TmRecord (sub_r tmr)
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
  | t when isnumericval t -> true
  | TmTuple l -> List.for_all(fun t -> isval(t)) l
  | TmRecord [] -> true
  | TmRecord l -> List.for_all(fun (s, t) -> isval(t)) l
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)
    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)
	
    (* E-FixBeta *)	
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2
	  
    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
	  TmFix t1'

  | TmFix (TmAbs (x, _, t)) ->
    subst x tm t
	  
	(* New rules for string *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
	  
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
	  TmConcat (TmString s1, t2')
	  
  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
	  TmConcat (t1', t2)

   (* E-Tuple *)
  | TmTuple tmt ->
      let rec eval_t = function
          [] -> raise NoRuleApplies
          | (tm::t) when isval tm -> tm::(eval_t t)
          | (tm::t) -> (eval1 ctx tm)::t
      in TmTuple (eval_t tmt)

   (* E-Projection *)
  | TmProjection (TmTuple l as v, s) when isval (v) ->
      List.nth l (int_of_string s - 1)
      
  | TmProjection (TmRecord l as v, s) when isval(v) ->
      List.assoc s l
  
  | TmProjection (TmRecord (tmr), n) ->
      List.assoc n tmr

  | TmProjection (t, n) ->
      TmProjection ((eval1 ctx t), n)

  | TmRecord tmr ->
      let rec eval_r = function
          | [] -> raise NoRuleApplies
          | ((str, tm)::t) when isval tm -> (str, tm)::(eval_r t)
          | ((str, tm)::t) -> (str, (eval1 ctx tm))::t
      in TmRecord (eval_r tmr)

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
  Eval tm ->
    let tyTm = typeof tctx tm in
    let tm' = eval ctx tm in
    print_endline("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (ctx, tctx)
  
  | Bind (s, tm) ->
    let tyTm = typeof tctx tm in
    let tm' = eval ctx tm in
    print_endline(s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
    (addbinding ctx s tm', addbinding tctx s tyTm)
;;
