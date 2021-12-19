type term =
    TmTrue
  | TmFalse
  | TmIf of term  * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term

let rec isnumericval t = match t with
    TmZero -> true
  | TmSucc(t1) -> isnumericval t1
  | _ -> false

let rec isval t = match t with
    TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false

exception NoRuleApplies

(* 1step評価 *)
let rec eval1 t = match t with
    TmIf(TmTrue, t2, t3) ->
      t2
  | TmIf(TmFalse, t2, t3) ->
      t3
  | TmIf(t1, t2, t3) ->
      let t1' = eval1 t1 in
      TmIf(t1', t2, t3)
  | TmSucc(t1) ->
      let t1' = eval1 t1 in
      TmSucc(t1')
  | TmPred(TmZero) ->
      TmZero
  | TmPred(TmSucc(nv1)) when (isnumericval nv1) ->
      nv1
  | TmPred(t1) ->
      let t1' = eval1 t1 in
      TmPred(t1')
  | TmIsZero(TmZero) ->
      TmTrue
  | TmIsZero(TmSucc(nv1)) when (isnumericval nv1) ->
      TmFalse
  | TmIsZero(t1) ->
      let t1' = eval1 t1 in
      TmIsZero(t1')
  | _ ->
      raise NoRuleApplies

let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t

let rec to_string t = match t with
    TmTrue -> "True"
  | TmFalse -> "False"
  | TmIf(t1, t2, t3) ->
    "If " ^ to_string t1 ^ " then " ^ to_string t2 ^ " else " ^ to_string t3
  | TmZero -> "0"
  | TmSucc(t1) ->
    "Succ(" ^ to_string t1 ^ ")"
  | TmPred(t1) ->
    "Pred(" ^ to_string t1 ^ ")"
  | TmIsZero(t1) ->
    "IsZero(" ^ to_string t1 ^ ")"


let main() =
    let t  = TmIf(TmFalse, TmTrue, TmFalse) in
    let r = eval t in
    let s = to_string r in
    Printf.printf "%s\n" s;
    exit 0;;
main();;

