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

let isval t = match t with
    TmTrue -> true
  | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false

exception NoRuleApplies

let rec eval t = match t with
    TmTrue -> TmTrue
  | TmFalse -> TmFalse
  | TmZero -> TmZero
  | TmIf(t1, t2, t3) ->
      let t1' = eval t1 in
      (match t1' with
          TmTrue -> eval t2
        | TmFalse -> eval t3
        | _ -> raise NoRuleApplies)
  | TmSucc(t1) when isnumericval t1 -> TmSucc(eval t1)
  | TmPred(t1) ->
      let t1' = eval t1 in
      (match t1' with
          TmZero -> TmZero
        | TmSucc(t2) when isnumericval t2 -> t2
        | _ -> raise NoRuleApplies)
  | TmIsZero(t1) ->
      let t1' = eval t1 in
      (match t1' with
          TmZero -> TmTrue
        | TmSucc(t2) when (isnumericval t2) -> TmFalse
        | _ -> raise NoRuleApplies)
  | _ -> raise NoRuleApplies

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
    let t = TmIf(TmIsZero(TmZero), TmSucc(TmZero), TmTrue) in
    let r = eval t in
    let s = to_string r in
    Printf.printf "%s\n" s;
    exit 0;;
main();;
