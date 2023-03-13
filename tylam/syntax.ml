module Context = Map.Make(String)

exception NoRuleApplies

type term =
    | TmTrue
    | TmFalse
    | TmCond of term * term * term
    | TmVar of string
    | TmAbs of string * term
    | TmApp of term * term

type debruijn_term =
    | DeTrue
    | DeFalse
    | DeCond of debruijn_term * debruijn_term * debruijn_term
    | DeVar of int
    | DeAbs of debruijn_term
    | DeApp of debruijn_term * debruijn_term

type typ =
    | Bool
    | Arrow of typ * typ

type typed_term =
    | TypedTrue
    | TypedFalse
    | TypedCond of typed_term * typed_term * typed_term
    | TypedVar of string
    | TypedAbs of string * typ * typed_term
    | TypedApp of typed_term * typed_term

let rec print_term = function
    | TmTrue -> print_string "true"
    | TmFalse -> print_string "false"
    | TmCond(cond, t, f) ->
            print_string "if";
            print_term cond;
            print_string " then ";
            print_term t;
            print_string " else ";
            print_term f
    | TmVar(v) ->
            print_string v
    | TmAbs(v, t) ->
            print_string "λ";
            print_string v;
            print_string ".";
            print_term t
    | TmApp(t1, t2) ->
            print_term t1;
            print_string " ";
            print_term t2

let rec print_type = function
    | Bool -> print_string "bool"
    | Arrow(t1, t2) ->
            print_type t1;
            print_string "->";
            print_type t2

let rec print_typed_term = function
    | TypedTrue -> print_string "true"
    | TypedFalse -> print_string "false"
    | TypedCond(cond, t, f) ->
            print_string "if ";
            print_typed_term cond;
            print_string " then ";
            print_typed_term t;
            print_string " else ";
            print_typed_term f
    | TypedVar(v) -> print_string v
    | TypedAbs(v, t, body) ->
            print_string "λ";
            print_string v;
            print_string ":";
            print_type t;
            print_string ".";
            print_typed_term body
    | TypedApp(t1, t2) ->
            print_typed_term t1;
            print_string " ";
            print_typed_term t2

let rec print_debruijn = function
    | DeTrue -> print_string "true"
    | DeFalse -> print_string "false"
    | DeCond(cond, t, f) ->
            print_string "if ";
            print_debruijn cond;
            print_string " then ";
            print_debruijn t;
            print_string " else ";
            print_debruijn f
    | DeVar(i) ->
            print_int i
    | DeAbs(dt) ->
            print_string "λ";
            print_string ".";
            print_debruijn dt
    | DeApp(t1, t2) ->
            print_string "(";
            print_debruijn t1;
            print_string " ";
            print_debruijn t2;
            print_string ")"

let rec typecheck_helper ctx = function
    | TypedTrue -> Bool
    | TypedFalse -> Bool
    | TypedCond(cond, t, f) ->
        (match typecheck_helper ctx cond with
            | Arrow(_, _) -> failwith "Arrow type not allowed in conditional."
            | Bool ->
                    let tt = typecheck_helper ctx t in
                    let ft = typecheck_helper ctx f in
                    if tt = ft then
                        tt
                    else
                        failwith "Not")
    | TypedVar(v) ->
        (match Context.find_opt v ctx with
        | Some t -> t
        | None -> failwith "Variable not found in type environment.")
    | TypedAbs(v, typ, body) ->
        let ctx' = Context.add v typ ctx in
        typecheck_helper ctx' body
    | TypedApp(t1, t2) ->
        match typecheck_helper ctx t1 with
        | Bool -> failwith "LHS of a function application must be an arrow type"
        | Arrow(tp1, tp2) ->
            if tp1 = typecheck_helper ctx t2 then
                tp2
            else
                failwith "RHS of a function application must match the input type of the LHS"

let typecheck = typecheck_helper Context.empty

let rec erase = function
    | TypedTrue -> TmTrue
    | TypedFalse -> TmFalse
    | TypedCond(cond, t, f) -> TmCond(erase cond, erase t, erase f)
    | TypedVar(v) -> TmVar(v)
    | TypedAbs(v, _, tt) -> TmAbs(v, erase tt)
    | TypedApp(t1, t2) -> TmApp(erase t1, erase t2)

let to_debruijn t =
    let ctx = Context.empty in
    let rec helper ctx depth = function
        | TmTrue -> DeTrue
        | TmFalse -> DeFalse
        | TmCond(cond, t, f) ->
                DeCond(helper ctx depth cond, helper ctx depth t, helper ctx depth f)
        | TmVar(n) ->
                let index = match Context.find_opt n ctx with
                | Some i -> i
                | None -> failwith "Variable name not found in context." in
                DeVar(depth - 1 - index)
        | TmAbs(name, term) ->
                (match Context.find_opt name ctx with
                    | Some _ -> failwith "Variable name conflict"
                    | None -> ());
                let new_ctx = Context.add name depth ctx in
                DeAbs(helper new_ctx (depth + 1) term)
        | TmApp(t1, t2) ->
                let dt1 = helper ctx depth t1 in
                let dt2 = helper ctx depth t2 in
                DeApp(dt1, dt2)
    in
    helper ctx 0 t

let rec d_place_shift d cutoff = function
    | DeTrue -> DeTrue
    | DeFalse -> DeFalse
    | DeCond(cond, t, f) ->
            DeCond(d_place_shift d cutoff cond, d_place_shift d cutoff t, d_place_shift d cutoff f)
    | DeVar(k) as v ->
            if k < cutoff then v else DeVar(k + d)
    | DeAbs(t) ->
            DeAbs(d_place_shift d (cutoff + 1) t)
    | DeApp(t1, t2) ->
            DeApp(d_place_shift d cutoff t1, d_place_shift d cutoff t2)

let is_value = function
    | DeAbs(_) -> true
    | _ -> false

let substitute_top body argument =
    let rec helper depth body argument = match body with
        | DeTrue -> DeTrue
        | DeFalse -> DeFalse
        | DeCond(cond, t, f) ->
                DeCond(helper depth cond argument, helper depth t argument, helper depth f argument)
        | DeVar(index) as v ->
                if depth = index then
                    argument
                else
                    v
        | DeAbs(term) ->
                DeAbs(helper (depth + 1) term (d_place_shift 1 0 argument))
        | DeApp(t1, t2) ->
                DeApp(helper depth t1 argument, helper depth t2 argument)
    in
    helper 0 body argument

(* Debug substitute_top *)
let rec evaluate_debruijn_step = function
    | DeApp(DeAbs(t12), v2) as t when is_value v2 ->
            print_debruijn t;
            print_newline();
            substitute_top t12 v2
    | DeApp(v1, t2) as t when is_value v1 ->
            print_debruijn t;
            print_newline();
            let t2' = evaluate_debruijn_step t2 in
            DeApp(v1, t2')
    | DeApp(t1, t2) as t ->
            print_debruijn t;
            print_newline();
            let t1' = evaluate_debruijn_step t1 in
            DeApp(t1', t2)
    | _ -> raise NoRuleApplies

let rec evaluate t =
    try
        let t' = evaluate_debruijn_step t in
        evaluate t'
    with NoRuleApplies -> t
