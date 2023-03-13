module Context = Map.Make(String)

exception NoRuleApplies

type term =
    | TmVar of string
    | TmAbs of string * term
    | TmApp of term * term

type debruijn_term =
    | DeVar of int
    | DeAbs of debruijn_term
    | DeApp of debruijn_term * debruijn_term

let rec printtm = function
    | TmVar(v) ->
            print_string v
    | TmAbs(v, t) ->
            print_string "λ";
            print_string v;
            print_string ".";
            printtm t
    | TmApp(t1, t2) ->
            printtm t1;
            print_string " ";
            printtm t2

let rec print_debruijn = function
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

let to_debruijn t =
    let ctx = Context.empty in
    let rec helper ctx depth = function
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
