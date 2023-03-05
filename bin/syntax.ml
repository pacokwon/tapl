module Context = Map.Make(String)

type term =
    | TmVar of string
    | TmAbs of string * term
    | TmApp of term * term

type debruijn_term =
    | DeVar of int
    | DeAbs of int * debruijn_term
    | DeApp of debruijn_term * debruijn_term

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
                DeAbs(depth, helper new_ctx (depth + 1) term)
        | TmApp(t1, t2) ->
                let dt1 = helper ctx depth t1 in
                let dt2 = helper ctx depth t2 in
                DeApp(dt1, dt2)
    in
    helper ctx 0 t

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
    | DeAbs(i, dt) ->
            print_string "λ";
            print_int i;
            print_string ".";
            print_debruijn dt
    | DeApp(t1, t2) ->
            print_debruijn t1;
            print_string " ";
            print_debruijn t2

