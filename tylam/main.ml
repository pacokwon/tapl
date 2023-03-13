open Format

let _interactive () =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            Printf.printf "> %!";
            let tterm = Parser.main Lexer.token lexbuf in
            Printf.printf ">> ";
            Syntax.print_typed_term tterm;
            print_newline ();
            Printf.printf ">> ";
            Syntax.typecheck tterm |> ignore;
            let debruijn = tterm
            |> Syntax.erase
            |> Syntax.to_debruijn in
            Syntax.print_debruijn debruijn;
            print_newline ();
            Printf.printf ">> ";
            debruijn
            |> Syntax.evaluate
            |> Syntax.print_debruijn;
            print_newline ();
        done
    with
    | Lexer.Eof -> exit 0
    | Parsing.Parse_error -> exit 1

let _ =
    _interactive ()
