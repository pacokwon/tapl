open Format

let _interactive () =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let named = Parser.main Lexer.token lexbuf in
            let debruijn = named |> Syntax.to_debruijn in
            named |> Syntax.printtm;
            print_newline ();
            debruijn |> Syntax.evaluate |> Syntax.print_debruijn;
            print_newline ();
            flush stdout;
        done
    with Lexer.Eof ->
        exit 0

let _test () =
    try
        let debruijn = Syntax.DeApp(Support.succ, Support.zero) in
        debruijn |> Syntax.print_debruijn;
        print_newline();
        debruijn |> Syntax.evaluate |> Syntax.print_debruijn;
        print_newline();
        flush stdout
    with Lexer.Eof ->
        exit 0

let _ =
    _interactive ()
