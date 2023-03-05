open Format

let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Parser.main Lexer.token lexbuf in
            Syntax.printtm result;
            print_newline();
            result |> Syntax.to_debruijn |> Syntax.print_debruijn;
            print_newline();
            flush stdout
        done
    with Lexer.Eof ->
        exit 0
