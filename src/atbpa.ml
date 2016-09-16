open Lexing

let _ =
  let filechan = open_in "./trace.trc" in
  let lexbuf   = Lexing.from_channel filechan in
  let content  =
    try
      Parser.file Lexer.main lexbuf
    with
    | Parsing.Parse_error ->
       failwith ("error-> parsing of lexeme: " ^ (Lexing.lexeme lexbuf)
                 ^ ", at line: " ^ (string_of_int lexbuf.lex_curr_p.pos_lnum)
                 ^ ", column: "  ^ (string_of_int lexbuf.lex_curr_p.pos_bol));
    | Lexer.Eof ->
       failwith ("Lexing error during parsing: Eof")
  in
  close_in filechan;
