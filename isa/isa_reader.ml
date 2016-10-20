open Lexing

module IsaParser = Isa_parser
module IsaLexer  = Isa_lexer
   
let read_isa file =
  let filechan = open_in file in
  let lexbuf = Lexing.from_channel filechan in
  let iset =
    try
      IsaParser.isa IsaLexer.main lexbuf
    with
    | IsaParser.Error  ->
       Printf.fprintf stderr "IsaParser: Error\n\tNear lexeme: %s, at line: %d, column %d-%d.\n"
                      (Lexing.lexeme lexbuf)
                      lexbuf.lex_curr_p.pos_lnum
                      (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
                      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
       raise Exit
    | IsaLexer.SyntaxError msg ->
       Printf.fprintf stderr "IsaLexer: %s\n\tLexeme: %s, at line: %d, column %d-%d.\n"
                      msg
                      (Lexing.lexeme lexbuf)
                      lexbuf.lex_curr_p.pos_lnum
                      (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
                      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
       raise Exit  
  in
  close_in filechan;
  iset
