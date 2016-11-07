open Lexing
 
let _ =
  Cmdline.scan_cmd_line;
  
  let filechan = open_in !Cmdline.file in
  let lexbuf   = Lexing.from_channel filechan in
  let trace  =
    try
      Parser.trace Lexer.main lexbuf
    with
    | Parser.Error  ->
       Printf.fprintf stderr "Parser: Error\n\tNear lexeme: %s, at line: %d, column %d-%d.\n"
                      (Lexing.lexeme lexbuf)
                      lexbuf.lex_curr_p.pos_lnum
                      (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
                      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
       raise Exit
    | Lexer.SyntaxError msg ->
       Printf.fprintf stderr "Lexer: %s\n\tLexeme: %s, at line: %d, column %d-%d.\n"
                      msg
                      (Lexing.lexeme lexbuf)
                      lexbuf.lex_curr_p.pos_lnum
                      (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
                      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
       raise Exit  
  in
  close_in filechan;

  (*trace#isa#print; *)

  if !Cmdline.cfg_graphviz then trace#dot_trace_fn !Cmdline.breakpoint;
  if !Cmdline.trace_graphviz then trace#dot_trace !Cmdline.breakpoint;
  if !Cmdline.cfg_text then trace#print_trace_fn !Cmdline.breakpoint;
  if !Cmdline.cfg_tikz then trace#tikz_trace !Cmdline.breakpoint;
  if !Cmdline.trace_text then trace#print_trace !Cmdline.breakpoint;
  
  if !Cmdline.nb_instruction then trace#print_nb_instruction !Cmdline.breakpoint;
  if !Cmdline.exec then trace#exec !Cmdline.breakpoint;

  if !Cmdline.pipeline_tikz then trace#tikz_pipeline !Cmdline.breakpoint
