open Lexing
 
let _ =
  Cmdline.scan_cmd_line;
  
  let filechan = open_in !Cmdline.trace_file in
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

  if !Cmdline.trace_function_graphviz then trace#dot_trace_fn !Cmdline.trace_breakpoint;
  if !Cmdline.trace_instruction_graphviz then trace#dot_trace !Cmdline.trace_breakpoint;
  if !Cmdline.trace_function_text then trace#print_trace_fn !Cmdline.trace_breakpoint;
  if !Cmdline.trace_function_tex then trace#tikz_trace !Cmdline.trace_breakpoint;
  if !Cmdline.trace_instruction_text then trace#print_trace !Cmdline.trace_breakpoint;
  
  (*  if !Cmdline.nb_instruction then trace#print_nb_instruction !Cmdline.breakpoint;*)
  if !Cmdline.trace_run_analysis then trace#exec !Cmdline.trace_breakpoint;

  if !Cmdline.dump_analysis_pipeline then trace#tikz_pipeline !Cmdline.trace_breakpoint
