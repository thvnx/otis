open Lexing
   
let _ =
  Cmdline.scan_cmd_line;
  let filechan = open_in !Cmdline.file in
  let lexbuf   = Lexing.from_channel filechan in
  let content  =
    try
      Parser.trace Lexer.main lexbuf
    with
    | Parsing.Parse_error ->
       failwith ("error-> parsing of lexeme: " ^ (Lexing.lexeme lexbuf)
                 ^ ", at line: " ^ (string_of_int lexbuf.lex_curr_p.pos_lnum)
                 ^ ", column: "  ^ (string_of_int lexbuf.lex_curr_p.pos_bol));
    | Lexer.Eof ->
       failwith ("Lexing error during parsing: Eof")
  in
  close_in filechan;

  let trace = new Trace.trace in
  trace#fill content; (* build trace IR *)

  if !Cmdline.cfg_graphviz then trace#dot_trace_fn !Cmdline.breakpoint;
  if !Cmdline.trace_graphviz then trace#dot_trace !Cmdline.breakpoint;
  if !Cmdline.cfg_text then trace#print_trace_fn !Cmdline.breakpoint;
  if !Cmdline.trace_text then trace#print_trace !Cmdline.breakpoint;
  
  (*trace#print_nb_instruction !Cmdline.breakpoint;*)
  
  (*trace#exec !Cmdline.breakpoint;*)
