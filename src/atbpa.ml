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

  (*List.iter (
      fun i ->
      i#print
    ) content;*)

  (*print_int (List.length content);
  print_string ("-");
  print_int (Hashtbl.length Trace.instruction_table);
  print_string ("\n");*)
 (* Hashtbl.iter (
      fun _ i -> i#print 
    ) Trace.instruction_table;
  print_string ("\n");*)
  (*List.iter (
      fun i -> i#print 
    ) !Trace.function_list;*)

(*  (List.hd !Trace.function_list)#print_trace;
  (List.hd !Trace.function_list)#print_fn_trace 0*)

  let trace = new Trace.trace in
  trace#fill content;
  trace#print_trace_fn !Cmdline.breakpoint;
  trace#print_trace !Cmdline.breakpoint
