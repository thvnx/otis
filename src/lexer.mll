{
  open Parser
  open Lexing
  exception Eof

  let error lexbuf msg =
    " LEXER: " ^ msg ^ "\t\tline:" ^ (string_of_int lexbuf.lex_curr_p.pos_lnum) ^ "\n"

  let debug lexbuf msg =
    print_string (" -> debug(" ^ msg ^ "), lexeme:" ^ (Lexing.lexeme lexbuf) ^ "\n")
}

let tab = '\t'
let newline = '\n'
let space = ' '
let comma = ','
let alpha = ['a'-'z''A'-'Z']
let decimalDigit = ['0'-'9']
let hexaDigit = ['0'-'9''a'-'f'] 

let pc = "0x" (hexaDigit)+ (space '<' (alpha|decimalDigit|'_')+ ('@' alpha+)? ('+' (decimalDigit)+)? '>')?
let instruction = space (alpha|decimalDigit|comma|tab|'#'|space|"//"|'.'|'<'|'>'|'_'|'+'|'-'|'@'|'['|']'|'{'|'}'|'!')+ '\n' 
  
rule main = parse
              
  | (space | tab) {
    debug lexbuf "space-or-tab";
    main lexbuf
  }

 (* | newline {
    (* debug lexbuf "newline"; *)
    Lexing.new_line lexbuf;
    EOL
  }*)

  | pc as str {
    debug lexbuf "pc";
    PC( str )
  }

  | instruction as str {
    debug lexbuf "instruction";
    Lexing.new_line lexbuf;                   
    INSTRUCTION( str )
  }                        
             
 
  | eof  {
    debug lexbuf "eof";
    EOF
  }		 

  | _ as lxm
    { print_string (error lexbuf ("Unknown token:" ^ (Char.escaped lxm))); exit 0; }
