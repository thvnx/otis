{
  open Parser
  open Lexing
  exception Eof

  let debug_ = false

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

let label = '<' (alpha|decimalDigit|'_')+ ('.' decimalDigit+)? ('@' alpha+)? ('+' (decimalDigit)+)? '>'
                  
let pca = "0x" (hexaDigit)+ (*(space label)?*)
let pcl = label
let comment = "\t// " '#' '-'? decimalDigit*
let instruction = (alpha|decimalDigit|(comma space)|comma|tab|label|'#'|" #"|'.'|'_'|'-'|'+'|'['|']'|'{'|'}'|'!'|(pca space pcl))+ (space* comment)? '\n' 
  
rule main = parse
              
  | (space | tab) {
    if(debug_) then debug lexbuf "space-or-tab";
    main lexbuf
  }

 (* | newline {
    (* debug lexbuf "newline"; *)
    Lexing.new_line lexbuf;
    EOL
  }*)

  | pca as str {
    if(debug_) then debug lexbuf "pca";
    PCA( str )
             }

                 | pcl as str {
    if(debug_) then debug lexbuf "pcl";
    PCL( str )
  }

  | instruction as str {
    if(debug_) then debug lexbuf "instruction";
    Lexing.new_line lexbuf;                   
    INSTRUCTION( str )
  }                        
             
 
  | eof  {
    if(debug_) then debug lexbuf "eof";
    EOF
  }		 

  | _ as lxm
    { print_string (error lexbuf ("Unknown token:" ^ (Char.escaped lxm))); exit 0; }
