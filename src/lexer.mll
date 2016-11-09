{
  open Parser
  open Lexing
  exception SyntaxError of string
}

let hexa_number = "0x" ['0'-'9''a'-'f']+
let function_name = ['_''a'-'z''A'-'Z'] ['_''a'-'z''A'-'Z''0'-'9''.']*
let instruction_name = ' ' ['.''a'-'z']+ ['1''2']?
let register_name = ['x''w''q''d''v''s']
let special_register = "sp" | "xzr" | "wzr" | "fpcr" | "tpidr_el0"             
let integer = ['+''-']? ['0'-'9']+
let shift = ("ls" ['r''l']) | "asr"
let vector_register = '.' ("1"|"2"|"4"|"8"|"16") ['b''h''s''d']
let vector_element = '.' ['b''h''s''d']
let condition = "ne" | "eq" | "cs" | "cc" | "ls" | "hi" | "le" | "pl" | "lt" | "gt" | "mi" | "ge"
let extend = ['u''s'] "xt" ['b''h''w''x']
let float = ['0'-'9'] '.' ['0'-'9']* ('e' integer)?
          
rule main =
  parse
  | ' ' | '\t' { main lexbuf }
  | '\n' { Lexing.new_line lexbuf; EOL }
  | ", " { PARAM_SEP }
  | ',' { COMMA }
  | '<' { label ("", false, None) lexbuf }              
  | '[' { L_SQUARE }
  | ']' { R_SQUARE }
  | '{' { L_BRACE }
  | '}' { R_BRACE }
  | '!' { EXCLAM }
  | ':' { COLON }
  | '#' { immediate lexbuf }

  | "//" [^'\n']* as cmt
                 { (*Printf.printf "comment: %s\n" cmt;*) COMMENT (cmt) }
  | integer as i
                 { (*Printf.printf "integer: %s\n" i;*) NUMBER (int_of_string i) }
  | hexa_number as hn
                     { (*Printf.printf "hexa_number: %s\n" hn;*) HEXADDR (Int64.of_string hn) }
  | extend as ex
                { (*Printf.printf "extend: %s\n" ex;*) EXTEND (ex) }
  | register_name as rn
                       { (*Printf.printf "register_name: %c\n" rn;*) REGISTER_TYPE (Char.escaped rn) }
  | vector_element as ve
                        { (*Printf.printf "vector_element: %s\n" ve;*) VECTOR_ELEMENT (ve) }
  | vector_register as vr
                         { (*Printf.printf "vector_register %s\n" vr;*) VECTOR_REGISTER (vr) }
  | condition as cn
                   { (*Printf.printf "condition: %s\n" cn;*) CONDITION (cn) }
  | special_register as sr
                          { (*Printf.printf "special_register: %s\n" sr;*) REGISTER_SPECIAL (sr) }
  | shift as ls
               { (*Printf.printf "shift:%s\n" ls;*) SHIFT_N_ROTATE (ls) }
  | instruction_name as inn
                          { (*Printf.printf "instruction_name: %s\n" inn;*) INSTRUCTION_NAME (inn) }
  | _ { raise (SyntaxError ("Illegal character")) }
  | eof { EOF }
and immediate =
  parse
  | float as f
               { (*Printf.printf "imm. float: %s\n" f;*) IMMEDIATE_FLOAT (float_of_string f) }
  | integer as i
                 { (*Printf.printf "imm. integer: %s\n" i;*) IMMEDIATE_DECIMAL (Int64.of_string i) }
  | hexa_number as hn
                     { (*Printf.printf "imm. hexa_number: %s\n" hn;*) IMMEDIATE_HEXA (Int64.of_string hn) }
  | _   { raise (SyntaxError ("Illegal character (immediate)")) }
  | eof { raise (SyntaxError ("Immadiate isn't terminated")) }
and label l =
  parse
  | '(' [^')']* ')' { label l lexbuf }
  | function_name as fn
                       { (*Printf.printf "function_name: %s\n" fn;*) label (fn, false, None) lexbuf }
  | "@plt" { match l with (f, _, o) -> label (f, true, o) lexbuf }
  | '+' { label l lexbuf }
  | ['0'-'9']* as i
                 { match l with (f, p, _) -> label (f, p, Some (int_of_string i)) lexbuf }
  | '>' { LABEL (l) }
  | _ { raise   (SyntaxError ("Illegal character label")) }
  | eof { raise (SyntaxError ("Label isn't terminated")) }
