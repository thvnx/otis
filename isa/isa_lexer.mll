{
  open Lexing
  exception SyntaxError of string

  open Isa_parser

  let i_name = ref ""
}

let destination = (['W''X''S''D''Q''B''H'] 'd') | (('V' "><"? ['d''b'] '>') (".<T"('>'|"s>[<index" ['1''2']? ">]"))?)
let source = ['W''X''S''D''Q''B''H'] ['n''m''s''a'] | (('V' "><"? ['n''m''a'] '>') (".<T"('>'|"s>[<index" ['1''2']? ">]"))?) | "R><n"
let transfert_or_test = ['W''X''S''D''Q''B''H'] 't'
let immediate = "label" | "imm" ['r''s']? | "lsb" | "width" | "pstatefield" | "cond" | "option" | "systemreg" | "shift" | "prfop"

rule main =
  parse
  | [' ''\t''('] { main lexbuf }               
  | "Unallocated" { UNALLOC }
  | ['x''0''1']+ '\t' as oc { category (oc,"") lexbuf }
  | (['-''0''1'')']|"!=(")+ as oc { INS_OPCODE (oc) }
  | ['.''A'-'Z']* ['1'-'9']*? ('C'?['B''H''W''X'])? as i { let name = (String.lowercase (String.trim i)) in i_name := name; INS_NAME name }
  | '\n' { Lexing.new_line lexbuf; EOL }
  | '<' { value lexbuf }
  | [','] { main lexbuf }

  | '#' [^',''\n''{']* { IMM }
  | "{, "|'{' { value_option lexbuf }
  | '[' [^'\n']* { BASE }
  | "{ " { vector lexbuf }

  | ':' { properties lexbuf }
  
  | _ { raise (SyntaxError ("Illegal character")) }
  | eof { EOF }
and category ct =
  parse
  | '\t' { category ct lexbuf }
  | [^'\n''\t']* as cat
                 { match ct with (oc, _) -> CATEGORY (oc, cat) }
  | _   { raise (SyntaxError ("Illegal character (category)")) }
  | eof { raise (SyntaxError ("category isn't terminated")) }
and value =
  parse
  | destination [^',''\n''{']* { WRITE }
  | source [^',''\n''{']* { READ }
  | immediate [^',''\n''{']* { IMM }
  | transfert_or_test [^',''\n''{']* { try let tmp = String.sub !i_name 0 2 in
                                          match tmp with
                                          | "ld" -> WRITE
                                          | _ -> READ
                                      with Invalid_argument _ -> READ
                                    }
  | _   { raise (SyntaxError ("Illegal character (value)")) }
  | eof { raise (SyntaxError ("value isn't terminated")) }
and value_option =
  parse
  | '<' source [^',''\n''{']* { OPT_READ }
  | "LSL #<" immediate [^',''\n''{']* 
  | '#'? '<' immediate [^',''\n''{']* { OPT_IMM }
  | _   { raise (SyntaxError ("Illegal character (value_option)")) }
  | eof { raise (SyntaxError ("value_option isn't terminated")) }
and vector =
  parse
  | "<Vt>" [^'}']* '}' {  try let tmp = String.sub !i_name 0 2 in
                          match tmp with
                          | "ld" -> WRITE
                          | _ -> READ
                      with Invalid_argument _ -> READ }
  | _   { raise (SyntaxError ("Illegal character (vector)")) }
  | eof { raise (SyntaxError ("vector isn't terminated")) }
and properties =
  parse
  | ':'
  | ' ' { main lexbuf }
  | "Jump" { BRANCH_PROP (Isa.Jump) }
  | "Return" { BRANCH_PROP (Isa.Return) }
  | "Call" { BRANCH_PROP (Isa.Call) }
  | _   { raise (SyntaxError ("Illegal character (properties)")) }
  | eof { raise (SyntaxError ("properties isn't terminated")) }
