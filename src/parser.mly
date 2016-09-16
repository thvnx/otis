%{
(* ocaml code *)      
%}

%token <string> PC
%token <string> INSTRUCTION
%token EOF
		       
%start file /* the entry point */
%type <(string * string) list> file

%%
  
  file :
    | l = line; EOF { [l] }
    | l1 = line; l2 = file { l1 :: l2 }
;;
      
  line :
    | pc = PC; i = INSTRUCTION { (pc, i) }
;;
