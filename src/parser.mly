%{
let instruction_table = Hashtbl.create 50000;;

%}

%token <string> PCA PCL
%token <string> INSTRUCTION
%token EOF
		       
%start trace
%type <Trace.trace_instruction list> trace
%type <(string * string option)> pc

%%
  
  trace :
    | l = line; EOF { [l] }
    | l1 = line; l2 = trace { l1 :: l2 }
;;
      
  line :
    | p = pc; i = INSTRUCTION {
    let ins =
        try
      Hashtbl.find instruction_table p
    with
      Not_found ->
      let ins = new Trace.trace_instruction(p, i) in
      Hashtbl.add instruction_table p ins;
      ins
    in
    ins#iter;
    ins
      }
;;

  pc :
    | pa = PCA { (pa, None) }
    | pa = PCA; pl = PCL { (pa, Some pl) }
;;
