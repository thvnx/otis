%{

%}

%token EOF EOL UNALLOC
%token <string * string> CATEGORY
%token <string> INS_OPCODE INS_NAME
%token READ WRITE BASE IMM OPT_READ OPT_IMM
%token <int> LATENCY
%token <Isa.branch_t> BRANCH_PROP


%start isa
%type <Isa.instruction_set> isa
%type <Isa.instruction list> line
%type <Isa.property_t list> properties
                                       
%%
                                 
  isa:
    | EOF { new Isa.instruction_set }
    | l = line; i = isa {
                        List.iter ( fun ins -> i#add_instruction ins ) l;
                        i
                        }
    ;
            
  line:
    | c = CATEGORY; EOL { [] }
    | EOL { [] }
    | io = INS_OPCODE; UNALLOC; EOL { [] }
    | io = INS_OPCODE; ip = instruction { let (n, prm, prp) = ip in [ (new Isa.instruction io n prm prp) ] }
    | io = INS_OPCODE; ip = instruction; ap = alias {
                                                  let (i, prmi, prp) = ip in
                                                  let ii = new Isa.instruction io i prmi prp in
                                                  let aliases = ref [] in
                                                  List.iter (
                                                      fun i ->
                                                      let (a, pa) = i in
                                                      aliases := (new Isa.instruction ~alias_of:ii io a pa prp) :: !aliases
                                                    ) ap;
                                                  ii :: !aliases }
    ;


  instruction:
    | p = properties; n = INS_NAME; EOL { (n, [], p) }
    | p = properties; n = INS_NAME; s = structure; EOL { (n, List.rev s, p) }

  alias:
    | n = INS_NAME; s = structure; EOL { [(n, List.rev s)] }
    | n = INS_NAME; s = structure; EOL; a = alias { (n, List.rev s) :: a }
    ;

  properties:
    | { [] }
    | l = properties; p = property { p :: l } 
    ;

  property:
    | p = BRANCH_PROP { Isa.BranchProp (p) }
    | p = LATENCY { Isa.Latency (p) }
    ;


  structure:
    | p = parameter { [p] }
    | s = structure; p = parameter { p :: s };

  parameter:
    | READ { Isa.Read }
    | WRITE { Isa.Write }
    | IMM { Isa.Imm }
    | OPT_READ { Isa.OptionRead }
    | OPT_IMM { Isa.OptionImm }
    | BASE { Isa.Base }
