%{
    open Aarch64
                           
    let instruction_table = Hashtbl.create 50000
    let isa = Isa_reader.read_isa !Cmdline.isa_description_file
    let trc = new Trace.trace isa
%}

%token <int64> HEXADDR IMMEDIATE_HEXA IMMEDIATE_DECIMAL
%token <string> INSTRUCTION_NAME
%token <string> REGISTER_TYPE REGISTER_SPECIAL CONDITION EXTEND
%token <string> SHIFT_N_ROTATE
%token <int> NUMBER 
%token <float> IMMEDIATE_FLOAT
%token <string> VECTOR_ELEMENT VECTOR_REGISTER
%token <string> COMMENT
%token COMMA PARAM_SEP
%token EXCLAM COLON
%token L_SQUARE R_SQUARE
%token L_BRACE R_BRACE
%token EOL EOF
%token <(string * bool * int option)> LABEL

%start trace
%type <Trace.trace> trace
%type <parameter_t> instruction_parameter                                       

%%
  
	trace:
	| EOF { trc }
	| line; t = trace { t }
	;

	line:
        | pc = program_counter; COLON; oc = HEXADDR; ins = disassembled_instruction; COMMENT; EOL
	| pc = program_counter; COLON; oc = HEXADDR; ins = disassembled_instruction; EOL {
                                                                    let p = match pc with (ha, _) -> ha in
                                                                    let instr =
                                                                      try
                                                                        Hashtbl.find instruction_table p
                                                                      with
                                                                        Not_found ->
                                                                        let i = new Trace.trace_instruction pc oc ins isa in
                                                                        Hashtbl.add instruction_table p i; i
                                                                    in
                                                                    instr#iter;
                                                                    trc#add_line instr
                                                                  }
	;

	program_counter:
	| ha = HEXADDR { (ha, None) }
	| ha = HEXADDR; l = LABEL { (ha, Some (Label (l))) }
	;
	
	disassembled_instruction:
	| name = INSTRUCTION_NAME { (String.trim name, None) }
	| name = INSTRUCTION_NAME; params = instruction_parameters { (String.trim name, Some params) }
	;
	
	instruction_parameters:
	| p = instruction_parameter; PARAM_SEP; ps = instruction_parameters { p :: ps }
        | p = instruction_parameter { [p] }
	;

	instruction_parameter:
        | L_BRACE; v = vector; R_BRACE { Vector v }
	| r = register { Register r }
        | i = immediate { Immediate i }
        | s = shift { ShiftRotate s }
        | c = CONDITION { Condition c }

        | e = extend { Extend e }
        
        | hn = HEXADDR { Addressing (LiteralImm (hn, None)) }
        | hn = HEXADDR; l = LABEL { Addressing (LiteralImm (hn, Some (Label (l)))) }
          
        | L_SQUARE; r = register; COMMA; i = immediate; R_SQUARE { Addressing (BaseRegisterOffsetImm (r, Some i)) }
        | L_SQUARE; r1 = register; COMMA; r2 = register; R_SQUARE { Addressing (BaseRegisterOffsetReg (r1, r2, None)) }
        | L_SQUARE; r1 = register; COMMA; r2 = register; COMMA; s = shift; R_SQUARE { Addressing (BaseRegisterOffsetReg (r1, r2, Some s)) }
        | L_SQUARE; r1 = register; COMMA; r2 = register; COMMA; e = extend; R_SQUARE { Addressing (BaseRegisterOffsetExt (r1, r2, e)) }
        | L_SQUARE; r = register; COMMA; i = immediate; R_SQUARE; EXCLAM { Addressing (PreIndexImm (r, i)) }
        | L_SQUARE; r = register; R_SQUARE; PARAM_SEP; i = immediate (* hint for ld1 instruction which isn't well disassembled *)
        | L_SQUARE; r = register; R_SQUARE; COMMA; i = immediate { Addressing (PostIndexImm (r, i)) }
        | L_SQUARE; r = register; R_SQUARE { Addressing (BaseRegisterImm r) }
	;

        vector:
        | r = register; PARAM_SEP; v = vector { v @ [r] }
        | r = register { [r] }
          
        shift:
        | s = SHIFT_N_ROTATE; i = immediate { build_shift s i }
        ;

        extend:
        | e = EXTEND { build_extend e}
        | e = EXTEND; i = immediate { build_extend ~i:i e }
        ;
          
        register:
        | r = REGISTER_TYPE; n = NUMBER { build_register  ~n:n r }
        | r = REGISTER_SPECIAL { build_register r }
        | r = REGISTER_TYPE; n = NUMBER; vr = VECTOR_REGISTER { build_register ~n:n ~vr:vr r }
        | r = REGISTER_TYPE; n = NUMBER; ve = VECTOR_ELEMENT; L_SQUARE; i = NUMBER; R_SQUARE { build_register ~n:n ~ve:ve ~i:i r }
        ;
        
        immediate:
        | i = IMMEDIATE_DECIMAL { Decimal i }
        | i = IMMEDIATE_HEXA { Hexa i }
        | i = IMMEDIATE_FLOAT { Float i }
        ;
