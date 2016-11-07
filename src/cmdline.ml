let usage_text = 
  "usage:\tcmd [options...] <asm_file>+\n"

let isa_file = ref ("/Users/ltheveno/workspace/otis/isa/armv8.isa")
  
let file:string ref = ref ""
let breakpoint = ref "main"

let exec = ref false
let nb_instruction = ref false
         
let cfg_graphviz = ref false
let cfg_text = ref false
let cfg_tikz = ref false
let trace_text = ref false
let trace_graphviz = ref false
let pipeline_tikz = ref false
                   
let hw = ref "Perfect"
type hardware_model_t = PerfectILP | Custom | CortexA53 | CortexA57
let hardware_model = ref PerfectILP
let hw_model_default_latency = ref 1
let pipeline_description = ref ""
let set_hardware_model_properties = function
  | "Perfect"    ->
     pipeline_description := "I0MC0LS0N0B0D0"
  | "Cortex-A53" ->
     hardware_model := CortexA53;
     pipeline_description := "I1MC1LS1N1B1D1"
  | "Cortex-A57" ->
     hardware_model := CortexA57;
     pipeline_description := "I2MC1LS2N2B1D1"
  | custom ->
     hardware_model := Custom;
     pipeline_description := if String.contains custom 'D' then custom else custom ^ "D0"
;;
         
(** Scan the command line. *)
let scan_cmd_line =
  let scan_options =
    Arg.align [
        ("-breakpoint", Arg.Set_string breakpoint , "<bp> set breakpoint to bp");
        ("-exec", Arg.Set exec , " execute trace");
        ("-hardware-model", Arg.Set_string hw, "<desc> set hardware model to desc");
        ("-hardware-model-default-latency", Arg.Set_int hw_model_default_latency, "<desc> set hardware model to desc");
        ("-isa", Arg.Set_string isa_file, "<file> set isac");
        ("-nbi", Arg.Set nb_instruction , " show the number of instruction");
        ("-cfg-graphviz", Arg.Set cfg_graphviz , " Output graphviz graph");
        ("-cfg-text", Arg.Set cfg_text , " Output text graph");
        ("-cfg-tikz", Arg.Set cfg_tikz, " Output tikz graph");
        ("-trace-pipeline-tikz", Arg.Set pipeline_tikz , " Output tikz pipeline graph");
        ("-trace", Arg.Set trace_text , " Output trace");
        ("-trace-graphviz", Arg.Set trace_graphviz , " Output trace graph");
      ]
  in
  Arg.parse scan_options (fun f -> file := f) usage_text;
  if(!file = "") then 
    begin
      Arg.usage scan_options usage_text;
      exit 2;
    end;
  set_hardware_model_properties !hw;;

let outfile ext =
  let bn = Filename.basename !file in
  Printf.sprintf "%s.%s"
                 (try  (Filename.chop_extension bn)
                  with Invalid_argument _ -> bn)
                 ext
