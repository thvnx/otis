let usage_text = 
  "usage:\tcmd [options...] <asm_file>+\n"

let isa_file = ref ("/Users/ltheveno/workspace/otis/isa/armv8.isa")
  
let file:string ref = ref ""
let breakpoint = ref "main"

let exec = ref false
let pipeline = ref ""
let nb_instruction = ref false
         
let cfg_graphviz = ref false
let cfg_text = ref false
let cfg_tikz = ref false
let trace_text = ref false
let trace_graphviz = ref false

                   
let hw = ref "PerfectILP"
type hardware_model_t = PerfectILP | NoModel
let hardmodel = function
    | "PerfectILP" -> PerfectILP
    | _ -> NoModel
         
(** Scan the command line. *)
let scan_cmd_line =
  let scan_options =
    Arg.align [
        ("-breakpoint", Arg.Set_string breakpoint , "<bp> set breakpoint to bp");
        ("-exec", Arg.Set exec , " execute trace");
        ("-pipeline", Arg.Set_string pipeline , "<desc> set pipeline description to desc");
        ("-hardware-model", Arg.Set_string hw, "<desc> set hardware model to desc");
        ("-isa", Arg.Set_string isa_file, "<file> set isac");
        ("-nbi", Arg.Set nb_instruction , " show the number of instruction");
        ("-cfg-graphviz", Arg.Set cfg_graphviz , " Output graphviz graph");
        ("-cfg-text", Arg.Set cfg_text , " Output text graph");
        ("-cfg-tikz", Arg.Set cfg_tikz , " Output tikz graph");
        ("-trace", Arg.Set trace_text , " Output trace");
        ("-trace-graphviz", Arg.Set trace_graphviz , " Output trace graph");
      ]
  in
  Arg.parse scan_options (fun f -> file := f) usage_text;
  if(!file = "") then 
    begin
      Arg.usage scan_options usage_text;
      exit 2;
    end
;;

let outfile ext =
  let bn = Filename.basename !file in
  Printf.sprintf "%s.%s"
                 (try  (Filename.chop_extension bn)
                  with Invalid_argument _ -> bn)
                 ext
