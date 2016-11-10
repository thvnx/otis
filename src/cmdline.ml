(** Scan the command line and set some program parameters. *)

(* Files *)
let isa_description_file = ref "/Users/ltheveno/workspace/otis/isa/armv8.isa" ;;
let trace_file           = ref "" ;;

(* Trace anlysis *)
let trace_breakpoint   = ref "main" ;;
let trace_run_analysis = ref false ;;

(* Text outputs *)
(** [trace_function_text] outputs the (function-level) control flow
graph of the trace in ASCII mode. Root node is set by
[trace_breakpoint]. *)
let trace_function_text = ref false ;;

(* Graphic outputs *)
(** [trace_function_graphviz] outputs the (function-level) control
flow graph of the trace in graphviz/digraph mode. Root node is set by
[trace_breakpoint]. *)
let trace_function_graphviz = ref false ;;
(** [trace_function_tex] outputs the (function-level) control flow
graph of the trace in tikz mode. The vertical dimension represents the
control flow (the darker the deeper), while the horizontal dimension
states the instructions from the first (leftmost) to the last
(rightmost) of the program's trace. Root node is set by
[trace_breakpoint]. *)
let trace_function_tex      = ref false ;;

(** [trace_instruction_text] outputs the (instruction-level) control
flow graph in ASCII mode, i.e. the trace dump of a given
function. Root function is set by [trace_breakpoint]. *)
let trace_instruction_text     = ref false ;;
(** [trace_instruction_graphviz] outputs a folded-up
(instruction-level) control flow graph (DFS) in graphviz/digraph
mode. Root node is set by [trace_breakpoint]. Not suitable for large
graph *)
let trace_instruction_graphviz = ref false ;;

(** [dump_analysis_pipeline] outputs the analysis history w.r.t. the
execution pipelines. Analysis strat point is set by
[trace_breakpoint]. Implies to set [trace_run_analysis]. *)
let dump_analysis_pipeline = ref false ;;


(* Hardware model settings *)
type hardware_model_t = PerfectILP | Custom | CortexA53 | CortexA57 ;;

let hardware_model           = ref PerfectILP ;;
let hw_model_default_latency = ref 1 ;;
let pipeline_description     = ref "I0MC0LS0N0B0U0" ;;

(** Set the hardware model execution pipelines properties. Possible
values are {i Perfect}, {i Cortex-A53}, {i Cortex-A57} or a custom
string describing the execution pipelines. The custom string can
contains the following pipeline descriptors: {i B, I, LS, MC, N}
(respctively Branch, Integer, Load/Store, MultiCycle, and Neon),
followed by the number of them (non-zero positive number, zero stands
for infinite number). For example, {i Perfect} is a shortcut for {i
B0I0LS0MC0N0}. *)
let set_hardware_model_properties = function
  | "Perfect"    ->
     pipeline_description := "I0MC0LS0N0B0U0"
  | "Cortex-A53" ->
     hardware_model := CortexA53;
     hw_model_default_latency := 4;
     pipeline_description := "I1MC1LS1N1B1U1"
  | "Cortex-A57" ->
     hardware_model := CortexA57;
     hw_model_default_latency := 3;
     pipeline_description := "I2MC1LS2N2B1U1"
  | custom ->
     hardware_model := Custom;
     pipeline_description := if String.contains custom 'U' then custom else custom ^ "U0"
;;


let usage_text =
  "usage:\n\totis.{byte|native} <trace_file> [options]\n\n\
   possible options are:\n"


(** [scan_cmd_line] scans the command line options *)
let scan_cmd_line =
  let scan_options =
    Arg.align [
        ("-tb", Arg.Set_string trace_breakpoint, "<function_name> Set the analysis starting point to <function_name>. Default value is main.");
        ("-ra", Arg.Set trace_run_analysis,      " Execute the performance analysis from trace.");

        ("-hardware-model",                 Arg.String set_hardware_model_properties,
         "<hm> Set the hardware model to <hw>. Possible values are Perfect, Cortex-A53, Cortex-A57, or custom pipelines properties string (see documentation).");
        ("-hardware-model-default-latency", Arg.Set_int hw_model_default_latency,     "<int> Set the default instruction lantecy to <int> (1 by default).");

        ("-isa", Arg.Set_string isa_description_file, "<file> Set ISA description file to <file>.");

        ("-trace-dump-all",          Arg.Tuple [(Arg.Set trace_function_graphviz);
                                                (Arg.Set trace_function_text);
                                                (Arg.Set trace_function_tex);
                                                (Arg.Set trace_instruction_text);
                                                (Arg.Set trace_instruction_graphviz);
                                                (Arg.Set dump_analysis_pipeline); (Arg.Set trace_run_analysis)], " Set all trace and dump outpus flags.");
        ("-trace-function-graphviz", Arg.Set trace_function_graphviz, " Graphic output (see documentation).");
        ("-trace-function-text",     Arg.Set trace_function_text,     " ASCII output (see documentation).");
        ("-trace-function-tex",      Arg.Set trace_function_tex,      " Graphic output (see documentation).");
        ("-trace-instruction-text",     Arg.Set trace_instruction_text,     " ASCII output (see documentation).");
        ("-trace-instruction-graphviz", Arg.Set trace_instruction_graphviz, " Graphic output (see documentation).");

        ("-dump-analysis-pipeline", Arg.Tuple [(Arg.Set dump_analysis_pipeline); (Arg.Set trace_run_analysis)], " Graphic output (see documentation).");
      ]
  in
  Arg.parse scan_options (fun f -> trace_file := f) usage_text;
  if(!trace_file = "") then
    begin
      Arg.usage scan_options usage_text;
      raise Exit;
    end
;;

(** [output_file ext] returns a file name based on [trace_file]'s
basename with extension [ext]. *)
let output_file ext =
  let bn = Filename.basename !trace_file in
  Printf.sprintf "%s.%s"
                 (try  Filename.chop_extension bn
                  with Invalid_argument _ -> bn)
                 ext
