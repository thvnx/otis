let usage_text = 
  "usage:\tcmd [options...] <asm_file>+\n"

let file:string ref = ref ""
let breakpoint = ref "main"

(** Scan the command line. *)
let scan_cmd_line =
  let scan_options =
    Arg.align [
        ("-breakpoint", Arg.Set_string breakpoint , "<bp> set breakpoint to bp");
      ]
  in
  Arg.parse scan_options (fun f -> file := f) usage_text;
  if(!file = "") then 
    begin
      Arg.usage scan_options usage_text;
      exit 2;
    end
;;
