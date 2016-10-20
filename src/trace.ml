

class trace_instruction pc oc ins (isa:Isa.instruction_set) =
  let _opcode = oc in
  let (_addr, _scope) = match pc with p -> p in
  let (_fn, _plt, _off) = match _scope with None -> ("", false, None) | Some (Aarch64.Label s) -> (s) in
  let (_name, _params) = match ins with (n, p) -> (n, (match p with None -> [] | Some pp -> pp)) in
  let _str =
    Printf.sprintf "0x%Lx%s:\t0x%08Lx %s%s"
           _addr
           (match _scope with None -> "" | Some s -> " " ^ (Aarch64.label_to_string s))
           _opcode
           _name
           ("\t" ^ (String.concat ", " (List.map (fun i -> Aarch64.parameter_to_string i) _params)))
  in
  let _pattern = isa#pattern_of _name _opcode in
  let (_wparams, _rparams) =
    if List.length _params <> List.length _pattern#params &&
         List.length _params <> List.length _pattern#params_nonoptional then begin
        Printf.fprintf stderr "Warning: instruction %s has a different number of parameters than its pattern counterpart\n\
                               \tempty write and read parameters lists are returned for this instruction\n\
                               \tinstruction is: %s\n\tbut expected pattern is %s\n"
                       _name _str _pattern#to_string;
        ([], [])
      end
    else begin
        let rtmp = ref [] in
        let wtmp = ref [] in
        List.iteri (
            fun i p ->
            match (List.nth _pattern#params i) with
            | Isa.Read | Isa.Base | Isa.OptionRead -> rtmp := p :: !rtmp
            | Isa.Write -> wtmp := p :: !wtmp
            | Isa.Imm | Isa.OptionImm -> ()
          ) _params;
        (!wtmp, !rtmp)
      end
  in
object(self)
                
  val mutable nb_called = 0

  method get_scope = _fn
  method get_code = _addr
  method get_code_str = Int64.to_string _addr

  method get_name = _name (* todo remove *)
  method name = _name

  method writes_param = _wparams
  method reads_param = _rparams

  method get_branch_type =
(*    match _name with
    | "b" | "br" | "b.gt" | "b.eq"
      | "cbz" | "cbnz" -> Jump
    | "bl" | "blr" -> Call
    | "ret"        -> Return
    | _ -> NoBranch*)
    _pattern#branch
                                                                                
  method iter =
    nb_called <- nb_called + 1


  method to_string = _str
    
end;;



class register name =
object(self)
  val mutable read_history = [0]
  val mutable write_history = [0]

  method last_read = List.hd read_history
  method last_write = List.hd write_history

  method read r =
    read_history <- r :: read_history;
    self#last_write
    
  method write r = write_history <- r :: write_history
end;;
  
class execUnit =
object(self)
  
  val mutable instruction_issued = 0
  val mutable execution_cycle = 0
  val mutable execution_cycle_max = 0
  val instruction_counter = Hashtbl.create 0
  val registers = Hashtbl.create 0

  val history = Hashtbl.create 0
  
  method issue_instruction (instr:trace_instruction) =
    let update_history =
      try
        let h = Hashtbl.find history execution_cycle in
        Hashtbl.replace history execution_cycle (instr :: h)
      with
        Not_found -> Hashtbl.add history execution_cycle [instr]
    in
    let rec read_register r =
      try
        (Hashtbl.find registers r)#read execution_cycle
      with
        Not_found -> Hashtbl.add registers r (new register r);
                     read_register r
    in
    let rec write_register r =
      try
        (Hashtbl.find registers r)#write execution_cycle
      with
        Not_found -> Hashtbl.add registers r (new register r);
                     write_register r
    in
    let key = instr#get_name in
    begin
      try
        let nb = Hashtbl.find instruction_counter key in
        Hashtbl.replace instruction_counter key (nb+1)
      with
        Not_found -> Hashtbl.add instruction_counter key 1
    end;
    instruction_issued <- instruction_issued + 1;

    (* map the list of read parameters to get the cycle at which the instruction can be executed: the maximum cycle count of the parameters *)
    let exec_at = Common.maxn (
                      List.map ( fun i ->
                                 read_register i
                               )
                               (List.concat (List.map ( fun i -> Aarch64.parameter_register_id i ) (instr#reads_param @ instr#writes_param)));
                    ) in
    (* execute the instruction consists of increment the cycle count *)
    execution_cycle <- (if instr#get_branch_type = Isa.NoBranch then exec_at else (max execution_cycle_max exec_at)) + 1;
    execution_cycle_max <- max execution_cycle execution_cycle_max;
    (* update the cycle count of the written parameters *)
    List.iter ( fun i ->
               write_register i
             )
             (List.concat (List.map ( fun i -> Aarch64.parameter_register_id i ) instr#writes_param));

    update_history;
    
    Printf.printf "%6s, r: %12s, w: %8s\texecuted at cycle: %d/%d\n"
                  instr#name
                  (String.concat ";" (List.concat (List.map (fun i -> Aarch64.parameter_register_id i) instr#reads_param)))
                  (String.concat ";" (List.concat (List.map (fun i -> Aarch64.parameter_register_id i) instr#writes_param)))
                  execution_cycle execution_cycle_max
    
  method history_to_plot =
    let fd = open_out "h.plot" in
    Hashtbl.iter (
        fun x y -> Printf.fprintf fd "%d %d\n" x (List.length y)
      ) history;
    close_out fd

  method print_instruction_counter =
    Hashtbl.iter (
        fun x y ->
        Printf.printf "%s\t%d\n" x y
      ) instruction_counter;
    Printf.printf "total:\t%d (in %d cycles, namely %1.2f IPC)\n" instruction_issued execution_cycle
                  ((float_of_int instruction_issued) /. (float_of_int execution_cycle));
    self#history_to_plot

end ;;
  
class trace_function ?parent _name =
(*let _ = printf "function: %s (child of: %s)\n" _name (match parent with None -> "NONE" | Some p -> p#get_name)  in*)
object(self)
  val name:string = _name
  val mutable trace2:(trace_instruction * trace_function option) list = []

  val eu = match parent with None -> new execUnit | Some p -> p#get_exec_unit

  method get_name = name
  method get_id = (2*(Obj.magic self))
  method get_exec_unit = eu
                                     
  method add_instruction i =
    trace2 <- (i, None) :: trace2

  method add_children c =
    let (new_hd, _) = (List.hd trace2) in
    trace2 <- (new_hd, Some c) :: List.tl trace2
    
  method print =
    Printf.printf "%s[#i:%d, d:%d]\n" name (List.length trace2) self#depth

  method print_trace =
    let rec prt l =
      match l with
      | [] -> ()
      | (h, child)::t ->
         print_endline h#to_string;
         begin match child with None -> () | Some c -> c#print_trace end;
         prt t
    in
    prt (List.rev trace2)

  method last_instruction =
    let (i, _) = List.hd trace2 in
    i

  method nb_instruction =
    let rec nb l acc =
      match l with
      | [] -> acc
      | (h, child)::t ->
         nb t (acc+1+(match child with None -> 0 | Some c -> c#nb_instruction))
    in
    nb (List.rev trace2) 0   

  method depth =
    match parent with
      None -> 0
    | Some p -> 1 + p#depth

  method exec =
    List.iter (
        fun (i, c) ->
        match c with
          None -> eu#issue_instruction i
        | Some f -> eu#issue_instruction i; (*printf "jump onto %s (#i:%d, d:%d)\n" f#get_name f#nb_instruction f#depth;*) f#exec
      ) (List.rev trace2)
    
  method print_trace_fn =
    let rec prt l =
      match l with
      | [] -> ()
      | (_, child)::t ->
         begin
           match child with
           | None -> ()
           | Some c -> c#print_trace_fn;
         end;
         prt t
    in
    for i = 1 to self#depth do Printf.printf "-" done;
    Printf.printf "> %s\n" name;
    prt (List.rev trace2)

  method dot_trace source =
    let nodes = ref [] in
    let add_node node =
      if not (List.mem node !nodes) then
        begin
          nodes := node :: !nodes;
          Dot.node (node#get_code_str ^ (string_of_int self#get_id)) node#get_name
        end
    in
    let relations = Hashtbl.create 0 in
    let add_relation (src:(trace_instruction * int) option) (dst:trace_instruction) =
      match src with
        None -> ()
      | Some (s, f) ->
         let key = (s#get_code_str ^ (string_of_int f), dst#get_code_str ^ (string_of_int self#get_id)) in
         try
           let (nb, r) = Hashtbl.find relations key in
           Hashtbl.replace relations key (nb+1, r)
         with
           Not_found ->
           Hashtbl.add relations key (1, (s, dst))
    in
    let rec dot l s =
      match l with
      | [] -> () (* end of trace *)
      | (ins, child)::t ->
         add_node ins;
         add_relation s ins;
         match child with
         | None   -> dot t (Some (ins, self#get_id))
         | Some c -> c#dot_trace (Some (ins, self#get_id));
                     dot t (Some (c#last_instruction, c#get_id))
    in
    Dot.open_subgraph (string_of_int self#get_id);
    dot (List.rev trace2) source;
    Hashtbl.iter (
        fun (keysrc, keydst) (nb, (src, dst)) -> Dot.edge keysrc keydst (string_of_int nb)
      ) relations;
    Dot.close_subgraph name
  
    
  method dot_trace_fn =
    let rec dot l =
      match l with
      | [] -> ()
      | (_, child)::t ->
         begin
           match child with
           | None -> ()
           | Some c ->
              c#dot_trace_fn;
              Printf.printf "\"%d\" -> \"%d\";\n" self#get_id c#get_id;
         end;
         dot t
    in
    Printf.printf "\"%d\" [label=\"%s(%d)\"];\n" self#get_id name (List.length trace2);
    dot (List.rev trace2)
    
end;;
  
  




class trace (_isa:Isa.instruction_set) =
object(self)

  (*val isa:Isa.instruction_set = _isa*)
  (** Keep an history of function calls to build function hierarchy *)
  val mutable function_history:(Isa.branch_t * trace_function) list = []
  (** List of all function of the trace *)
  val mutable function_list:trace_function list = []

  method isa = _isa
                                                
 (** Add trace line, i.e. instruction line *)
  method add_line (_instr:trace_instruction) =
    let fn_name = _instr#get_scope in
    (** Find the right function where to add _instr *)
    let rec findf fnh =
      match fnh with
      | [] -> (* history is empty, so create a new function labeled by instructionn scope *)
         let tmp = new trace_function (fn_name) in
         function_list <- tmp :: function_list;
         function_history <- (Isa.Call, tmp) :: function_history; (* first instruction of the trace, simulate a call *)
         tmp
      | (Isa.NoBranch, f)::_ -> (* in this function f, last added instruction is not a branch *)
         f
      | (Isa.Jump, f)::_ -> (* now, we are maybe in a new function since last instruction was a jump *)
         if f#get_name <> fn_name then begin
             let tmp = new trace_function ~parent:f (fn_name) in (* create a new function *)
             function_list <- tmp :: function_list;
             f#add_children tmp; (* add the new function as children of called function f *)
             tmp
           end
         else f
      | (Isa.Call, f)::_ -> (* now, we are in a new function since last instruction was a call *)
         let tmp = new trace_function ~parent:f (fn_name) in (* create a new function *)
         function_list <- tmp :: function_list;
         f#add_children tmp; (* add the new function as children of called function f *)
         tmp
      | (Isa.Return, f)::_ -> (* now, we are in a new function since last instruction was a return *)
         (* recover the caller: 
            clean function_history list (remove the return and all Jump until fisrt Call *)
         let rec clean l =
           match l with
           | [] -> failwith "not supposed to happen"
           | (Isa.Call, f)::[] -> (* history is empty, we must start a new function, see try...with below *)
              []
           | (Isa.Call, f)::t -> (Isa.NoBranch, f)::t (* hint to specify that Call has already been taken *)
           | _::t -> clean t
         in
         function_history <- (clean function_history);
         let tmp = try match List.hd function_history with (_, t) -> t
                   with Failure("hd") -> findf []
         in 
         tmp
    in
    let fn = findf function_history in
    fn#add_instruction _instr;
    
    let ins_branch_type = _instr#get_branch_type in
    function_history <- (ins_branch_type, fn) :: function_history

 (* method fill content =
    List.iter (
        fun i -> self#add_line i
      ) content *)
    
  method exec breakpoint =
    let filtered = List.filter (fun i -> i#get_name = breakpoint) function_list in
    (*List.iter (
        fun i -> i#exec;
                 i#get_exec_unit#print_instruction_counter
      ) filtered*)
    (List.hd filtered)#exec;
    (List.hd filtered)#get_exec_unit#print_instruction_counter

  (*method print_isa = isa#print*)
    
  method print_nb_instruction name =
    let filtered = List.filter (fun i -> i#get_name = name) function_list in
    List.iter (
        fun i ->
        Printf.printf "%s: %d\n" name i#nb_instruction
      ) filtered
    
  (** Print the function hierarchy *)
  method print_trace name =
    let filtered = List.filter (fun i -> i#get_name = name) function_list in
    List.iter (
        fun i ->
        i#print_trace
      ) filtered
  method print_trace_fn name =
    let filtered = List.filter (fun i -> i#get_name = name) function_list in
    List.iter (
        fun i ->
        i#print_trace_fn
      ) filtered

  method dot_trace name =
    let filtered = List.filter (fun i -> i#get_name = name) function_list in
    begin
      match List.length filtered with
      | 0 -> failwith ("breakpoint " ^ name ^ " was not found")
      | i -> if i > 1 then failwith ("breakpoint " ^ name ^ " was found too many times (" ^ (string_of_int i) ^ ")")
    end;
    Printf.printf "digraph G {\n";
    List.iter (
        fun i ->
        i#dot_trace None
      ) filtered;
    Printf.printf "}\n";
  method dot_trace_fn name =
    let filtered = List.filter (fun i -> i#get_name = name) function_list in
    begin
      match List.length filtered with
      | 0 -> failwith ("breakpoint " ^ name ^ " was not found")
      | i -> if i > 1 then failwith ("breakpoint " ^ name ^ " was found too many times (" ^ (string_of_int i) ^ ")")
    end;
    List.iter (
        fun i ->
        i#dot_trace_fn
      ) filtered
end;;
