

class trace_instruction pc oc ins (isa:Isa.instruction_set) =
  let _opcode = oc in
  let (_addr, _scope) = match pc with p -> p in
  let (_fn, _plt, _off) = match _scope with None -> ("", false, None) | Some (Aarch64.Label s) -> (s) in
  let (_name, _params) = match ins with (n, p) -> (n, (match p with None -> [] | Some pp -> pp)) in
  let _str =
    Printf.sprintf "0x%Lx%s:\t0x%08Lx %s\t%s"
           _addr
           (match _scope with None -> "" | Some s -> " " ^ (Aarch64.label_to_string s))
           _opcode
           _name
           (String.concat ", " (List.map (fun i -> Aarch64.parameter_to_string i) _params))
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
  let pipeline = Isa.read_pipeline in
object(self)
  
  val mutable instruction_issued = 0
  val mutable execution_cycle = 0
  val mutable execution_cycle_max = 0
  val instruction_counter = Hashtbl.create 0
  val registers = Hashtbl.create 0

  val history = Hashtbl.create 0
  
  method issue_instruction fd (instr:trace_instruction) =
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
    
    Printf.fprintf fd "%6s, r: %12s, w: %8s\texecuted at cycle: %d/%d\n"
                   instr#name
                   (String.concat ";" (List.concat (List.map (fun i -> Aarch64.parameter_register_id i) instr#reads_param)))
                   (String.concat ";" (List.concat (List.map (fun i -> Aarch64.parameter_register_id i) instr#writes_param)))
                   execution_cycle execution_cycle_max
    
  method ilp_data fd =
    Hashtbl.iter (
        fun x y -> Printf.fprintf fd "%d %d\n" x (List.length y)
      ) history

  method print_instruction_counter =
    Hashtbl.iter (
        fun x y ->
        Printf.printf "%s\t%d\n" x y
      ) instruction_counter;
    Printf.printf "total:\t%d (in %d cycles, namely %1.2f IPC)\n" instruction_issued execution_cycle
                  ((float_of_int instruction_issued) /. (float_of_int execution_cycle))

end ;;
  
class trace_function ?parent _name =
(*let _ = printf "function: %s (child of: %s)\n" _name (match parent with None -> "NONE" | Some p -> p#get_name)  in*)
object(self)
  val name:string = _name
  val mutable trace2:(trace_instruction * trace_function option) list = []

  val eu = match parent with None -> new execUnit | Some p -> p#get_exec_unit

  method parent_name = match parent with None -> "no parent" | Some p -> p#get_name
                                                            
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

  method print_trace fd =
    let rec prt l =
      match l with
      | [] -> ()
      | (h, child)::t ->
         Printf.fprintf fd "%s\n" h#to_string;
         begin match child with None -> () | Some c -> c#print_trace fd end;
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

  method exec fd =
    List.iter (
        fun (i, c) ->
        match c with
          None   -> eu#issue_instruction fd i
        | Some f -> eu#issue_instruction fd i; (*printf "jump onto %s (#i:%d, d:%d)\n" f#get_name f#nb_instruction f#depth;*)
                    f#exec fd
      ) (List.rev trace2)
    
  method print_trace_fn fd n =
    let rec prt i l =
      match l with
      | [] -> ()
      | (_, child)::t ->
         begin
           match child with
           | None -> ()
           | Some c -> c#print_trace_fn fd i;
         end;
         prt (i+1) t
    in
    for i = 1 to self#depth do Printf.fprintf fd "-" done;
    Printf.fprintf fd "> %s; length:%d/%d%s\n" name (List.length trace2) (self#nb_instruction)
                   (if n <> 0 then (Printf.sprintf " parent:%s begin:%d" self#parent_name n) else "");
    prt 1 (List.rev trace2)


  method tikz_trace fd n scale =
    let scaling x =
      (float_of_int (x * 100)) /. (float_of_int scale)
    in
    let rec prt i l =
      match l with
      | []            -> ()
      | (_, child)::t ->
         begin
           match child with
           | None   -> prt (i+1) t
           | Some c -> c#tikz_trace fd i scale; prt (i+c#nb_instruction+1) t
         end;
         
    in
    
    Printf.fprintf fd "\\draw[mynode, fill=blue!%d] (%f,%d) rectangle (%f,%d) node[midway] {%s};\n"
                   (self#depth*10) (scaling n) self#depth (scaling (n + self#nb_instruction)) (self#depth + 1) (Str.global_replace (Str.regexp "_") "\\_" name)
    ;
   
    prt n (List.rev trace2)

  method dot_trace source fd =
    let nodes = ref [] in
    let add_node node =
      if not (List.mem node !nodes) then
        begin
          nodes := node :: !nodes;
          Printf.fprintf fd "%s\n" (Dot.node (node#get_code_str ^ (string_of_int self#get_id)) node#get_name)
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
         | Some c -> c#dot_trace (Some (ins, self#get_id)) fd;
                     dot t (Some (c#last_instruction, c#get_id))
    in
    Printf.fprintf fd "%s\n" (Dot.open_subgraph (string_of_int self#get_id));
    dot (List.rev trace2) source;
    Hashtbl.iter (
        fun (keysrc, keydst) (nb, (src, dst)) -> Printf.fprintf fd "%s\n" (Dot.edge keysrc keydst (string_of_int nb))
      ) relations;
    Printf.fprintf fd "%s\n" (Dot.close_subgraph name)
  
    
  method dot_trace_fn fd =
    let rec dot l =
      match l with
      | [] -> ()
      | (_, child)::t ->
         begin
           match child with
           | None -> ()
           | Some c ->
              c#dot_trace_fn fd;
              Printf.fprintf fd "%s\n" (Dot.edge (string_of_int self#get_id) (string_of_int c#get_id) "")
         end;
         dot t
    in
    (*let rec rank ?a l =
      let acc = match a with None -> [] | Some aa -> aa in
      match l with
      | []             -> acc
      | (_, None)::t   -> rank ~a:acc t
      | (_, Some c)::t -> rank ~a:(((Printf.sprintf "\"%d\"" c#get_id)) :: acc) t
    in*)
    Printf.fprintf fd "%s\n" (Dot.node (string_of_int self#get_id) name);
    dot (List.rev trace2);
    (*Printf.fprintf fd "{ rank = same; %s }\n" (String.concat "; " (rank trace2))*)
    
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

  method find_breakpoint bp =
    let (name, nb) =
      if String.contains bp '(' then
        (String.sub bp 0 (String.index bp '('),
         (int_of_string (String.sub bp ((String.index bp '(')+1) ((String.length bp) - (String.index bp '(') - 2) ) - 1))
      else (bp, -1)
    in
    let fltr = List.filter (fun i -> i#get_name = name) function_list in
    match List.length fltr with
    | 0 -> Printf.fprintf stderr "Error: can't set breakpoint (%s doesn't exist)\n" name;
           raise Exit
    | 1 -> List.hd fltr
    | i -> if nb >= 0 then
             begin
               if nb < i then List.nth fltr nb
               else begin
                   Printf.fprintf stderr "Warning: %s was found %d times, but you are asking for the occurence #%d\n\
                                          \treturning the first occurence instead\n" name i (nb+1);
                   List.hd fltr
                 end
             end
           else begin
               Printf.fprintf stderr "Warning: %s was found %d times, returning the first occurence\n\
                                      \ttry %s(n) for selecting the n-th occurence\n" name i name;
               List.hd fltr
             end
    
  method exec breakpoint =
    let fd = open_out (Cmdline.outfile "analysis.dump") in
    let filtered = self#find_breakpoint breakpoint in
    filtered#exec fd;
    filtered#get_exec_unit#print_instruction_counter;
    close_out fd;
    let fd = open_out (Cmdline.outfile "ilp.data") in
    filtered#get_exec_unit#ilp_data fd;
    close_out fd;

  (*method print_isa = isa#print*)
    
  method print_nb_instruction name =
    Printf.printf "%s: %d\n" name (self#find_breakpoint name)#nb_instruction
 
    
  (** Print the function hierarchy *)
  method print_trace name =
    let fd = open_out (Cmdline.outfile "processed_trace") in
    (self#find_breakpoint name)#print_trace fd;
    close_out fd
  method print_trace_fn name =
    let fd = open_out (Cmdline.outfile "cfg.txt") in
    (self#find_breakpoint name)#print_trace_fn fd 0;
    close_out fd

  method tikz_trace name =
    let fd = open_out (Cmdline.outfile "rg.tex") in
    Printf.fprintf fd "\\documentclass[tikz]{standalone}\n\
                       \\tikzset{mynode/.append style={draw=none, rounded corners=1pt}}\n\
                       \\begin{document}\n\
                       \\begin{tikzpicture}[x=15pt, y=10pt, font=\\small]\n";
    (self#find_breakpoint name)#tikz_trace fd 1 (self#find_breakpoint name)#nb_instruction;
    Printf.fprintf fd "\\end{tikzpicture}\n\
                       \\end{document}";
    close_out fd

  method dot_trace name =
    let fd = open_out (Cmdline.outfile "trc.dot") in
    Printf.fprintf fd "%s\n\
                       node [shape=rectangle, style=rounded]\n" Dot.open_graph;
    (self#find_breakpoint name)#dot_trace None fd;
    Printf.fprintf fd "%s\n" Dot.close_graph;
    close_out fd;
    Printf.printf "\trun <dot -Tpdf %s > %s>\n" (Cmdline.outfile "trc.dot") (Cmdline.outfile "trc.pdf")
  method dot_trace_fn name =
    let fd = open_out (Cmdline.outfile "cfg.dot") in
    let filtered = self#find_breakpoint name in
    Printf.fprintf fd "%s\n\
                       node [shape=rectangle, style=rounded]\n" Dot.open_graph;
    filtered#dot_trace_fn fd;   
    Printf.fprintf fd "%s\n" Dot.close_graph;
    Printf.printf "\trun <dot -Tpdf %s > %s>\n" (Cmdline.outfile "cfg.dot") (Cmdline.outfile "cfg.pdf");
    close_out fd
end;;
