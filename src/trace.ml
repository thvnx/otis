open Printf
  

type instruction_branch_type =
  | NoBranch
  | Call
  | Jump
  | Return
let ins_branch_type_to_string t =
  match t with
  | NoBranch -> "NoBranch"
  | Call -> "Call"
  | Jump -> "Jump"
  | Return -> "Return"

class trace_instruction((_code, _scope), _instr) =
object(self)
  val code:int = int_of_string _code
  val scope:string =
    match _scope with
      Some s ->
      let len =
        try
          String.index s '@'
        with
          Not_found ->
          try String.index s '+'
          with Not_found -> String.index s '>'
      in
      (String.sub s 1 (len-1))
    | None -> failwith "instruction without known scope"

  val instr:string = _instr


                   
  val mutable nb_called = 0

  method get_scope = scope
  method get_code = code
  method get_code_str = string_of_int code

  method get_name =
    let len = try String.index instr '\t' with Not_found -> (String.length instr)-1 in String.sub instr 0 len

  method get_branch_type =
    match self#get_name with
    | "b" | "br" -> Jump
    | "bl" | "blr" -> Call
    | "ret" -> Return
    | _ -> NoBranch
                                                                                
  method iter =
    nb_called <- nb_called + 1
                       
  method print =
    printf "0x%x[%d]\t %s" code nb_called instr                 
end;;

class execUnit =
object
  
  val mutable instruction_issued = 0
  
  method issue_instruction (instr:trace_instruction) =
    printf "execution of %s\n" instr#get_name;
    instruction_issued <- instruction_issued + 1

end ;;
  
class trace_function ?parent _name =
object(self)
  val name:string = _name
  (*val mutable trace:trace_instruction list = []
  val mutable childs:trace_function list = []*)
  (* todo: choose between trace and childs or trace2 *)
  val mutable trace2:(trace_instruction * trace_function option) list = []

  val eu = new execUnit

  method get_name = name
  method get_id = (2*(Obj.magic self))
                                     
  method add_instruction i =
    (* trace <- trace @ [i];*)
    trace2 <- (i, None) :: trace2

  method add_children c =
    (*printf "Add a child in %s\n" name;*)
    (* childs <- childs @ [c];*)
    let (new_hd, _) = (List.hd trace2) in
    trace2 <- (new_hd, Some c) :: List.tl trace2
    
  method print =
    printf "%s[#i:%d, d:%d]\n" name (List.length trace2) self#depth

  method print_trace =
    (*List.iter (
        fun i ->
        i#print
      ) trace*)
    let rec prt l =
      match l with
      | [] -> ()
      | (h, child)::t ->
         h#print;
         begin match child with None -> () | Some c -> c#print_trace end;
         prt t
    in
    prt (List.rev trace2)

  method last_instruction =
    let (i, _) = List.hd trace2 in
    i

  method nb_instruction =
    (*let nb = ref (List.length trace) in
    List.iter (
        fun i -> nb := !nb + i#nb_instruction
      ) childs;
    !nb*)
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

  method exec  =
    List.iter (
        fun (i, c) ->
        match c with
          None -> eu#issue_instruction i
        | Some f -> eu#issue_instruction i; printf "jump onto %s (#i:%d, d:%d)\n" f#get_name f#nb_instruction f#depth; f#exec
      ) (List.rev trace2)
    
              (*method print_trace_fn (recur:int) =*)
    (*for i = 1 to recur-1 do printf "|" done;
    printf "-> %s[%d]\n" name (List.length trace);
    if recur > 0 then begin
      List.iter (
          fun i ->
          i#print_trace_fn (recur+1)
        ) childs;
      end;*)
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
    for i = 1 to self#depth do printf "-" done;
    printf "> %s\n" name;
    prt (List.rev trace2)

  method dot_trace source =(* take into account trace2 and the child information *)
    (*let uniq_nodes = Hashtbl.create 0 in
    let uniq_rel = Hashtbl.create 0 in
    let add_relation key value =
      try
        let (n, v) = Hashtbl.find uniq_rel key in
        Hashtbl.replace uniq_rel key (n+1, v)
      with
        Not_found ->
        Hashtbl.add uniq_rel key (1, value)
    in
    let rec keep_uniq (l:trace_instruction list) =
      match l with
      | [] -> failwith ("function trace is empty")
      | i::[] -> Hashtbl.add uniq_nodes i#get_code i
      | i1::i2::[] ->
         Hashtbl.replace uniq_nodes i1#get_code i1;
         Hashtbl.replace uniq_nodes i2#get_code i2;
         add_relation (i1#get_code_str ^ i2#get_code_str) (i1, i2)
      | i1::i2::t ->
         Hashtbl.replace uniq_nodes i1#get_code i1;
         add_relation (i1#get_code_str ^ i2#get_code_str) (i1, i2);
         keep_uniq (i2::t)
    in
    keep_uniq trace;
    printf "subgraph cluster_%d {\n" self#get_id;
    Hashtbl.iter (
        fun _ y ->
        printf "\"%x\" [label=\"%s\"];\n" y#get_code y#get_name
      ) uniq_nodes;
    Hashtbl.iter (
        fun _ (yn, (yr, yl)) ->
        printf "\"%x\" -> \"%x\" [label=\"%d\"];\n" yr#get_code yl#get_code yn
      ) uniq_rel;
    (**)
    List.iter (
        fun i ->
        i#dot_trace
      ) childs;
    (**)
    printf "label=\"%s\";\n}\n" name*)
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
    (*printf "\"%d\" [label=\"%s(%d)\"];\n" self#get_id name (List.length trace);
    List.iter (
        fun i ->
        printf "\"%d\" -> \"%d\";\n" self#get_id i#get_id;
        i#dot_trace_fn
      ) childs;*)

    let rec dot l =
      match l with
      | [] -> ()
      | (_, child)::t ->
         begin
           match child with
           | None -> ()
           | Some c ->
              c#dot_trace_fn;
              printf "\"%d\" -> \"%d\";\n" self#get_id c#get_id;
         end;
         dot t
    in
    printf "\"%d\" [label=\"%s(%d)\"];\n" self#get_id name (List.length trace2);
    dot (List.rev trace2)
    
end;;
  
  




class trace =
object(self)

  (** Keep an history of function calls to build function hierarchy *)
  val mutable function_history:(instruction_branch_type * trace_function) list = []
  (** List of all function of the trace *)
  val mutable function_list:trace_function list = [] 
  
  (** Add trace line, i.e. instruction line *)
  method add_line (_instr:trace_instruction) =
    let fn_name = _instr#get_scope in
    (** Find the right function where to add _instr *)
    let fn =
      match function_history with
      | [] -> (* history is empty, so create a new function labeled by instructionn scope *)
         let tmp = new trace_function (fn_name) in
         function_list <- tmp :: function_list;
         function_history <- (Call, tmp) :: function_history; (* first instruction of the trace, simulate a call *)
         tmp
      | (NoBranch, f)::_ -> (* in this function f, last added instruction is not a branch *)
         f
      | (Jump, f)::_ -> (* now, we are maybe in a new function since last instruction was a jump *)
         if f#get_name <> fn_name then begin
             let tmp = new trace_function ~parent:f (fn_name) in (* create a new function *)
             function_list <- tmp :: function_list;
             f#add_children tmp; (* add the new function as children of called function f *)
             tmp
           end
         else f
      | (Call, f)::_ -> (* now, we are in a new function since last instruction was a call *)
         let tmp = new trace_function ~parent:f (fn_name) in (* create a new function *)
         function_list <- tmp :: function_list;
         f#add_children tmp; (* add the new function as children of called function f *)
         tmp
      | (Return, f)::_ -> (* now, we are in a new function since last instruction was a return *)
         (* recover the caller: 
            clean function_history list (remove the return and all Jump until fisrt Call *)
         let rec clean l =
           match l with
           | [] -> l
           | (Call, f)::t -> (NoBranch, f)::t (* hint to specify that Call has already been taken *)
           | _::t -> clean t
         in
         function_history <- (clean function_history);
         let (_, tmp) = try List.hd function_history with _ -> (NoBranch, f) in (* remove the try: do not empty the list while cleaning! *)
         tmp
    in
    fn#add_instruction _instr;
    
    let ins_branch_type = _instr#get_branch_type in
    function_history <- (ins_branch_type, fn) :: function_history

  method fill content =
    List.iter (
        fun i -> self#add_line i
      ) content
    
  method exec breakpoint =
    let filtered = List.filter (fun i -> i#get_name = breakpoint) function_list in
    List.iter (
        fun i -> i#exec
      ) filtered
    
  method print_nb_instruction name =
    let filtered = List.filter (fun i -> i#get_name = name) function_list in
    List.iter (
        fun i ->
        printf "%s: %d\n" name i#nb_instruction
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
    printf "digraph G {\n";
    List.iter (
        fun i ->
        i#dot_trace None
      ) filtered;
    printf "}\n";
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
