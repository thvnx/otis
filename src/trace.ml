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


class trace_instruction pc ins =
  let (_code, _scope) = match pc with p -> p in
  let (_fn, _plt, _off) = match _scope with None -> ("", false, None) | Some (Aarch64.Label s) -> (s) in
  let (_name, _params) = match ins with i -> i in
object(self)
                
  val mutable nb_called = 0

  method get_scope = _fn
  method get_code = _code
  method get_code_str = Int64.to_string _code

  method get_name = _name

  method get_branch_type =
    match _name with
    | "b" | "br" -> Jump
    | "bl" | "blr" -> Call
    | "ret" -> Return
    | _ -> NoBranch
                                                                                
  method iter =
    nb_called <- nb_called + 1
                       
  method print =
    printf "0x%Lx[%d,%s]\t %s\n" _code nb_called _fn _name                 
end;;

class execUnit =
object
  
  val mutable instruction_issued = 0
  val instruction_counter = Hashtbl.create 0
  
  method issue_instruction (instr:trace_instruction) =
    (*printf "execution of %s\n" instr#get_name;*)
    let key = instr#get_name in
    begin
      try
        let nb = Hashtbl.find instruction_counter key in
        Hashtbl.replace instruction_counter key (nb+1)
      with
        Not_found -> Hashtbl.add instruction_counter key 1
    end;
    instruction_issued <- instruction_issued + 1

  method print_instruction_counter =
    Hashtbl.iter (
        fun x y ->
        printf "%s\t%d\n" x y
      ) instruction_counter;
    printf "total:\t%d\n" instruction_issued

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
    printf "%s[#i:%d, d:%d]\n" name (List.length trace2) self#depth

  method print_trace =
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
    for i = 1 to self#depth do printf "-" done;
    printf "> %s\n" name;
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
    let rec findf fnh =
      match fnh with
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
           | [] -> failwith "not supposed to happen"
           | (Call, f)::[] -> (* history is empty, we must start a new function, see try...with below *)
              []
           | (Call, f)::t -> (NoBranch, f)::t (* hint to specify that Call has already been taken *)
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

  method fill content =
    List.iter (
        fun i -> self#add_line i
      ) content
    
  method exec breakpoint =
    let filtered = List.filter (fun i -> i#get_name = breakpoint) function_list in
    List.iter (
        fun i -> i#exec;
                 i#get_exec_unit#print_instruction_counter
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
