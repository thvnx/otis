open Printf
  
(*class instruction(_code, _instr) =
object(self)
  val code:int = int_of_string _code
  val instr:string = _instr

  val mutable nb_called = 0

  method iter =
    nb_called <- nb_called + 1

  (*method branch_type =
    match String.sub instr 0 (String.index ' ') with
    | "bl" -> CALL
    | "ret" -> RETURN
    | "b" -> JUMP
    | _ -> NOBRANCH*)
                       
  method print =
    printf "0x%x[%d]\t %s" code nb_called instr                 
end;;

class fonction(_name) =
object(self)
  val name:string = _name
  val mutable parent:fonction option = None
  val mutable children:fonction option = None 

  val mutable trace:instruction list = []

  method get_name = name

  method set_parent p =
  (* check if parent == none *)
    parent <- Some p

  method set_children p =
  (* check if parent == none *)
    children <- Some p
                                     
  method add_instruction i =
    trace <- trace @ [i]

  method print =
    printf "%s %d\n" name (List.length trace)

  method print_fn_trace ind =
    for i = 0 to ind do
      printf " "
    done;
    match children with
    | None ->
       printf "| %s\n" name
    | Some c ->
       printf "| %s\n" name;
       c#print_fn_trace (ind+0)
      

  method print_trace =
    List.iter (
        fun i ->
        i#print
      ) trace
end;;

let instruction_table = Hashtbl.create 50000;;
let function_table = Hashtbl.create 50;;
  
let decode((_pca, _pcl), _instr) =
  let ins_o =
    try
      Hashtbl.find instruction_table _pca
    with
      Not_found ->
      let ins = new instruction(_pca, _instr) in
      Hashtbl.add instruction_table _pca ins;
      ins
  in
  ins_o#iter;
  let (fn_name, offset) =
    match _pcl with
      Some l ->
      let len =
        try
          String.index l '@'
        with
          Not_found ->
          try String.index l '+'
          with Not_found -> String.index l '>'
      in
      ((String.sub l 1 (len-1)), 0)
    | None -> ("0", 0)
  in
  let fn_o =
    try
      Hashtbl.find function_table fn_name
    with
      Not_found ->
      let fn = new fonction(fn_name) in
      Hashtbl.add function_table fn_name fn;
      fn
  in
  fn_o#add_instruction ins_o;
  ins_o
  
;;





(* decode a label <label_name@...+...> as (label_name, 0) *)
let decode_label label =
  match label with
    Some l ->
    let len =
      try
        String.index l '@'
      with
        Not_found ->
        try String.index l '+'
        with Not_found -> String.index l '>'
    in
    (Some (String.sub l 1 (len-1)), Some 0)
  | None -> (None, None)
;;
let decode_instruction pc asm =
  let ins_o =
    try
      Hashtbl.find instruction_table pc
    with
      Not_found ->
      let ins = new instruction(pc, asm) in
      Hashtbl.add instruction_table pc ins;
      ins
  in
  ins_o#iter;
  ins_o
;;
let parent_hierarchy = ref [new fonction("trace_master")] ;;
let function_list = ref [] ;;
let next_rel = ref false;;
   
let decode2((_pca, _pcl), _instr) =
  let (fname, _) = decode_label _pcl in
  let instr = (decode_instruction _pca _instr) in

  
  
  match fname with
  | Some f ->
     let fn = 
       if (List.hd !parent_hierarchy)#get_name <> f then
         begin
           let tmp = new fonction(f) in
           function_list := !function_list @ [tmp];

           (List.hd !parent_hierarchy)#set_children tmp;
           tmp#set_parent (List.hd !parent_hierarchy);

           parent_hierarchy := [tmp];
           tmp
         end
       else
         begin
           (List.hd !parent_hierarchy)
         end
     in
     fn#add_instruction instr;

  
     instr  
  | None -> failwith ("function name unknown")
   
;;
 *)

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


class trace_function(_name) =
object(self)
  val name:string = _name
  val mutable trace:trace_instruction list = []
  val mutable childs:trace_function list = []

  method get_name = name
                                     
  method add_instruction i =
    trace <- trace @ [i]

  method add_children c =
    (*printf "Add a child in %s\n" name;*)
    childs <- childs @ [c]
    
  method print =
    printf "%s[%d]\n" name (List.length trace)

  method print_trace =
    List.iter (
        fun i ->
        i#print
      ) trace

  method print_trace_fn (recur:int) =
    for i = 1 to recur-1 do printf "|" done;
    printf "-> %s[%d]\n" name (List.length trace);
    if recur > 0 then begin
      List.iter (
          fun i ->
          i#print_trace_fn (recur+1)
        ) childs;
      end;
    
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
             let tmp = new trace_function (fn_name) in (* create a new function *)
             function_list <- tmp :: function_list;
             f#add_children tmp; (* add the new function as children of called function f *)
             tmp
           end
         else f
      | (Call, f)::_ -> (* now, we are in a new function since last instruction was a call *)
         let tmp = new trace_function (fn_name) in (* create a new function *)
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
        i#print_trace_fn 1
      ) filtered
end;;
