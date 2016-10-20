exception OpCodeNeq of string

type param_t    = Read | Write | Base | Imm | OptionRead | OptionImm
type branch_t   = Jump | Call | Return | NoBranch

type property_t = BranchProp of branch_t
                | Latency of int

                                                      
let param_to_string l =
  let rec stos e =
    match e with
    | Read       -> "R"
    | Write      -> "W"
    | Base       -> "B"
    | Imm        -> "I"
    | OptionRead -> "R'"
    | OptionImm  -> "I'"
  in
  String.concat ", " (List.map (fun i -> stos i) l)       



class instruction ?alias_of _op _name _params _properties =
  let _str = Printf.sprintf "%s: %5s %s" _op _name (param_to_string _params) in
  let _opcode =
    if String.length _op <> 32 then
      raise (OpCodeNeq _op)
    else
      _op in
  let _branch = 
    let rec p l =
      match l with
      | (BranchProp b)::t -> b
      | h::t -> p t
      | []   -> NoBranch
    in p _properties
  in

  (*let _ = print_endline _str in*)
object
  method name = _name
  method params = _params

  method branch = _branch

  method params_nonoptional =
    List.filter (fun i -> match i with OptionRead | OptionImm -> false | _ -> true) _params
                
  method code_match code =
    let oc = Int64.of_string ("0b" ^ (String.map (fun i -> match i with '-' -> '0' | _ -> i) _opcode)) in
    let mask = Int64.of_string ("0b" ^ (String.map (fun i -> match i with '-' -> '0' | _ -> '1') _opcode)) in
    (Int64.logand code mask) = oc
                
  method to_string = _str
end;;


  
class instruction_set =
object
  val mutable set:instruction list = []

  method add_instruction i =
    set <- i :: set

  method pattern_of name oc =
    let filtered =
      List.filter (
          fun (b, e) -> b )
                  ( List.map (
                        fun i -> (i#code_match oc, i)
                      ) (List.filter (fun i -> i#name = name) set) )
    in
    match filtered with
    | []          ->
       Printf.fprintf stderr "Warning: insrtruction %s wasn't found in ISA\n\
                              \tthe instruction has the following opcode: 0x%Lx, an empty pattern was returned\n"
                      name oc;
       new instruction "--------------------------------" "" [] [] 
    | (_,h) :: [] -> h
    | (_,h) :: _  ->
       Printf.fprintf stderr "Warning: insrtruction %s was found too much times (%d) in ISA\n\
                              \tthe instruction has the following opcode: 0x%Lx, first match was returned: %s\n"
                      name (List.length filtered) oc h#to_string;
       h
         
  method print =
    List.iter (fun i -> Printf.printf "%s\n" i#to_string) set
end;;
