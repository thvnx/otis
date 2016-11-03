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
    if String.length (Str.global_replace (Str.regexp "[!=()]") "" _op) <> 32 then
      raise (OpCodeNeq (_op))
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

  (** [code_match code] returns true if the instruction opcode [code] match the pattern opcode [_opcode] *)
  method code_match code =
    (** [sub_int n p l] returns the sub integer starting at position [p] with length [l] from [n]. [n] is an Int64 integer which has its 32 upper bits set to zero. *)
    let sub_int n p l =
      (* clear the p-l last significant bits of n *)
      let rs = Int64.shift_right n (p - l) in
      (* clear 32-l first significant bits of rs *)
      let ls = Int64.logand (Int64.shift_left rs (32 - l)) (Int64.of_string "0xffffffff") in
      (* return the l significant bits *)
      Int64.shift_right ls (32 - l)
    in
    (** [diff m p] returns true if the sub pattern [m], at position [p] isn't matched in the instruction opcode *)
    let diff m p =
      let len = String.length m in
      let res = sub_int code (32 - p) len in
      Printf.printf "%32s? %08Lx <> %08Lx\n" "" res (Int64.of_string ("0b" ^ m));
      res <> (Int64.of_string ("0b" ^ m))
    in
    (** [mch p_code pos] checks the adequacy of [p_code] (the pattern's opcode) and [code] (the instruction opcode). *)
    let rec mch p_code pos =
      Printf.printf "%32s [%c=%d] at %d\n" p_code (String.get _opcode pos) (Int64.to_int (Int64.shift_right code (31 - pos)) mod 2) pos;
      if pos < 31 then begin (* 32 bits length *)
        match String.get p_code 0 with
        | '0' -> (* continue if bit at position pos = 0 *)
           if (Int64.to_int (Int64.shift_right code (31 - pos)) mod 2) = 0 then mch (Str.string_after p_code 1) (pos+1) else false
        | '1' -> (* continue if bit at position pos = 1 *)
           if (Int64.to_int (Int64.shift_right code (31 - pos)) mod 2) = 1 then mch (Str.string_after p_code 1) (pos+1) else false
        | '-' -> mch (Str.string_after p_code 1) (pos+1) (* ignore and continue to check *)
        | '!' -> (* check if a sub pattern isn't matched *)
           let pstart = (String.index p_code '(') + 1 in
           let pstop  = String.index p_code ')' in
           let sub_pattern = Str.string_after (Str.string_before p_code pstop) pstart in
           if (diff sub_pattern pos) then
             mch (Str.string_after p_code pstart) (pos + 1 + pstop) (* continue checking *)
           else false (* sub pattern is matched, stop checking *)  
        end
      else true (* pos = 32, all the bits of the instruction match the pattern *)
    in
    Printf.printf "%s (%s) %08Lx\n" _opcode _name code; 
    mch _opcode 0 (* start matching at pos 0 *)

                
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


type pipeline_way_t = Inf | Fixed of int
type pipeline_kind_t = Integer of pipeline_way_t
                     | MultiCycle of pipeline_way_t
                     | LoadStore of pipeline_way_t
                     | Neon of pipeline_way_t
                     | Branch of pipeline_way_t
  
let read_pipeline =
  let default = ref [Integer Inf; MultiCycle Inf; LoadStore Inf; Neon Inf; Branch Inf] in
  let read_pipe p n =
    let n = int_of_string n in
    match p with
      "I" -> default := List.map (fun i -> match i with Integer _ -> Integer (Fixed n) | _ -> i) !default
    | "MC" -> default := List.map (fun i -> match i with MultiCycle _ -> MultiCycle (Fixed n) | _ -> i) !default
    | "LS" -> default := List.map (fun i -> match i with LoadStore _ -> LoadStore (Fixed n) | _ -> i) !default
    | "N" -> default := List.map (fun i -> match i with Neon _ -> Neon (Fixed n) | _ -> i) !default
    | "B" -> default := List.map (fun i -> match i with Branch _ -> Branch (Fixed n) | _ -> i) !default
    | _ -> Printf.fprintf stderr "Error: pipeline mnemonic %s is unknown\n\
                                  \tKnown mnemonics are: I, MC, LS, B, and N\n"
                          p;
           raise Exit
  in
  let rec read s =
    match s with
      [] -> !default
    | Str.Delim p :: Str.Text n :: t -> read_pipe p n; read t
    | Str.Delim p :: t ->
       Printf.fprintf stderr "Warning: no number of ways is provided for pipeline mnemonic %s (set to default instead)\n" p;
       read t
    | Str.Text _ :: t -> Printf.fprintf stderr "Error: unreachable\n"; raise Exit
  in
  match !Cmdline.pipeline with
  | "Cortex-A53" -> [Integer (Fixed 1); MultiCycle (Fixed 1); LoadStore (Fixed 1); Neon (Fixed 1); Branch (Fixed 1)]
  | "Cortex-A57" -> [Integer (Fixed 2); MultiCycle (Fixed 1); LoadStore (Fixed 2); Neon (Fixed 2); Branch (Fixed 1)]
  | s -> read (Str.full_split (Str.regexp "[A-Z]+") s)
