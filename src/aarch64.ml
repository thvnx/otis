
exception Pattern_not_found

type label_t =
  | Label of (string * bool * int option)
           
let label_to_string l =
  match l with Label (s, b, i) ->
    Printf.sprintf "<%s%s%s>"
                   s
                   (if b then "@plt" else "")
                   (match i with None -> "" | Some ii -> "+" ^ (string_of_int ii))
           
type vector_t =
  | Register8B | Register16B
  | Register4H | Register8H
  | Register2S | Register4S
  | Register1D | Register2D
  | ElementB of int
  | ElementH of int
  | ElementS of int
  | ElementD of int
let vector_to_string v =
  match v with
  | Register8B -> "8b"
  | Register16B -> "16b"
  | Register4H -> "4h"
  | Register8H -> "8h"
  | Register2S -> "2s"
  | Register4S -> "4s"
  | Register1D -> "1d"
  | Register2D -> "2d"
  | ElementB i -> Printf.sprintf "b[%d]" i
  | ElementH i -> Printf.sprintf "h[%d]" i
  | ElementS i -> Printf.sprintf "s[%d]" i
  | ElementD i -> Printf.sprintf "d[%d]" i
  
let build_vector v i =
  match i with
  | None ->
     begin
       match v with
       | ".8b" -> Register8B
       | ".16b" -> Register16B
       | ".4h" -> Register4H
       | ".8h" -> Register8H
       | ".2s" -> Register2S
       | ".4s" -> Register4S
       | ".1d" -> Register1D
       | ".2d" -> Register2D
       | _ -> Printf.fprintf stderr "Error: pattern matching failed on: %s (%s)\n" v __LOC__; 
          raise Exit
     end
  | Some idx ->
     begin
       match v with
       | ".b" -> ElementB idx
       | ".h" -> ElementH idx
       | ".s" -> ElementS idx
       | ".d" -> ElementD idx
       | _ -> Printf.fprintf stderr "Error: pattern matching failed on: %s (%s)\n" v __LOC__; 
              raise Exit
     end

    
type register_t =
  | Xn of int
  | Wn of int
  | Qn of int
  | Dn of int
  | Sn of int
  | Vn of (int * vector_t)
  | Wzr
  | Xzr
  | SP
  | MiscR of string
let register_to_string r =
  match r with
  | Xn n -> Printf.sprintf "x%d" n
  | Wn n -> Printf.sprintf "w%d" n
  | Qn n -> Printf.sprintf "q%d" n
  | Dn n -> Printf.sprintf "d%d" n
  | Sn n -> Printf.sprintf "s%d" n
  | Vn (n, v) -> Printf.sprintf "v%d.%s" n (vector_to_string v)
  | Wzr -> "wzr"
  | Xzr -> "xzr"
  | SP -> "sp"
  | MiscR s -> s

type immediate_t =
  | Decimal of int64
  | Hexa of int64
  | Float of float
let immediate_to_string i =
  "#" ^ (
    match i with
    | Decimal d -> Printf.sprintf "%Ld" d
    | Hexa h -> "0x" ^ Printf.sprintf "%Lx" h
    | Float f -> Printf.sprintf "%f" f 
  )
type shift_t =
  | LSL of immediate_t
  | LSR of immediate_t
  | ASR of immediate_t
type extend_t =
  | UXTB of immediate_t option
  | UXTH of immediate_t option
  | UXTW of immediate_t option
  | UXTX of immediate_t option
  | SXTB of immediate_t option
  | SXTH of immediate_t option
  | SXTW of immediate_t option
  | SXTX of immediate_t option
let shift_to_string s =
  match s with
  | LSL i -> Printf.sprintf "lsl %s" (immediate_to_string i)
  | LSR i -> Printf.sprintf "lsr %s" (immediate_to_string i)
  | ASR i -> Printf.sprintf "asr %s" (immediate_to_string i)
let extend_to_string e =
  let (reg, imm) = (match e with
                    | UXTB i -> "uxtb", ""(*imm to string*)
                    | UXTH i -> "uxth", ""
                    | UXTW i -> "uxtw", ""
                    | UXTX i -> "uxtx", ""
                    | SXTB i -> "sxtb", ""
                    | SXTH i -> "sxth", ""
                    | SXTW i -> "sxtw", ""
                    | SXTX i -> "sxtx", ""
                   ) in
  Printf.sprintf "%s%s" reg imm
          
type addressing_t =
  | BaseRegisterImm of register_t
  | BaseRegisterOffsetImm of (register_t * immediate_t option) 
  | BaseRegisterOffsetReg of (register_t * register_t * shift_t option)
  | BaseRegisterOffsetExt of (register_t * register_t * extend_t)
  | PreIndexImm of (register_t * immediate_t)
  | PostIndexImm of (register_t * immediate_t)
  | PostIndexReg of (register_t * register_t)
  | LiteralImm of (int64 * label_t option)
let addressing_to_string a =
  match a with
  | BaseRegisterImm r -> Printf.sprintf "[%s]"
                                        (register_to_string r)
  | BaseRegisterOffsetImm (r, i) -> Printf.sprintf "[%s%s]"
                                                   (register_to_string r)
                                                   (match i with None -> ""
                                                               | Some ii -> "," ^ immediate_to_string ii)
  | BaseRegisterOffsetReg (r1, r2, s) -> Printf.sprintf "[%s,%s%s]"
                                                        (register_to_string r1)
                                                        (register_to_string r2)
                                                        (match s with None -> ""
                                                                    | Some ss -> "," ^ shift_to_string ss)
  | BaseRegisterOffsetExt (r1, r2, e) -> Printf.sprintf "[%s,%s,%s]"
                                                        (register_to_string r1)
                                                        (register_to_string r2)
                                                        (extend_to_string e)       
  | PreIndexImm (r, i) -> Printf.sprintf "[%s,%s]!"
                                         (register_to_string r)
                                         (immediate_to_string i)
  | PostIndexImm (r, i) -> Printf.sprintf "[%s],%s"
                                          (register_to_string r)
                                          (immediate_to_string i)
  | PostIndexReg (r1, r2) -> Printf.sprintf "[%s],%s"
                                          (register_to_string r1)
                                          (register_to_string r2)
  | LiteralImm (i, l) -> Printf.sprintf "0x%Lx%s"
                                        i
                                        (match l with None -> ""
                                                    | Some ll -> " " ^ label_to_string ll)
                       
type parameter_t =
  | Register of register_t
  | Addressing of addressing_t
  | Immediate of immediate_t
  | ShiftRotate of shift_t
  | Condition of string
  | Extend of extend_t
  | Vector of register_t list

let parameter_to_string p =
  match p with
  | Register r -> register_to_string r
  | Addressing a -> addressing_to_string a
  | Immediate i -> immediate_to_string i
  | ShiftRotate s -> shift_to_string s
  | Condition s -> s
  | Extend e -> extend_to_string e
  | Vector l -> Printf.sprintf "{%s}" (String.concat ", " (List.map (fun i -> register_to_string i) l))


let parameter_register_id p =
  let reg_to_id r = [
    match r with
    | Xn n
      | Wn n -> Printf.sprintf "R%d" n
    | Qn n
      | Dn n
      | Sn n
      | Vn (n, _) -> Printf.sprintf "V%d" n
    | Wzr
      | Xzr -> "zr"
    | SP  -> "sp"
    | MiscR s -> s ]
  in
  match p with
  | Register r   -> reg_to_id r
  | Addressing a ->
     begin
       match a with
       | BaseRegisterImm r
       | BaseRegisterOffsetImm (r, _)
       | PreIndexImm (r, _)
       | PostIndexImm (r, _) -> reg_to_id r
                       
       | BaseRegisterOffsetReg (r1, r2, _)
       | BaseRegisterOffsetExt (r1, r2, _)
       | PostIndexReg (r1, r2) -> (reg_to_id r1) @ (reg_to_id r2)
                       
       | LiteralImm _ -> Printf.fprintf stderr "Warning: trying to get the register id of an non-register parameter (%s)"
                                        (parameter_to_string p);
                         []
     end
  | Vector v     -> List.concat (List.map ( fun i -> reg_to_id i ) v) 
  | _ -> Printf.fprintf stderr "Warning: trying to get the register id of an non-register parameter (%s)"
                        (parameter_to_string p);
         []
     

let build_register ?n ?vr ?ve ?i r =
  let nb = match n with None -> 0 | Some nn -> nn in
  match r with
  | "x" -> Xn (nb)
  | "w" -> Wn (nb)
  | "q" -> Qn (nb)
  | "d" -> Dn (nb)
  | "s" -> Sn (nb)
  | "v" ->
     begin
       match vr, ve with
       | None, None | Some _, Some _ -> failwith "not supposed to happen"
       | Some r, None | None, Some r -> Vn (nb, build_vector r i)
     end
  | "sp" -> SP
  | "wzr" -> Wzr
  | "xzr" -> Xzr
  | "fpcr" | "tpidr_el0" -> MiscR (r)
  | _ -> Printf.fprintf stderr "Error: pattern matching failed on: %s (%s)\n" r __LOC__; 
         raise Exit
                          
let build_shift s i =
  match s with
  | "lsl" -> LSL i
  | "lsr" -> LSR i
  | "asr" -> ASR i
  | _ -> Printf.fprintf stderr "Error: pattern matching failed on: %s (%s)\n" s __LOC__; 
         raise Exit
           
let build_extend ?i e =
  match e with
  | "uxtb" -> UXTB i
  | "uxth" -> UXTH i
  | "uxtw" -> UXTW i
  | "uxtx" -> UXTX i
  | "sxtb" -> SXTB i
  | "sxth" -> SXTH i
  | "sxtw" -> SXTW i
  | "sxtx" -> SXTX i
  | _ -> Printf.fprintf stderr "Error: pattern matching failed on: %s (%s)\n" e __LOC__; 
         raise Exit
