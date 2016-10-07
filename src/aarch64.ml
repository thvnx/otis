   type label_t =
      | Label of (string * bool * int option)

    type vector_t =
      | Register8B | Register16B
      | Register4H | Register8H
      | Register2S | Register4S
      | Register1D | Register2D
      | ElementB of int
      | ElementH of int
      | ElementS of int
      | ElementD of int

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
         end
      | Some idx ->
         begin
           match v with
           | ".b" -> ElementB idx
           | ".h" -> ElementH idx
           | ".s" -> ElementS idx
           | ".d" -> ElementD idx
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

    type immediate_t =
      | Decimal of int64
      | Hexa of int64
      | Float of float

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
      
    type addressing_t =
      | BaseRegisterImm of register_t
      | BaseRegisterOffsetImm of (register_t * immediate_t option) 
      | BaseRegisterOffsetReg of (register_t * register_t * shift_t option)
      | BaseRegisterOffsetExt of (register_t * register_t * extend_t)
      | PreIndexImm of (register_t * immediate_t)
      | PostIndexImm of (register_t * immediate_t)
      | PostIndexReg of (register_t * register_t)
      | LiteralImm of (int64 * label_t option)

    type parameter_t =
      | Register of register_t
      | Addressing of addressing_t
      | Immediate of immediate_t
      | ShiftRotate of shift_t
      | Condition of string
      | Extend of extend_t
      | Vector of register_t list

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

    let build_shift s i =
      match s with
      | "lsl" -> LSL i
      | "lsr" -> LSR i
      | "asr" -> ASR i
                     
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
