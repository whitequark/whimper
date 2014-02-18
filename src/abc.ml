module Reader : sig
  type t

  val create     : string -> t
  val offset     : t -> int
  val at_end     : t -> bool
  val rewind     : t -> unit

  val read_u8    : t -> int
  val read_u16   : t -> int
  val read_s24   : t -> int
  val read_u30   : t -> int
  val read_u32   : t -> Uint32.t
  val read_s32   : t -> Int32.t
  val read_d64   : t -> float
  val read_bytes : t -> int -> string
end = struct
  type t = {
            source : string;
    mutable pos    : int;
  }

  let create source =
    { source; pos = 0 }

  let offset r =
    r.pos

  let rewind r =
    r.pos <- 0

  let at_end r =
    (String.length r.source) = r.pos

  let read_u8 r =
    if r.pos = 0x465d then failwith "x";
    let u8 = Char.code r.source.[r.pos] in
    r.pos <- r.pos + 1;
    u8

  let read_u16 r =
    let byte1 = read_u8 r in
    let byte2 = read_u8 r in
    (byte2 lsl 8) lor byte1

  let read_s24 r =
    let byte1 = read_u8 r in
    let byte2 = read_u8 r in
    let byte3 = read_u8 r in
    let s24   = (byte3 lsl 16) lor (byte2 lsl 8) lor byte1 in
    if s24 land 0x800000 <> 0 then s24 - 0x1000000 else s24

  let read_u30 r =
    let rec read_part () =
      let byte = read_u8 r in
      if byte land 0x80 <> 0 then
        ((read_part ()) lsl 7) lor (byte land 0x7f)
      else
        byte land 0x7f
    in
    read_part ()

  let read_u32 r =
    let rec read_part () =
      let byte = read_u8 r in
      if byte land 0x80 <> 0 then
        Uint32.logor (Uint32.shift_left (read_part ()) 7) (Uint32.of_int (byte land 0x7f))
      else
        Uint32.of_int (byte land 0x7f)
    in
    read_part ()

  let read_s32 r =
    (* Handling of highest bit present in bitstream does *not*
       match avm2overview. This is correct. *)
    Uint32.to_int32 (read_u32 r)

  let read_d64 r =
    let rec read len =
      if len > 0 then
        let byte = read_u8 r in
        Int64.logor (Int64.shift_left (read (len - 1)) 8) (Int64.of_int byte)
      else
        Int64.zero
    in
    Int64.float_of_bits (read 8)

  let read_bytes r len =
    let bytes = String.sub r.source r.pos len in
    r.pos <- r.pos + len;
    bytes
end

module Writer : sig
  type t

  val create      : unit -> t
  val offset      : t -> int
  val clear       : t -> unit
  val to_string   : t -> string

  val write_u8    : t -> int -> unit
  val write_u16   : t -> int -> unit
  val write_s24   : t -> int -> unit
  val write_u30   : t -> int -> unit
  val write_u32   : t -> Uint32.t -> unit
  val write_s32   : t -> Int32.t -> unit
  val write_d64   : t -> float -> unit
  val write_bytes : t -> string -> unit
end = struct
  type t = Buffer.t

  let create () =
    Buffer.create 0

  let offset w =
    Buffer.length w

  let clear w =
    Buffer.clear w

  let to_string w =
    Buffer.contents w

  let write_u8 w u8 =
    Buffer.add_char w (Char.chr u8)

  let write_u16 w u16 =
    write_u8 w (u16 land 0xff);
    write_u8 w ((u16 lsr 8) land 0xff)

  let write_s24 w s24 =
    write_u8 w (s24 land 0xff);
    write_u8 w ((s24 asr 8)  land 0xff);
    write_u8 w ((s24 asr 16) land 0xff)

  let write_u30 w u30 =
    let rec write_part num =
      if num lsr 7 <> 0 then begin
        write_u8 w (0x80 lor (num land 0x7f));
        write_part (num lsr 7)
      end else
        write_u8 w num
    in
    write_part u30

  let write_u32 w u32 =
    let rec write_part num =
      if Uint32.shift_right num 7 <> Uint32.zero then begin
        write_u8 w (0x80 lor ((Uint32.to_int num) land 0x7f));
        write_part (Uint32.shift_right num 7)
      end else
        write_u8 w (Uint32.to_int num)
    in
    write_part u32

  let write_s32 w s32 =
    (* Handling of highest bit present in bitstream does *not*
       match avm2overview. This is correct. *)
    write_u32 w (Uint32.of_int32 s32)

  let write_d64 w d64 =
    let rec write len num =
      if len > 0 then begin
        write_u8 w ((Int64.to_int num) land 0xff);
        write (len - 1) (Int64.shift_right_logical num 8)
      end
    in
    write 8 (Int64.bits_of_float d64)

  let write_bytes w str =
    Buffer.add_string w str
end

open Sexplib.Std

type uint32 = Uint32.t
let sexp_of_uint32 n = Sexplib.Sexp.Atom (Uint32.to_string n)
let uint32_of_sexp sexp =
  let exn_to_string e = Sexplib.Sexp.to_string_hum (sexp_of_exn e) in
  match sexp with
  | Sexplib.Sexp.Atom str ->
      (try Uint32.of_string str
      with exc ->
        Sexplib.Conv.of_sexp_error ("uint32_of_sexp: " ^ exn_to_string exc) sexp)
  | Sexplib.Sexp.List _ ->
      Sexplib.Conv.of_sexp_error "uint32_of_sexp: atom needed" sexp

type file = {
  file_minor_version : (* u16 *) int;
  file_major_version : (* u16 *) int;
  file_const_pool    : constant_pool;
  file_methods       : method_ array;
  file_metadata      : metadata array;
  file_instances     : instance array;
  file_classes       : class_ array;
  file_scripts       : script array;
  file_method_bodies : method_body array;
}
and method_ref      = int
and metadata_ref    = int
and class_ref       = int
and script_ref      = int
and method_body_ref = int
and constant_pool = {
  const_pool_integers   : int32 array;
  const_pool_uintegers  : uint32 array;
  const_pool_doubles    : float array;
  const_pool_strings    : string array;
  const_pool_namespaces : namespace array;
  const_pool_ns_sets    : namespace_ref array array;
  const_pool_multinames : multiname array;
}
and integer_ref   = int
and uinteger_ref  = int
and double_ref    = int
and string_ref    = int
and namespace_ref = int
and ns_set_ref    = int
and multiname_ref = int
and constant_ref =
| ConstInt                of integer_ref
| ConstUInt               of uinteger_ref
| ConstDouble             of double_ref
| ConstUtf8               of string_ref
| ConstTrue
| ConstFalse
| ConstNull
| ConstUndefined
| ConstNamespace          of namespace_ref
| ConstPackageNamespace   of namespace_ref
| ConstPackageInternalNS  of namespace_ref
| ConstProtectedNamespace of namespace_ref
| ConstExplicitNamespace  of namespace_ref
| ConstStaticProtectedNS  of namespace_ref
| ConstPrivateNS          of namespace_ref
and namespace = {
  namespace_name : string_ref;
  namespace_kind : ns_kind;
}
and ns_kind =
| NSKindNamespace
| NSKindPackageNamespace
| NSKindPackageInternalNS
| NSKindProtectedNamespace
| NSKindExplicitNamespace
| NSKindStaticProtectedNS
| NSKindPrivateNS
and ns_set = namespace_ref array
and multiname =
| QName        of multiname_qname
| QNameA       of multiname_qname
| RTQName      of multiname_rtqname
| RTQNameA     of multiname_rtqname
| RTQNameL
| RTQNameLA
| Multiname    of multiname_multiname
| MultinameA   of multiname_multiname
| MultinameL   of multiname_multinamel
| MultinameLA  of multiname_multinamel
| GenericName  of multiname_genericname
and multiname_qname = {
  qname_ns           : namespace_ref;
  qname_name         : string_ref;
}
and multiname_rtqname = {
  rtqname_name       : string_ref;
}
and multiname_multiname = {
  multiname_name     : string_ref;
  multiname_ns_set   : ns_set_ref;
}
and multiname_multinamel = {
  multinamel_ns_set  : ns_set_ref;
}
and multiname_genericname = {
  genericname_name   : multiname_ref;
  genericname_params : multiname_ref array;
}
and method_ = {
  method_param_types     : multiname_ref array;
  method_return_type     : multiname_ref;
  method_name            : string_ref;
  method_need_arguments  : bool;
  method_need_activation : bool;
  method_need_rest       : bool;
  method_set_dxns        : bool;
  method_defaults        : constant_ref array option;
  method_param_names     : string array option;
}
and metadata = {
  metadata_name  : string_ref;
  metadata_items : metadata_item array;
}
and metadata_item = {
  metadata_key   : string_ref;
  metadata_value : string_ref;
}
and instance = {
  instance_name         : multiname_ref;
  instance_super_name   : multiname_ref;
  instance_sealed       : bool;
  instance_final        : bool;
  instance_interface    : bool;
  instance_protected_ns : namespace_ref option;
  instance_interfaces   : multiname_ref array;
  instance_initializer  : method_ref;
  instance_traits       : trait array;
}
and trait = {
  trait_name     : multiname_ref;
  trait_final    : bool;
  trait_override : bool;
  trait_slot_id  : (* u30 *) int;
  trait_data     : trait_data;
  trait_metadata : metadata_ref array option;
}
and trait_slot = {
  trait_slot_type  : multiname_ref;
  trait_slot_value : constant_ref option;
}
and trait_data =
| TraitSlot     of trait_slot
| TraitMethod   of method_ref
| TraitGetter   of method_ref
| TraitSetter   of method_ref
| TraitClass    of class_ref
| TraitFunction of method_ref
| TraitConst    of trait_slot
and class_ = {
  class_initializer  : method_ref;
  class_traits       : trait array;
}
and script = {
  script_initializer : method_ref;
  script_traits      : trait array;
}
and method_body = {
  body_method           : method_ref;
  body_max_stack        : (* u30 *) int;
  body_local_count      : (* u30 *) int;
  body_init_scope_depth : (* u30 *) int;
  body_max_scope_depth  : (* u30 *) int;
  body_code             : opcodes;
  body_exceptions       : exception_ array;
  body_traits           : trait array;
}
and exception_ = {
  exception_from     : (* u30 *) int;
  exception_to       : (* u30 *) int;
  exception_target   : (* u30 *) int;
  exception_type     : string_ref;
  exception_var_name : string_ref;
}
(* http://www.anotherbigidea.com/javaswf/avm2/AVM2Instructions.html,
   AVM2 source code and other sources *)
and opcode =
| (* 0x01; 0 -> 0 *) OpBkpt
| (* 0x02; 0 -> 0 *) OpNop
| (* 0x03; 1 -> 0 *) OpThrow
| (* 0x04; 0 -> 1 *) OpGetSuper     of (* u30 property *) multiname_ref
| (* 0x05; 1 -> 0 *) OpSetSuper     of (* u30 property *) multiname_ref
| (* 0x06; 0 -> 0 *) OpDXNS         of (* u30 string *) string_ref
| (* 0x07; 1 -> 0 *) OpDXNSLate
| (* 0x08; 0 -> 0 *) OpKill         of (* u30 local_index *) int
| (* 0x09; 0 -> 0 *) OpLabel
| (* 0x0C; 2 -> 0 *) OpIfNlt        of (* s24 target *) int
| (* 0x0D; 2 -> 0 *) OpIfNle        of (* s24 target *) int
| (* 0x0E; 2 -> 0 *) OpIfNgt        of (* s24 target *) int
| (* 0x0F; 2 -> 0 *) OpIfNge        of (* s24 target *) int
| (* 0x10; 0 -> 0 *) OpJump         of (* s24 target *) int
| (* 0x11; 1 -> 0 *) OpIfTrue       of (* s24 target *) int
| (* 0x12; 1 -> 0 *) OpIfFalse      of (* s24 target *) int
| (* 0x13; 2 -> 0 *) OpIfEq         of (* s24 target *) int
| (* 0x14; 2 -> 0 *) OpIfNe         of (* s24 target *) int
| (* 0x15; 2 -> 0 *) OpIfLt         of (* s24 target *) int
| (* 0x16; 2 -> 0 *) OpIfLe         of (* s24 target *) int
| (* 0x17; 2 -> 0 *) OpIfGt         of (* s24 target *) int
| (* 0x18; 2 -> 0 *) OpIfGe         of (* s24 target *) int
| (* 0x19; 2 -> 0 *) OpIfStrictEq   of (* s24 target *) int
| (* 0x1A; 2 -> 0 *) OpIfStrictNe   of (* s24 target *) int
| (* 0x1B; 1 -> 0 *) OpLookupSwitch of (* s24 default *) int *
                                       (* u30 case_count; s24[] case_offsets *) int array
| (* 0x1C; 1 -> 0 *) OpPushWith
| (* 0x1D; 0 -> 0 *) OpPopScope
| (* 0x1E; 2 -> 1 *) OpNextName
| (* 0x1F; 2 -> 1 *) OpHasNext
| (* 0x20; 0 -> 1 *) OpPushNull
| (* 0x21; 0 -> 1 *) OpPushUndefined
| (* 0x23; 2 -> 1 *) OpNextValue
| (* 0x24; 0 -> 1 *) OpPushByte       of (* u8 value *)  int
| (* 0x25; 0 -> 1 *) OpPushShort      of (* s32 value *) int32
| (* 0x26; 0 -> 1 *) OpPushTrue
| (* 0x27; 0 -> 1 *) OpPushFalse
| (* 0x28; 0 -> 1 *) OpPushNan
| (* 0x29; 1 -> 0 *) OpPop
| (* 0x2A; 1 -> 2 *) OpDup
| (* 0x2B; 2 -> 2 *) OpSwap
| (* 0x2C; 0 -> 1 *) OpPushString     of (* u30 value *) string_ref
| (* 0x2D; 0 -> 1 *) OpPushInt        of (* u30 value *) integer_ref
| (* 0x2E; 0 -> 1 *) OpPushUint       of (* u30 value *) uinteger_ref
| (* 0x2F; 0 -> 1 *) OpPushDouble     of (* u30 value *) double_ref
| (* 0x30; 1 -> 0 *) OpPushScope
| (* 0x31; 0 -> 0 *) OpPushNamespace  of (* u30 ns *) namespace_ref
| (* 0x32; 0 -> 1 *) OpHasNext2       of (* u30 obj_reg *) int * (* u30 idx_reg *) int
| (* 0x35; 1 -> 1 *) OpAlchemyLoadInt8
| (* 0x36; 1 -> 1 *) OpAlchemyLoadInt16
| (* 0x37; 1 -> 1 *) OpAlchemyLoadInt32
| (* 0x38; 1 -> 1 *) OpAlchemyLoadFloat32
| (* 0x39; 1 -> 1 *) OpAlchemyLoadFloat64
| (* 0x3A; 2 -> 0 *) OpAlchemyStoreInt8
| (* 0x3B; 2 -> 0 *) OpAlchemyStoreInt16
| (* 0x3C; 2 -> 0 *) OpAlchemyStoreInt32
| (* 0x3D; 2 -> 0 *) OpAlchemyStoreFloat32
| (* 0x3E; 2 -> 0 *) OpAlchemyStoreFloat64
| (* 0x40; 0 -> 1 *) OpNewFunction              of (* u30 method *) method_ref
| (* 0x41; argc + 2 -> 1 *) OpCall              of (* u30 argc *) int
| (* 0x42; argc + 1 -> 1 *) OpConstruct         of (* u30 argc *) int
| (* 0x43; argc + 1 -> 1 *) OpCallMethod        of (* u30 slot_id *) int * (* u30 argc *) int
| (* 0x44; argc + 1 -> 1 *) OpCallStatic        of (* u30 slot_id *) int * (* u30 argc *) int
| (* 0x45; argc     -> 1 *) OpCallSuper         of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x46; argc     -> 1 *) OpCallProperty      of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x47; 0 -> 0 *) OpReturnVoid
| (* 0x48; 1 -> 0 *) OpReturnValue
| (* 0x49; argc + 1 -> 0 *) OpConstructSuper    of (* u30 argc *) int
| (* 0x4A; argc     -> 1 *) OpConstructProperty of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x4C; argc     -> 1 *) OpCallPropertyLex   of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x4E; argc     -> 0 *) OpCallSuperVoid     of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x4F; argc     -> 0 *) OpCallPropertyVoid  of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x50; 1 -> 1 *) OpAlchemyExtend1
| (* 0x51; 1 -> 1 *) OpAlchemyExtend8
| (* 0x52; 1 -> 1 *) OpAlchemyExtend16
| (* 0x53; argc + 1 -> 1 *) OpApplyType         of (* u30 argc *) int
| (* 0x55; argc * 2 -> 1 *) OpNewObject         of (* u30 argc *) int
| (* 0x56; argc     -> 1 *) OpNewArray          of (* u30 argc *) int
| (* 0x57; 0 -> 1 *) OpNewActivation
| (* 0x58; 1 -> 1 *) OpNewClass           of (* u30 class *) class_ref
| (* 0x59; 0 -> 1 *) OpGetDescendants     of (* u30 prop *)  multiname_ref
| (* 0x5A; 0 -> 1 *) OpNewCatch           of (* u30 catch *) int
| (* 0x5D; 0 -> 1 *) OpFindPropertyStrict of (* u30 prop *)  multiname_ref
| (* 0x5E; 0 -> 1 *) OpFindProperty       of (* u30 prop *)  multiname_ref
| (* 0x5F; 0 -> 1 *) OpFindDef            of (* u30 prop *)  multiname_ref
| (* 0x60; 0 -> 1 *) OpGetLex             of (* u30 prop *)  multiname_ref
| (* 0x61; 1 -> 0 *) OpSetProperty        of (* u30 prop *)  multiname_ref
| (* 0x62; 0 -> 1 *) OpGetLocal           of (* u30 local *) int
| (* 0x63; 1 -> 0 *) OpSetLocal           of (* u30 local *) int
| (* 0x64; 0 -> 1 *) OpGetGlobalScope
| (* 0x65; 0 -> 1 *) OpGetScopeObject     of (* u8 index *) int
| (* 0x66; 0 -> 1 *) OpGetProperty        of (* u30 prop *) multiname_ref
| (* 0x68; 1 -> 0 *) OpInitProperty       of (* u30 prop *) multiname_ref
| (* 0x6A; 0 -> 1 *) OpDeleteProperty     of (* u30 prop *) multiname_ref
| (* 0x6C; 1 -> 1 *) OpGetSlot            of (* u30 slot_id *) int
| (* 0x6D; 2 -> 0 *) OpSetSlot            of (* u30 slot_id *) int
| (* 0x6E; 0 -> 1 *) OpGetGlobalSlot      of (* u30 slot_id *) int (* Deprecated *)
| (* 0x6F; 1 -> 0 *) OpSetGlobalSlot      of (* u30 slot_id *) int (* Deprecated *)
| (* 0x70; 1 -> 1 *) OpConvertS
| (* 0x71; 1 -> 1 *) OpEscXelem
| (* 0x72; 1 -> 1 *) OpEscXattr
| (* 0x73; 1 -> 1 *) OpConvertI
| (* 0x74; 1 -> 1 *) OpConvertU
| (* 0x75; 1 -> 1 *) OpConvertD
| (* 0x76; 1 -> 1 *) OpConvertB
| (* 0x77; 1 -> 1 *) OpConvertO
| (* 0x78; 1 -> 1 *) OpCheckFilter
| (* 0x80; 1 -> 1 *) OpCoerce       of (* u30 type *) multiname_ref
| (* 0x81; 1 -> 1 *) OpCoerceB (* Deprecated; use convert_b *)
| (* 0x82; 1 -> 1 *) OpCoerceA
| (* 0x83; 1 -> 1 *) OpCoerceI (* Deprecated; use convert_i *)
| (* 0x84; 1 -> 1 *) OpCoerceD (* Deprecated; use convert_d *)
| (* 0x85; 1 -> 1 *) OpCoerceS
| (* 0x86; 1 -> 1 *) OpAsType       of (* u30 type *) multiname_ref
| (* 0x87; 2 -> 1 *) OpAsTypeLate
| (* 0x89; 1 -> 1 *) OpCoerceO
| (* 0x90; 1 -> 1 *) OpNegate
| (* 0x91; 1 -> 1 *) OpIncrement
| (* 0x92; 0 -> 0 *) OpIncLocal     of (* u30 local *) int
| (* 0x93; 1 -> 1 *) OpDecrement
| (* 0x94; 0 -> 0 *) OpDecLocal     of (* u30 local *) int
| (* 0x95; 1 -> 1 *) OpTypeOf
| (* 0x96; 1 -> 1 *) OpNot
| (* 0x97; 1 -> 1 *) OpBitNot
| (* 0xA0; 2 -> 1 *) OpAdd
| (* 0xA1; 2 -> 1 *) OpSubtract
| (* 0xA2; 2 -> 1 *) OpMultiply
| (* 0xA3; 2 -> 1 *) OpDivide
| (* 0xA4; 2 -> 1 *) OpModulo
| (* 0xA5; 2 -> 1 *) OpLshift
| (* 0xA6; 2 -> 1 *) OpRshift
| (* 0xA7; 2 -> 1 *) OpUrshift
| (* 0xA8; 2 -> 1 *) OpBitAnd
| (* 0xA9; 2 -> 1 *) OpBitOr
| (* 0xAA; 2 -> 1 *) OpBitXor
| (* 0xAB; 2 -> 1 *) OpEquals
| (* 0xAC; 2 -> 1 *) OpStrictEquals
| (* 0xAD; 2 -> 1 *) OpLessThan
| (* 0xAE; 2 -> 1 *) OpLessEquals
| (* 0xAF; 2 -> 1 *) OpGreaterThan
| (* 0xB0; 2 -> 1 *) OpGreaterEquals
| (* 0xB1; 2 -> 1 *) OpInstanceOf
| (* 0xB2; 1 -> 1 *) OpIsType       of (* u30 type *) multiname_ref
| (* 0xB3; 2 -> 1 *) OpIsTypeLate
| (* 0xB4; 2 -> 1 *) OpIn
| (* 0xC0; 1 -> 1 *) OpIncrementI
| (* 0xC1; 1 -> 1 *) OpDecrementI
| (* 0xC2; 0 -> 0 *) OpIncLocalI    of (* u30 local *) int
| (* 0xC3; 0 -> 0 *) OpDecLocalI    of (* u30 local *) int
| (* 0xC4; 1 -> 1 *) OpNegateI
| (* 0xC5; 2 -> 1 *) OpAddI
| (* 0xC6; 2 -> 1 *) OpSubtractI
| (* 0xC7; 2 -> 1 *) OpMultiplyI
| (* 0xD0; 0 -> 1 *) OpGetLocal0
| (* 0xD1; 0 -> 1 *) OpGetLocal1
| (* 0xD2; 0 -> 1 *) OpGetLocal2
| (* 0xD3; 0 -> 1 *) OpGetLocal3
| (* 0xD4; 1 -> 0 *) OpSetLocal0
| (* 0xD5; 1 -> 0 *) OpSetLocal1
| (* 0xD6; 1 -> 0 *) OpSetLocal2
| (* 0xD7; 1 -> 0 *) OpSetLocal3
| (* 0xEF; 0 -> 0 *) OpDebug        of (* u8 type *) int * (* u30 idx *)   int *
                                       (* u8 reg *)  int * (* u30 extra *) int
| (* 0xF0; 0 -> 0 *) OpDebugLine    of (* u30 line *) int
| (* 0xF1; 0 -> 0 *) OpDebugFile    of (* u30 file *) string_ref
| (* 0xF2; 0 -> 0 *) OpBkptLine
| (* 0xF3; 0 -> 0 *) OpTimestamp
and opcodes = opcode array
with sexp

let integer file idx =
  if idx = 0 then Int32.zero
  else file.file_const_pool.const_pool_integers.(idx - 1)

let uinteger file idx =
  if idx = 0 then Uint32.zero
  else file.file_const_pool.const_pool_uintegers.(idx - 1)

let double file idx =
  if idx = 0 then 0.0
  else file.file_const_pool.const_pool_doubles.(idx - 1)

let string file idx =
  if idx = 0 then ""
  else file.file_const_pool.const_pool_strings.(idx - 1)

let namespace file idx =
  if idx = 0 then failwith "namespace #0"
  else file.file_const_pool.const_pool_namespaces.(idx - 1)

let ns_set file idx =
  if idx = 0 then failwith "ns_set #0"
  else file.file_const_pool.const_pool_ns_sets.(idx - 1)

let multiname file idx =
  if idx = 0 then failwith "multiname #0"
  else file.file_const_pool.const_pool_multinames.(idx - 1)

let method_     file idx = file.file_methods.(idx)
let metadata    file idx = file.file_metadata.(idx)
let instance    file idx = file.file_instances.(idx)
let class_      file idx = file.file_classes.(idx)
let script      file idx = file.file_scripts.(idx)
let method_body file idx = file.file_method_bodies.(idx)

let string_of_namespace file ns =
  string file (namespace file ns).namespace_name

let string_of_ns_set file set =
  Array.to_list (ns_set file set) |>
  List.map (string_of_namespace file) |>
  String.concat ", " |>
  Printf.sprintf "{%s}"

let rec string_of_multiname file name =
  match multiname file name with
  | QName { qname_ns; qname_name } | QNameA { qname_ns; qname_name } ->
    Printf.sprintf "%s::%s" (string_of_namespace file qname_ns) (string file qname_name)
  | RTQName { rtqname_name } | RTQNameA { rtqname_name } ->
    Printf.sprintf "?::%s"  (string file rtqname_name)
  | RTQNameL | RTQNameLA ->
    "?::?"
  | Multiname { multiname_ns_set; multiname_name }
  | MultinameA { multiname_ns_set; multiname_name } ->
    Printf.sprintf "%s::%s" (string_of_ns_set file multiname_ns_set) (string file multiname_name)
  | MultinameL { multinamel_ns_set } | MultinameLA { multinamel_ns_set } ->
    Printf.sprintf "%s::?" (string_of_ns_set file multinamel_ns_set)
  | GenericName { genericname_name; genericname_params } ->
    let params = List.map (string_of_multiname file) (Array.to_list genericname_params) in
    Printf.sprintf "%s<%s>" (string_of_multiname file genericname_name) (String.concat ", " params)

let string_of_opcode file opcode =
  let integer      = integer  file in
  let uinteger     = uinteger file in
  let double       = double   file in
  let lit_string s = Printf.sprintf "\"%s\"" (String.escaped (string file s)) in
  let multiname    = string_of_multiname file in
  let namespace ns = Printf.sprintf "namespace %s" (string file (namespace file ns).namespace_name) in
  let class_ cls   = Printf.sprintf "class %s" (multiname (instance file cls).instance_name) in
  let method_ meth = Printf.sprintf "method #%d" meth in
  match opcode with
  | OpBkpt                -> "bkpt"
  | OpNop                 -> "nop"
  | OpThrow               -> "throw"
  | OpGetSuper (name)     -> Printf.sprintf "getsuper %s" (multiname name)
  | OpSetSuper (name)     -> Printf.sprintf "setsuper %s" (multiname name)
  | OpDXNS (str)          -> Printf.sprintf "dxns %s" (lit_string str)
  | OpDXNSLate            -> "dxnslate"
  | OpKill (local)        -> Printf.sprintf "kill L%d" local
  | OpLabel               -> "label"
  | OpIfNlt (target)      -> Printf.sprintf "ifnlt $%04x" target
  | OpIfNle (target)      -> Printf.sprintf "ifnle $%04x" target
  | OpIfNgt (target)      -> Printf.sprintf "ifngt $%04x" target
  | OpIfNge (target)      -> Printf.sprintf "ifnge $%04x" target
  | OpJump (target)       -> Printf.sprintf "jump $%04x" target
  | OpIfTrue (target)     -> Printf.sprintf "iftrue $%04x" target
  | OpIfFalse (target)    -> Printf.sprintf "iffalse $%04x" target
  | OpIfEq (target)       -> Printf.sprintf "ifeq $%04x" target
  | OpIfNe (target)       -> Printf.sprintf "ifne $%04x" target
  | OpIfLt (target)       -> Printf.sprintf "iflt $%04x" target
  | OpIfLe (target)       -> Printf.sprintf "ifle $%04x" target
  | OpIfGt (target)       -> Printf.sprintf "ifgt $%04x" target
  | OpIfGe (target)       -> Printf.sprintf "ifge $%04x" target
  | OpIfStrictEq (target) -> Printf.sprintf "ifstricteq $%04x" target
  | OpIfStrictNe (target) -> Printf.sprintf "ifstrictne $%04x" target
  | OpLookupSwitch (default, targets) ->
                             let targets = List.map (Printf.sprintf "$%04x") (Array.to_list targets) in
                             Printf.sprintf "lookupswitch $%04x (%s)" default (String.concat ", " targets)
  | OpPushWith            -> "pushwith"
  | OpPopScope            -> "popscope"
  | OpNextName            -> "nextname"
  | OpHasNext             -> "hasnext"
  | OpPushNull            -> "pushnull"
  | OpPushUndefined       -> "pushundefined"
  | OpNextValue           -> "nextvalue"
  | OpPushByte (num)      -> Printf.sprintf "pushbyte %d" num
  | OpPushShort (num)     -> Printf.sprintf "pushshort %ld" num
  | OpPushTrue            -> "pushtrue"
  | OpPushFalse           -> "pushfalse"
  | OpPushNan             -> "pushnan"
  | OpPop                 -> "pop"
  | OpDup                 -> "dup"
  | OpSwap                -> "swap"
  | OpPushString (str)    -> Printf.sprintf "pushstring %s" (lit_string str)
  | OpPushInt (int)       -> Printf.sprintf "pushint %ld" (integer int)
  | OpPushUint (uint)     -> Printf.sprintf "pushuint %s" (Uint32.to_string (uinteger uint))
  | OpPushDouble (dbl)    -> Printf.sprintf "pushdouble %f" (double dbl)
  | OpPushScope           -> "pushscope"
  | OpPushNamespace (ns)  -> Printf.sprintf "pushnamespace [%s]" (namespace ns)
  | OpHasNext2 (obj, reg) -> Printf.sprintf "hasnext2 L%d L%d" obj reg
  | OpAlchemyLoadInt8     -> "loadint8"
  | OpAlchemyLoadInt16    -> "loadint16"
  | OpAlchemyLoadInt32    -> "loadint32"
  | OpAlchemyLoadFloat32  -> "loadfloat32"
  | OpAlchemyLoadFloat64  -> "loadfloat64"
  | OpAlchemyStoreInt8    -> "storeint8"
  | OpAlchemyStoreInt16   -> "storeint16"
  | OpAlchemyStoreInt32   -> "storeint32"
  | OpAlchemyStoreFloat32 -> "storefloat32"
  | OpAlchemyStoreFloat64 -> "storefloat64"
  | OpNewFunction (meth)  -> Printf.sprintf "newfunction %s" (method_ meth)
  | OpCall (argc)         -> Printf.sprintf "call %d" argc
  | OpConstruct (argc)    -> Printf.sprintf "construct %d" argc
  | OpCallMethod (slot_id, argc)      -> Printf.sprintf "callmethod [#%d] %d" slot_id argc
  | OpCallStatic (slot_id, argc)      -> Printf.sprintf "callstatic [#%d] %d" slot_id argc
  | OpCallSuper (name, argc)          -> Printf.sprintf "callsuper [%s] %d" (multiname name) argc
  | OpCallProperty (name, argc)       -> Printf.sprintf "callproperty [%s] %d" (multiname name) argc
  | OpReturnVoid                      -> "returnvoid"
  | OpReturnValue                     -> "returnvalue"
  | OpConstructSuper (argc)           -> Printf.sprintf "constructsuper %d" argc
  | OpConstructProperty (name, argc)  -> Printf.sprintf "constructproperty [%s] %d" (multiname name) argc
  | OpCallPropertyLex (name, argc)    -> Printf.sprintf "callpropertylex [%s] %d" (multiname name) argc
  | OpCallSuperVoid (name, argc)      -> Printf.sprintf "callsupervoid [%s] %d" (multiname name) argc
  | OpCallPropertyVoid (name, argc)   -> Printf.sprintf "callpropertyvoid [%s] %d" (multiname name) argc
  | OpAlchemyExtend1            -> "extend1"
  | OpAlchemyExtend8            -> "extend8"
  | OpAlchemyExtend16           -> "extend16"
  | OpApplyType (argc)          -> Printf.sprintf "applytype %d" argc
  | OpNewObject (argc)          -> Printf.sprintf "newobject %d" argc
  | OpNewArray (argc)           -> Printf.sprintf "newarray %d" argc
  | OpNewActivation             -> "newactivation"
  | OpNewClass (cls)            -> Printf.sprintf "newclass [%s]" (class_ cls)
  | OpGetDescendants (name)     -> Printf.sprintf "getdescendants [%s]" (multiname name)
  | OpNewCatch (catch)          -> Printf.sprintf "newcatch [%d]" catch
  | OpFindPropertyStrict (name) -> Printf.sprintf "findpropertystrict [%s]" (multiname name)
  | OpFindProperty (name)       -> Printf.sprintf "findproperty [%s]" (multiname name)
  | OpFindDef (name)            -> Printf.sprintf "finddef [%s]" (multiname name)
  | OpGetLex (name)             -> Printf.sprintf "getlex [%s]" (multiname name)
  | OpSetProperty (name)        -> Printf.sprintf "setproperty [%s]" (multiname name)
  | OpGetLocal (local)          -> Printf.sprintf "getlocal L%d" local
  | OpSetLocal (local)          -> Printf.sprintf "setlocal L%d" local
  | OpGetGlobalScope            -> "getglobalscope"
  | OpGetScopeObject (index)    -> Printf.sprintf "getscopeobject"
  | OpGetProperty (name)        -> Printf.sprintf "getproperty [%s]" (multiname name)
  | OpInitProperty (name)       -> Printf.sprintf "initproperty [%s]" (multiname name)
  | OpDeleteProperty (name)     -> Printf.sprintf "deleteproperty [%s]" (multiname name)
  | OpGetSlot (slot_id)         -> Printf.sprintf "getslot [#%d]" slot_id
  | OpSetSlot (slot_id)         -> Printf.sprintf "setslot [#%d]" slot_id
  | OpGetGlobalSlot (slot_id)   -> Printf.sprintf "getglobalslot [#%d]" slot_id
  | OpSetGlobalSlot (slot_id)   -> Printf.sprintf "setglobalslot [#%d]" slot_id
  | OpConvertS            -> "convert_s"
  | OpEscXelem            -> "esc_xelem"
  | OpEscXattr            -> "esc_xattr"
  | OpConvertI            -> "convert_i"
  | OpConvertU            -> "convert_u"
  | OpConvertD            -> "convert_d"
  | OpConvertB            -> "convert_b"
  | OpConvertO            -> "convert_o"
  | OpCheckFilter         -> "checkfilter"
  | OpCoerce (name)       -> Printf.sprintf "coerce [%s]" (multiname name)
  | OpCoerceB             -> "coerce_b"
  | OpCoerceA             -> "coerce_a"
  | OpCoerceI             -> "coerce_i"
  | OpCoerceD             -> "coerce_d"
  | OpCoerceS             -> "coerce_s"
  | OpAsType (name)       -> Printf.sprintf "astype [%s]" (multiname name)
  | OpAsTypeLate          -> "astypelate"
  | OpCoerceO             -> "coerce_o"
  | OpNegate              -> "negate"
  | OpIncrement           -> "increment"
  | OpIncLocal (local)    -> Printf.sprintf "inclocal L%d" local
  | OpDecrement           -> "decrement"
  | OpDecLocal (local)    -> Printf.sprintf "declocal L%d" local
  | OpTypeOf              -> "typeof"
  | OpNot                 -> "not"
  | OpBitNot              -> "bitnot"
  | OpAdd                 -> "add"
  | OpSubtract            -> "subtract"
  | OpMultiply            -> "multiply"
  | OpDivide              -> "divide"
  | OpModulo              -> "modulo"
  | OpLshift              -> "lshift"
  | OpRshift              -> "rshift"
  | OpUrshift             -> "urshift"
  | OpBitAnd              -> "bitand"
  | OpBitOr               -> "bitor"
  | OpBitXor              -> "bitxor"
  | OpEquals              -> "equals"
  | OpStrictEquals        -> "strictequals"
  | OpLessThan            -> "lessthan"
  | OpLessEquals          -> "lessequals"
  | OpGreaterThan         -> "greaterthan"
  | OpGreaterEquals       -> "greaterequals"
  | OpInstanceOf          -> "instanceof"
  | OpIsType (name)       -> Printf.sprintf "istype [%s]" (multiname name)
  | OpIsTypeLate          -> "istypelate"
  | OpIn                  -> "in"
  | OpIncrementI          -> "increment_i"
  | OpDecrementI          -> "decrement_i"
  | OpIncLocalI (local)   -> Printf.sprintf "inclocal_i L%d" local
  | OpDecLocalI (local)   -> Printf.sprintf "declocal_i L%d" local
  | OpNegateI             -> "negate_i"
  | OpAddI                -> "add_i"
  | OpSubtractI           -> "subtract_i"
  | OpMultiplyI           -> "multiply_i"
  | OpGetLocal0           -> "getlocal_0"
  | OpGetLocal1           -> "getlocal_1"
  | OpGetLocal2           -> "getlocal_2"
  | OpGetLocal3           -> "getlocal_3"
  | OpSetLocal0           -> "setlocal_0"
  | OpSetLocal1           -> "setlocal_1"
  | OpSetLocal2           -> "setlocal_2"
  | OpSetLocal3           -> "setlocal_3"
  | OpDebug (ty, idx, reg, extra) ->
                             begin match ty with
                             | 1 (* DI_LOCAL *) ->
                               Printf.sprintf "debug DI_LOCAL %s L%d" (lit_string idx) reg
                             | _ -> Printf.sprintf "debug %d %d %d %d" ty idx reg extra
                             end
  | OpDebugLine (line)    -> Printf.sprintf "debugline %d" line
  | OpDebugFile (fname)   -> Printf.sprintf "debugfile %s" (lit_string fname)
  | OpBkptLine            -> "bkptline"
  | OpTimestamp           -> "timestamp"

let disassemble file opcodes =
  opcodes |>
  Array.mapi (fun idx opcode ->
    Printf.sprintf "%04x: %s" idx (string_of_opcode file opcode)) |>
  Array.to_list |>
  String.concat "\n"

let rec read_some r f n =
  let rec loop n rest =
    if n > 0 then loop (n - 1) ((f ()) :: rest) else rest
  in
  Array.of_list (List.rev (loop n []))

let read_array r f =
  read_some r f (Reader.read_u30 r)

let load_code input =
  let r = Reader.create input in
  let read_some = read_some r in
  let read_opcode map_offset =
    let offset = Reader.offset r in
    let map_jump byte = map_offset (offset + 4 + byte) in
    match Reader.read_u8 r with
    | 0x01 -> OpBkpt
    | 0x02 -> OpNop
    | 0x03 -> OpThrow
    | 0x04 -> OpGetSuper     ((* u30 property *) Reader.read_u30 r)
    | 0x05 -> OpSetSuper     ((* u30 property *) Reader.read_u30 r)
    | 0x06 -> OpDXNS         ((* u30 string *) Reader.read_u30 r)
    | 0x07 -> OpDXNSLate
    | 0x08 -> OpKill         ((* u30 local_index *) Reader.read_u30 r)
    | 0x09 -> OpLabel
    | 0x0C -> OpIfNlt        ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x0D -> OpIfNle        ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x0E -> OpIfNgt        ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x0F -> OpIfNge        ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x10 -> OpJump         ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x11 -> OpIfTrue       ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x12 -> OpIfFalse      ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x13 -> OpIfEq         ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x14 -> OpIfNe         ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x15 -> OpIfLt         ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x16 -> OpIfLe         ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x17 -> OpIfGt         ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x18 -> OpIfGe         ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x19 -> OpIfStrictEq   ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x1A -> OpIfStrictNe   ((* s24 target *) map_jump (Reader.read_s24 r))
    | 0x1B -> let default  = map_offset (offset + (Reader.read_s24 r)) in
              let case_num = Reader.read_u30 r in
              let cases    = read_some (fun () -> map_offset (offset + (Reader.read_s24 r)))
                                       (case_num + 1) in
              OpLookupSwitch ((* s24 default *) default,
                              (* u30 case_count; s24[] case_offsets *) cases)
    | 0x1C -> OpPushWith
    | 0x1D -> OpPopScope
    | 0x1E -> OpNextName
    | 0x1F -> OpHasNext
    | 0x20 -> OpPushNull
    | 0x21 -> OpPushUndefined
    | 0x23 -> OpNextValue
    | 0x24 -> OpPushByte       ((* u8 value *) Reader.read_u8 r)
    | 0x25 -> OpPushShort      ((* s32 value *) Reader.read_s32 r)
    | 0x26 -> OpPushTrue
    | 0x27 -> OpPushFalse
    | 0x28 -> OpPushNan
    | 0x29 -> OpPop
    | 0x2A -> OpDup
    | 0x2B -> OpSwap
    | 0x2C -> OpPushString     ((* u30 value *) Reader.read_u30 r)
    | 0x2D -> OpPushInt        ((* u30 value *) Reader.read_u30 r)
    | 0x2E -> OpPushUint       ((* u30 value *) Reader.read_u30 r)
    | 0x2F -> OpPushDouble     ((* u30 value *) Reader.read_u30 r)
    | 0x30 -> OpPushScope
    | 0x31 -> OpPushNamespace  ((* u30 ns *) Reader.read_u30 r)
    | 0x32 -> let obj_reg = Reader.read_u30 r in
              let idx_reg = Reader.read_u30 r in
              OpHasNext2       ((* u30 obj_reg *) obj_reg, (* u30 idx_reg *) idx_reg)
    | 0x35 -> OpAlchemyLoadInt8
    | 0x36 -> OpAlchemyLoadInt16
    | 0x37 -> OpAlchemyLoadInt32
    | 0x38 -> OpAlchemyLoadFloat32
    | 0x39 -> OpAlchemyLoadFloat64
    | 0x3A -> OpAlchemyStoreInt8
    | 0x3B -> OpAlchemyStoreInt16
    | 0x3C -> OpAlchemyStoreInt32
    | 0x3D -> OpAlchemyStoreFloat32
    | 0x3E -> OpAlchemyStoreFloat64
    | 0x40 -> OpNewFunction       ((* u30 method *) Reader.read_u30 r)
    | 0x41 -> OpCall              ((* u30 argc *) Reader.read_u30 r)
    | 0x42 -> OpConstruct         ((* u30 argc *) Reader.read_u30 r)
    | 0x43 -> let slot_id = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpCallMethod        ((* u30 slot_id *) slot_id, (* u30 argc *) argc)
    | 0x44 -> let slot_id = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpCallStatic        ((* u30 slot_id *) slot_id, (* u30 argc *) argc)
    | 0x45 -> let prop    = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpCallSuper         ((* u30 prop *) prop, (* u30 argc *) argc)
    | 0x46 -> let prop    = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpCallProperty      ((* u30 prop *) prop, (* u30 argc *) argc)
    | 0x47 -> OpReturnVoid
    | 0x48 -> OpReturnValue
    | 0x49 -> OpConstructSuper    ((* u30 argc *) Reader.read_u30 r)
    | 0x4A -> let prop    = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpConstructProperty ((* u30 prop *) prop, (* u30 argc *) argc)
    | 0x4C -> let prop    = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpCallPropertyLex   ((* u30 prop *) prop, (* u30 argc *) argc)
    | 0x4E -> let prop    = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpCallSuperVoid     ((* u30 prop *) prop, (* u30 argc *) argc)
    | 0x4F -> let prop    = Reader.read_u30 r in
              let argc    = Reader.read_u30 r in
              OpCallPropertyVoid  ((* u30 prop *) prop, (* u30 argc *) argc)
    | 0x50 -> OpAlchemyExtend1
    | 0x51 -> OpAlchemyExtend8
    | 0x52 -> OpAlchemyExtend16
    | 0x53 -> OpApplyType          ((* u30 argc *) Reader.read_u30 r)
    | 0x55 -> OpNewObject          ((* u30 argc *) Reader.read_u30 r)
    | 0x56 -> OpNewArray           ((* u30 argc *) Reader.read_u30 r)
    | 0x57 -> OpNewActivation
    | 0x58 -> OpNewClass           ((* u30 class *) Reader.read_u30 r)
    | 0x59 -> OpGetDescendants     ((* u30 prop *) Reader.read_u30 r)
    | 0x5A -> OpNewCatch           ((* u30 catch *) Reader.read_u30 r)
    | 0x5D -> OpFindPropertyStrict ((* u30 prop *) Reader.read_u30 r)
    | 0x5E -> OpFindProperty       ((* u30 prop *) Reader.read_u30 r)
    | 0x5F -> OpFindDef            ((* u30 prop *) Reader.read_u30 r)
    | 0x60 -> OpGetLex             ((* u30 prop *) Reader.read_u30 r)
    | 0x61 -> OpSetProperty        ((* u30 prop *) Reader.read_u30 r)
    | 0x62 -> OpGetLocal           ((* u30 local *) Reader.read_u30 r)
    | 0x63 -> OpSetLocal           ((* u30 local *) Reader.read_u30 r)
    | 0x64 -> OpGetGlobalScope
    | 0x65 -> OpGetScopeObject     ((* u8 index *) Reader.read_u8 r)
    | 0x66 -> OpGetProperty        ((* u30 prop *) Reader.read_u30 r)
    | 0x68 -> OpInitProperty       ((* u30 prop *) Reader.read_u30 r)
    | 0x6A -> OpDeleteProperty     ((* u30 prop *) Reader.read_u30 r)
    | 0x6C -> OpGetSlot            ((* u30 slot_id *) Reader.read_u30 r)
    | 0x6D -> OpSetSlot            ((* u30 slot_id *) Reader.read_u30 r)
    | 0x6E -> OpGetGlobalSlot      ((* u30 slot_id *) Reader.read_u30 r)
    | 0x6F -> OpSetGlobalSlot      ((* u30 slot_id *) Reader.read_u30 r)
    | 0x70 -> OpConvertS
    | 0x71 -> OpEscXelem
    | 0x72 -> OpEscXattr
    | 0x73 -> OpConvertI
    | 0x74 -> OpConvertU
    | 0x75 -> OpConvertD
    | 0x76 -> OpConvertB
    | 0x77 -> OpConvertO
    | 0x78 -> OpCheckFilter
    | 0x80 -> OpCoerce       ((* u30 type *) Reader.read_u30 r)
    | 0x81 -> OpCoerceB
    | 0x82 -> OpCoerceA
    | 0x83 -> OpCoerceI
    | 0x84 -> OpCoerceD
    | 0x85 -> OpCoerceS
    | 0x86 -> OpAsType       ((* u30 type *) Reader.read_u30 r)
    | 0x87 -> OpAsTypeLate
    | 0x89 -> OpCoerceO
    | 0x90 -> OpNegate
    | 0x91 -> OpIncrement
    | 0x92 -> OpIncLocal     ((* u30 local *) Reader.read_u30 r)
    | 0x93 -> OpDecrement
    | 0x94 -> OpDecLocal     ((* u30 local *) Reader.read_u30 r)
    | 0x95 -> OpTypeOf
    | 0x96 -> OpNot
    | 0x97 -> OpBitNot
    | 0xA0 -> OpAdd
    | 0xA1 -> OpSubtract
    | 0xA2 -> OpMultiply
    | 0xA3 -> OpDivide
    | 0xA4 -> OpModulo
    | 0xA5 -> OpLshift
    | 0xA6 -> OpRshift
    | 0xA7 -> OpUrshift
    | 0xA8 -> OpBitAnd
    | 0xA9 -> OpBitOr
    | 0xAA -> OpBitXor
    | 0xAB -> OpEquals
    | 0xAC -> OpStrictEquals
    | 0xAD -> OpLessThan
    | 0xAE -> OpLessEquals
    | 0xAF -> OpGreaterThan
    | 0xB0 -> OpGreaterEquals
    | 0xB1 -> OpInstanceOf
    | 0xB2 -> OpIsType       ((* u30 type *) Reader.read_u30 r)
    | 0xB3 -> OpIsTypeLate
    | 0xB4 -> OpIn
    | 0xC0 -> OpIncrementI
    | 0xC1 -> OpDecrementI
    | 0xC2 -> OpIncLocalI    ((* u30 local *) Reader.read_u30 r)
    | 0xC3 -> OpDecLocalI    ((* u30 local *) Reader.read_u30 r)
    | 0xC4 -> OpNegateI
    | 0xC5 -> OpAddI
    | 0xC6 -> OpSubtractI
    | 0xC7 -> OpMultiplyI
    | 0xD0 -> OpGetLocal0
    | 0xD1 -> OpGetLocal1
    | 0xD2 -> OpGetLocal2
    | 0xD3 -> OpGetLocal3
    | 0xD4 -> OpSetLocal0
    | 0xD5 -> OpSetLocal1
    | 0xD6 -> OpSetLocal2
    | 0xD7 -> OpSetLocal3
    | 0xEF -> let ty    = Reader.read_u8  r in
              let idx   = Reader.read_u30 r in
              let reg   = Reader.read_u8  r in
              let extra = Reader.read_u30 r in
              OpDebug        ((* u8 type *) ty, (* u30 idx *) idx, (* u8 reg *) reg, (* u30 extra *) extra)
    | 0xF0 -> OpDebugLine    ((* u30 line *) Reader.read_u30 r)
    | 0xF1 -> OpDebugFile    ((* u30 file *) Reader.read_u30 r)
    | 0xF2 -> OpBkptLine
    | 0xF3 -> OpTimestamp
    | opco -> failwith (Printf.sprintf "read_opcode: unknown opcode 0x%02x" opco)
  in
  (* Create a map of byte offsets -> instruction numbers *)
  let index = Array.make (String.length input) (-1) in
  let rec make_index n =
    if Reader.at_end r then ()
    else begin
      index.(Reader.offset r) <- n;
      ignore (read_opcode (fun x -> x));
      make_index (n + 1)
    end
  in
  make_index 0;
  Reader.rewind r;
  (* Actually read the opcodes *)
  let rec make_opcodes rest =
    if Reader.at_end r then rest
    else
      let map_index byte =
        let opcode = index.(byte) in
        assert (opcode <> (-1));
        opcode
      in
      make_opcodes ((read_opcode map_index) :: rest)
  in
  index, Array.of_list (List.rev (make_opcodes []))

let load_file input =
  let r = Reader.create input in
  let read_some  f n = read_some  r f n in
  let read_array f   = read_array r f in
  (* abcFile {
      u16 minor_version
      u16 major_version
      cpool_info constant_pool
      u30 method_count
      method_info method[method_count]
      u30 metadata_count
      metadata_info metadata[metadata_count]
      u30 class_count
      instance_info instance[class_count]
      class_info class[class_count]
      u30 script_count
      script_info script[script_count]
      u30 method_body_count
      method_body_info method_body[method_body_count]
    } *)
  let rec read_file () =
    let minor     = Reader.read_u16 r in
    let major     = Reader.read_u16 r in
    let cpool     = read_constant_pool () in
    let methods   = read_array read_method in
    let metadata  = read_array read_metadata in
    let class_cnt = Reader.read_u30 r in
    let instances = read_some read_instance class_cnt in
    let classes   = read_some read_class class_cnt in
    let scripts   = read_array read_script in
    let bodies    = read_array read_method_body in
    { file_minor_version = minor;
      file_major_version = major;
      file_const_pool    = cpool;
      file_methods       = methods;
      file_metadata      = metadata;
      file_instances     = instances;
      file_classes       = classes;
      file_scripts       = scripts;
      file_method_bodies = bodies; }
  (* cpool_info {
      u30 int_count
      s32 integer[int_count]
      u30 uint_count
      u32 uinteger[uint_count]
      u30 double_count
      d64 double[double_count]
      u30 string_count
      string_info string[string_count]
      u30 namespace_count
      namespace_info namespace[namespace_count]
      u30 ns_set_count
      ns_set_info ns_set[ns_set_count]
      u30 multiname_count
      multiname_info multiname[multiname_count]
    } *)
  and read_constant_pool () =
    let read_array f =
      (* Const pool has zero element omitted from file *)
      read_some f ((Reader.read_u30 r) - 1)
    in
    let integers   = read_array (fun () -> Reader.read_s32 r) in
    let uintegers  = read_array (fun () -> Reader.read_u32 r) in
    let doubles    = read_array (fun () -> Reader.read_d64 r) in
    let strings    = read_array read_string in
    let namespaces = read_array read_namespace in
    let ns_sets    = read_array read_ns_set in
    let multinames = read_array read_multiname in
    { const_pool_integers   = integers;
      const_pool_uintegers  = uintegers;
      const_pool_doubles    = doubles;
      const_pool_strings    = strings;
      const_pool_namespaces = namespaces;
      const_pool_ns_sets    = ns_sets;
      const_pool_multinames = multinames; }
  (* string_info {
      u30 size
      u8 utf8[size]
    } *)
  and read_string () =
    let size = Reader.read_u30 r in
    Reader.read_bytes r size
  (* namespace_info {
      u8 kind
      u30 name
    } *)
  and read_namespace () =
    let kind = Reader.read_u8 r in
    let name = Reader.read_u30 r in
    let kind = match kind with
      | 0x08 -> NSKindNamespace
      | 0x16 -> NSKindPackageNamespace
      | 0x17 -> NSKindPackageInternalNS
      | 0x18 -> NSKindProtectedNamespace
      | 0x19 -> NSKindExplicitNamespace
      | 0x1A -> NSKindStaticProtectedNS
      | 0x05 -> NSKindPrivateNS
      | kind -> failwith (Printf.sprintf "read_namespace: unknown kind 0x%02x" kind)
    in
    { namespace_kind = kind;
      namespace_name = name; }
  (* ns_set_info {
      u30 count
      u30 ns[count]
    } *)
  and read_ns_set () =
    read_array (fun () -> Reader.read_u30 r)
  (* multiname_info {
      u8 kind
      u8 data[]
    } *)
  and read_multiname () =
    match Reader.read_u8 r with
    | 0x07 -> QName (read_multiname_qname ())
    | 0x0D -> QNameA (read_multiname_qname ())
    | 0x0F -> RTQName (read_multiname_rtqname ())
    | 0x10 -> RTQNameA (read_multiname_rtqname ())
    | 0x11 -> RTQNameL
    | 0x12 -> RTQNameLA
    | 0x09 -> Multiname (read_multiname_multiname ())
    | 0x0E -> MultinameA (read_multiname_multiname ())
    | 0x1B -> MultinameL (read_multiname_multinamel ())
    | 0x1C -> MultinameLA (read_multiname_multinamel ())
    (* Undocumented in avm2overview. *)
    | 0x1D -> GenericName (read_multiname_genericname ())
    | kind -> failwith (Printf.sprintf "read_multiname: unknown kind 0x%02x" kind)
  (* multiname_kind_QName {
      u30 ns
      u30 name
    } *)
  and read_multiname_qname () =
    let ns   = Reader.read_u30 r in
    let name = Reader.read_u30 r in
    { qname_ns = ns;
      qname_name = name; }
  (* multiname_kind_RTQName {
      u30 name
    } *)
  and read_multiname_rtqname () =
    { rtqname_name = Reader.read_u30 r; }
  (* multiname_kind_RTQNameL {
    } *)
  (* multiname_kind_Multiname {
      u30 name
      u30 ns_set
    } *)
  and read_multiname_multiname () =
    let name   = Reader.read_u30 r in
    let ns_set = Reader.read_u30 r in
    { multiname_name   = name;
      multiname_ns_set = ns_set; }
  (* multiname_kind_MultinameL {
      u30 ns_set
    } *)
  and read_multiname_multinamel () =
    { multinamel_ns_set = Reader.read_u30 r; }
  (* Undocumented in avm2overview. *)
  and read_multiname_genericname () =
    let name   = Reader.read_u30 r in
    let params = read_array (fun () -> Reader.read_u30 r) in
    { genericname_name   = name;
      genericname_params = params; }
  (* Common code for option_detail and trait_slot. *)
  and make_constant kind index =
    match kind with
    | 0x03 -> ConstInt index
    | 0x04 -> ConstUInt index
    | 0x06 -> ConstDouble index
    | 0x01 -> ConstUtf8 index
    | 0x0B -> ConstTrue
    | 0x0A -> ConstFalse
    | 0x0C -> ConstNull
    | 0x00 -> ConstUndefined
    | 0x08 -> ConstNamespace index
    | 0x16 -> ConstPackageNamespace index
    | 0x17 -> ConstPackageInternalNS index
    | 0x18 -> ConstProtectedNamespace index
    | 0x19 -> ConstExplicitNamespace index
    | 0x1A -> ConstStaticProtectedNS index
    | 0x05 -> ConstPrivateNS index
    | kind -> failwith (Printf.sprintf "make_constant: unknown kind 0x%02x" kind)
  (* method_info {
      u30 param_count
      u30 return_type
      u30 param_type[param_count]
      u30 name
      u8 flags
      option_info options
      param_info param_names
    } *)
  and read_method () =
    let param_count = Reader.read_u30 r in
    let return_type = Reader.read_u30 r in
    let param_types = read_some (fun () -> Reader.read_u30 r) param_count in
    let name        = Reader.read_u30 r in
    let flags       = Reader.read_u8 r in
    (* NEED_ARGUMENTS  0x01
       NEED_ACTIVATION 0x02
       NEED_REST       0x04
       HAS_OPTIONAL    0x08
       SET_DXNS        0x40
       HAS_PARAM_NAMES 0x80 *)
    let need_arguments  = flags land 0x01 <> 0 in
    let need_activation = flags land 0x02 <> 0 in
    let need_rest       = flags land 0x04 <> 0 in
    let has_optional    = flags land 0x08 <> 0 in
    let set_dxns        = flags land 0x40 <> 0 in
    let has_param_names = flags land 0x80 <> 0 in
    let flags_rest = flags land (lnot 0xcf) in
    if flags_rest <> 0 then
      failwith (Printf.sprintf "read_method: unknown flags 0x%02x" flags_rest);
    (* option_info {
        u30 option_count
        option_detail option[option_count]
      } *)
    let defaults =
      if has_optional then Some (read_array read_default) else None
    in
    let param_names =
      if has_param_names then Some (read_some read_string param_count) else None
    in
    { method_param_types     = param_types;
      method_return_type     = return_type;
      method_name            = name;
      method_need_arguments  = need_arguments;
      method_need_activation = need_activation;
      method_need_rest       = need_rest;
      method_set_dxns        = set_dxns;
      method_defaults        = defaults;
      method_param_names     = param_names; }
  (* option_detail {
      u30 val
      u8 kind
    } *)
  and read_default () =
    let index = Reader.read_u30 r in
    make_constant (Reader.read_u8 r) index
  (* metadata_info {
      u30 name
      u30 item_count
      item_info items[item_count]
    } *)
  and read_metadata () =
    let name  = Reader.read_u30 r in
    let items = read_array read_metadata_item in
    { metadata_name  = name;
      metadata_items = items; }
  (* item_info {
      u30 key
      u30 value
    } *)
  and read_metadata_item () =
    let key  = Reader.read_u30 r in
    let data = Reader.read_u30 r in
    { metadata_key   = key;
      metadata_value = data; }
  (* traits_info {
      u30 name
      u8 kind
      u8 data[]
      u30 metadata_count
      u30 metadata[metadata_count]
    } *)
  and read_trait () =
    let name = Reader.read_u30 r in
    let kind = Reader.read_u8 r in
    (* ATTR_Final    0x1
       ATTR_Override 0x2
       ATTR_Metadata 0x4 *)
    let final        = kind land 0x10 <> 0 in
    let override     = kind land 0x20 <> 0 in
    let has_metadata = kind land 0x40 <> 0 in
    let flags_rest = kind land (lnot 0x7f) in
    if flags_rest <> 0 then
      failwith (Printf.sprintf "read_trait: unknown flags 0x%02x" flags_rest);
    let slot_id = Reader.read_u30 r in
    (* trait_class {
        u30 slot_id
        u30 classi
      } *)
    (* trait_function {
        u30 slot_id
        u30 function
      } *)
    (* trait_method {
        u30 disp_id
        u30 method
      } *)
    let data =
      match kind land 0xf with
      | 0 -> TraitSlot (read_trait_slot ())
      | 1 -> TraitMethod (Reader.read_u30 r)
      | 2 -> TraitGetter (Reader.read_u30 r)
      | 3 -> TraitSetter (Reader.read_u30 r)
      | 4 -> TraitClass (Reader.read_u30 r)
      | 5 -> TraitFunction (Reader.read_u30 r)
      | 6 -> TraitConst (read_trait_slot ())
      | _ -> failwith (Printf.sprintf "read_trait: unknown kind %d" (kind land 0xf));
    in
    let metadata =
      if has_metadata then Some (read_array (fun () -> Reader.read_u30 r)) else None
    in
    { trait_name     = name;
      trait_final    = final;
      trait_override = override;
      trait_slot_id  = slot_id;
      trait_data     = data;
      trait_metadata = metadata; }
  (* trait_slot {
      u30 slot_id
      u30 type_name
      u30 vindex
      u8  vkind
    } *)
  and read_trait_slot () =
    let type_name = Reader.read_u30 r in
    let index     = Reader.read_u30 r in
    let const =
      if index <> 0 then Some (make_constant (Reader.read_u8 r) index) else None
    in
    { trait_slot_type  = type_name;
      trait_slot_value = const; }
  (* instance_info {
      u30 name
      u30 super_name
      u8 flags
      u30 protectedNs
      u30 intrf_count
      u30 interface[intrf_count]
      u30 iinit
      u30 trait_count
      traits_info trait[trait_count]
    } *)
  and read_instance () =
    let name       = Reader.read_u30 r in
    let super_name = Reader.read_u30 r in
    let flags      = Reader.read_u8 r in
    (* ClassSealed       0x01
       ClassFinal        0x02
       ClassInterface    0x04
       ClassProtectedNs  0x08 *)
    let sealed           = flags land 0x01 <> 0 in
    let final            = flags land 0x02 <> 0 in
    let interface        = flags land 0x04 <> 0 in
    let has_protected_ns = flags land 0x08 <> 0 in
    let flags_rest = flags land (lnot 0x0f) in
    if flags_rest <> 0 then
      failwith (Printf.sprintf "read_instance: unknown flags 0x%02x" flags_rest);
    let protected_ns =
      if has_protected_ns then Some (Reader.read_u30 r) else None
    in
    let interfaces = read_array (fun () -> Reader.read_u30 r) in
    let init       = Reader.read_u30 r in
    let traits     = read_array read_trait in
    { instance_name         = name;
      instance_super_name   = super_name;
      instance_sealed       = sealed;
      instance_final        = final;
      instance_interface    = interface;
      instance_protected_ns = protected_ns;
      instance_interfaces   = interfaces;
      instance_initializer  = init;
      instance_traits       = traits; }
  (* class_info {
      u30 cinit
      u30 trait_count
      traits_info traits[trait_count]
    } *)
  and read_class () =
    let init   = Reader.read_u30 r in
    let traits = read_array read_trait in
    { class_initializer = init;
      class_traits      = traits; }
  (* script_info {
      u30 init
      u30 trait_count
      traits_info trait[trait_count]
    } *)
  and read_script () =
    let init   = Reader.read_u30 r in
    let traits = read_array read_trait in
    { script_initializer = init;
      script_traits      = traits; }
  (* method_body_info {
      u30 method
      u30 max_stack
      u30 local_count
      u30 init_scope_depth
      u30 max_scope_depth
      u30 code_length
      u8 code[code_length]
      u30 exception_count
      exception_info exception[exception_count]
      u30 trait_count
      traits_info trait[trait_count]
    } *)
  and read_method_body () =
    let method_idx       = Reader.read_u30 r in
    let max_stack        = Reader.read_u30 r in
    let local_count      = Reader.read_u30 r in
    let init_scope_depth = Reader.read_u30 r in
    let max_scope_depth  = Reader.read_u30 r in
    let index, opcodes   = load_code (read_string ()) in
    let exceptions       = read_array (read_exception index) in
    let traits           = read_array read_trait in
    { body_method           = method_idx;
      body_max_stack        = max_stack;
      body_local_count      = local_count;
      body_init_scope_depth = init_scope_depth;
      body_max_scope_depth  = max_scope_depth;
      body_code             = opcodes;
      body_exceptions       = exceptions;
      body_traits           = traits; }
  (* exception_info {
    u30 from
    u30 to
    u30 target
    u30 exc_type
    u30 var_name
    } *)
  and read_exception index () =
    let map_offset byte =
      let opcode = index.(byte) in
      assert (opcode <> (-1));
      opcode
    in
    let from_pc   = map_offset (Reader.read_u30 r) in
    let to_pc     = map_offset (Reader.read_u30 r) in
    let target_pc = map_offset (Reader.read_u30 r) in
    let exc_type  = Reader.read_u30 r in
    let var_name  = Reader.read_u30 r in
    { exception_from     = from_pc;
      exception_to       = to_pc;
      exception_target   = target_pc;
      exception_type     = exc_type;
      exception_var_name = var_name; }
  in
  read_file ()

let write_array w f arr =
  Writer.write_u30 w (Array.length arr);
  Array.iter f arr

let dump_code opcodes =
  let w = Writer.create () in
  let write_opcode index opcode =
    let offset = Writer.offset w in
    let map_offset num = index.(num) - (offset + 4) in
    let map_case num = index.(num) - offset in
    match opcode with
    | OpBkpt                -> Writer.write_u8 w 0x01
    | OpNop                 -> Writer.write_u8 w 0x02
    | OpThrow               -> Writer.write_u8 w 0x03
    | OpGetSuper (name)     -> Writer.write_u8 w 0x04; Writer.write_u30 w name
    | OpSetSuper (name)     -> Writer.write_u8 w 0x05; Writer.write_u30 w name
    | OpDXNS (str)          -> Writer.write_u8 w 0x06; Writer.write_u30 w str
    | OpDXNSLate            -> Writer.write_u8 w 0x07
    | OpKill (local)        -> Writer.write_u8 w 0x08; Writer.write_u30 w local
    | OpLabel               -> Writer.write_u8 w 0x09
    | OpIfNlt (target)      -> Writer.write_u8 w 0x0C; Writer.write_s24 w (map_offset target)
    | OpIfNle (target)      -> Writer.write_u8 w 0x0D; Writer.write_s24 w (map_offset target)
    | OpIfNgt (target)      -> Writer.write_u8 w 0x0E; Writer.write_s24 w (map_offset target)
    | OpIfNge (target)      -> Writer.write_u8 w 0x0F; Writer.write_s24 w (map_offset target)
    | OpJump (target)       -> Writer.write_u8 w 0x10; Writer.write_s24 w (map_offset target)
    | OpIfTrue (target)     -> Writer.write_u8 w 0x11; Writer.write_s24 w (map_offset target)
    | OpIfFalse (target)    -> Writer.write_u8 w 0x12; Writer.write_s24 w (map_offset target)
    | OpIfEq (target)       -> Writer.write_u8 w 0x13; Writer.write_s24 w (map_offset target)
    | OpIfNe (target)       -> Writer.write_u8 w 0x14; Writer.write_s24 w (map_offset target)
    | OpIfLt (target)       -> Writer.write_u8 w 0x15; Writer.write_s24 w (map_offset target)
    | OpIfLe (target)       -> Writer.write_u8 w 0x16; Writer.write_s24 w (map_offset target)
    | OpIfGt (target)       -> Writer.write_u8 w 0x17; Writer.write_s24 w (map_offset target)
    | OpIfGe (target)       -> Writer.write_u8 w 0x18; Writer.write_s24 w (map_offset target)
    | OpIfStrictEq (target) -> Writer.write_u8 w 0x19; Writer.write_s24 w (map_offset target)
    | OpIfStrictNe (target) -> Writer.write_u8 w 0x1A; Writer.write_s24 w (map_offset target)
    | OpLookupSwitch (default, cases) ->
                               Writer.write_u8 w 0x1B; Writer.write_s24 w (map_case default);
                               Writer.write_u30 w (Array.length cases - 1);
                               Array.iter (fun case -> Writer.write_s24 w (map_case case)) cases
    | OpPushWith            -> Writer.write_u8 w 0x1C
    | OpPopScope            -> Writer.write_u8 w 0x1D
    | OpNextName            -> Writer.write_u8 w 0x1E
    | OpHasNext             -> Writer.write_u8 w 0x1F
    | OpPushNull            -> Writer.write_u8 w 0x20
    | OpPushUndefined       -> Writer.write_u8 w 0x21
    | OpNextValue           -> Writer.write_u8 w 0x23
    | OpPushByte (byte)     -> Writer.write_u8 w 0x24; Writer.write_u8 w byte
    | OpPushShort (short)   -> Writer.write_u8 w 0x25; Writer.write_s32 w short
    | OpPushTrue            -> Writer.write_u8 w 0x26
    | OpPushFalse           -> Writer.write_u8 w 0x27
    | OpPushNan             -> Writer.write_u8 w 0x28
    | OpPop                 -> Writer.write_u8 w 0x29
    | OpDup                 -> Writer.write_u8 w 0x2A
    | OpSwap                -> Writer.write_u8 w 0x2B
    | OpPushString (str)    -> Writer.write_u8 w 0x2C; Writer.write_u30 w str
    | OpPushInt (int)       -> Writer.write_u8 w 0x2D; Writer.write_u30 w int
    | OpPushUint (uint)     -> Writer.write_u8 w 0x2E; Writer.write_u30 w uint
    | OpPushDouble (dbl)    -> Writer.write_u8 w 0x2F; Writer.write_u30 w dbl
    | OpPushScope           -> Writer.write_u8 w 0x30
    | OpPushNamespace (ns)  -> Writer.write_u8 w 0x31; Writer.write_u30 w ns
    | OpHasNext2 (obj, reg) -> Writer.write_u8 w 0x32; Writer.write_u30 w obj; Writer.write_u30 w reg
    | OpAlchemyLoadInt8     -> Writer.write_u8 w 0x35
    | OpAlchemyLoadInt16    -> Writer.write_u8 w 0x36
    | OpAlchemyLoadInt32    -> Writer.write_u8 w 0x37
    | OpAlchemyLoadFloat32  -> Writer.write_u8 w 0x38
    | OpAlchemyLoadFloat64  -> Writer.write_u8 w 0x39
    | OpAlchemyStoreInt8    -> Writer.write_u8 w 0x3A
    | OpAlchemyStoreInt16   -> Writer.write_u8 w 0x3B
    | OpAlchemyStoreInt32   -> Writer.write_u8 w 0x3C
    | OpAlchemyStoreFloat32 -> Writer.write_u8 w 0x3D
    | OpAlchemyStoreFloat64 -> Writer.write_u8 w 0x3E
    | OpNewFunction (meth)  -> Writer.write_u8 w 0x40; Writer.write_u30 w meth
    | OpCall (argc)                     -> Writer.write_u8 w 0x41; Writer.write_u30 w argc
    | OpConstruct (argc)                -> Writer.write_u8 w 0x42; Writer.write_u30 w argc
    | OpCallMethod (slot_id, argc)      -> Writer.write_u8 w 0x43; Writer.write_u30 w slot_id; Writer.write_u30 w argc
    | OpCallStatic (slot_id, argc)      -> Writer.write_u8 w 0x44; Writer.write_u30 w slot_id; Writer.write_u30 w argc
    | OpCallSuper (prop, argc)          -> Writer.write_u8 w 0x45; Writer.write_u30 w prop; Writer.write_u30 w argc
    | OpCallProperty (prop, argc)       -> Writer.write_u8 w 0x46; Writer.write_u30 w prop; Writer.write_u30 w argc
    | OpReturnVoid                      -> Writer.write_u8 w 0x47
    | OpReturnValue                     -> Writer.write_u8 w 0x48
    | OpConstructSuper (argc)           -> Writer.write_u8 w 0x49; Writer.write_u30 w argc
    | OpConstructProperty (prop, argc)  -> Writer.write_u8 w 0x4A; Writer.write_u30 w prop; Writer.write_u30 w argc
    | OpCallPropertyLex (prop, argc)    -> Writer.write_u8 w 0x4C; Writer.write_u30 w prop; Writer.write_u30 w argc
    | OpCallSuperVoid (prop, argc)      -> Writer.write_u8 w 0x4E; Writer.write_u30 w prop; Writer.write_u30 w argc
    | OpCallPropertyVoid (prop, argc)   -> Writer.write_u8 w 0x4F; Writer.write_u30 w prop; Writer.write_u30 w argc
    | OpAlchemyExtend1            -> Writer.write_u8 w 0x50
    | OpAlchemyExtend8            -> Writer.write_u8 w 0x51
    | OpAlchemyExtend16           -> Writer.write_u8 w 0x52
    | OpApplyType (argc)          -> Writer.write_u8 w 0x53; Writer.write_u30 w argc
    | OpNewObject (argc)          -> Writer.write_u8 w 0x55; Writer.write_u30 w argc
    | OpNewArray (argc)           -> Writer.write_u8 w 0x56; Writer.write_u30 w argc
    | OpNewActivation             -> Writer.write_u8 w 0x57
    | OpNewClass (cls)            -> Writer.write_u8 w 0x58; Writer.write_u30 w cls
    | OpGetDescendants (prop)     -> Writer.write_u8 w 0x59; Writer.write_u30 w prop
    | OpNewCatch (catch)          -> Writer.write_u8 w 0x5A; Writer.write_u30 w catch
    | OpFindPropertyStrict (prop) -> Writer.write_u8 w 0x5D; Writer.write_u30 w prop
    | OpFindProperty (prop)       -> Writer.write_u8 w 0x5E; Writer.write_u30 w prop
    | OpFindDef (prop)            -> Writer.write_u8 w 0x5F; Writer.write_u30 w prop
    | OpGetLex (prop)             -> Writer.write_u8 w 0x60; Writer.write_u30 w prop
    | OpSetProperty (prop)        -> Writer.write_u8 w 0x61; Writer.write_u30 w prop
    | OpGetLocal (local)          -> Writer.write_u8 w 0x62; Writer.write_u30 w local
    | OpSetLocal (local)          -> Writer.write_u8 w 0x63; Writer.write_u30 w local
    | OpGetGlobalScope            -> Writer.write_u8 w 0x64
    | OpGetScopeObject (index)    -> Writer.write_u8 w 0x65; Writer.write_u30 w index
    | OpGetProperty (prop)        -> Writer.write_u8 w 0x66; Writer.write_u30 w prop
    | OpInitProperty (prop)       -> Writer.write_u8 w 0x68; Writer.write_u30 w prop
    | OpDeleteProperty (prop)     -> Writer.write_u8 w 0x6A; Writer.write_u30 w prop
    | OpGetSlot (slot_id)         -> Writer.write_u8 w 0x6C; Writer.write_u30 w slot_id
    | OpSetSlot (slot_id)         -> Writer.write_u8 w 0x6D; Writer.write_u30 w slot_id
    | OpGetGlobalSlot (slot_id)   -> Writer.write_u8 w 0x6E; Writer.write_u30 w slot_id
    | OpSetGlobalSlot (slot_id)   -> Writer.write_u8 w 0x6F; Writer.write_u30 w slot_id
    | OpConvertS          -> Writer.write_u8 w 0x70
    | OpEscXelem          -> Writer.write_u8 w 0x71
    | OpEscXattr          -> Writer.write_u8 w 0x72
    | OpConvertI          -> Writer.write_u8 w 0x73
    | OpConvertU          -> Writer.write_u8 w 0x74
    | OpConvertD          -> Writer.write_u8 w 0x75
    | OpConvertB          -> Writer.write_u8 w 0x76
    | OpConvertO          -> Writer.write_u8 w 0x77
    | OpCheckFilter       -> Writer.write_u8 w 0x78
    | OpCoerce (name)     -> Writer.write_u8 w 0x80; Writer.write_u30 w name
    | OpCoerceB           -> Writer.write_u8 w 0x81
    | OpCoerceA           -> Writer.write_u8 w 0x82
    | OpCoerceI           -> Writer.write_u8 w 0x83
    | OpCoerceD           -> Writer.write_u8 w 0x84
    | OpCoerceS           -> Writer.write_u8 w 0x85
    | OpAsType (name)     -> Writer.write_u8 w 0x86; Writer.write_u30 w name
    | OpAsTypeLate        -> Writer.write_u8 w 0x87
    | OpCoerceO           -> Writer.write_u8 w 0x89
    | OpNegate            -> Writer.write_u8 w 0x90
    | OpIncrement         -> Writer.write_u8 w 0x91
    | OpIncLocal (local)  -> Writer.write_u8 w 0x92; Writer.write_u30 w local
    | OpDecrement         -> Writer.write_u8 w 0x93
    | OpDecLocal (local)  -> Writer.write_u8 w 0x94; Writer.write_u30 w local
    | OpTypeOf            -> Writer.write_u8 w 0x95
    | OpNot               -> Writer.write_u8 w 0x96
    | OpBitNot            -> Writer.write_u8 w 0x97
    | OpAdd               -> Writer.write_u8 w 0xA0
    | OpSubtract          -> Writer.write_u8 w 0xA1
    | OpMultiply          -> Writer.write_u8 w 0xA2
    | OpDivide            -> Writer.write_u8 w 0xA3
    | OpModulo            -> Writer.write_u8 w 0xA4
    | OpLshift            -> Writer.write_u8 w 0xA5
    | OpRshift            -> Writer.write_u8 w 0xA6
    | OpUrshift           -> Writer.write_u8 w 0xA7
    | OpBitAnd            -> Writer.write_u8 w 0xA8
    | OpBitOr             -> Writer.write_u8 w 0xA9
    | OpBitXor            -> Writer.write_u8 w 0xAA
    | OpEquals            -> Writer.write_u8 w 0xAB
    | OpStrictEquals      -> Writer.write_u8 w 0xAC
    | OpLessThan          -> Writer.write_u8 w 0xAD
    | OpLessEquals        -> Writer.write_u8 w 0xAE
    | OpGreaterThan       -> Writer.write_u8 w 0xAF
    | OpGreaterEquals     -> Writer.write_u8 w 0xB0
    | OpInstanceOf        -> Writer.write_u8 w 0xB1
    | OpIsType (ty)       -> Writer.write_u8 w 0xB2; Writer.write_u30 w ty
    | OpIsTypeLate        -> Writer.write_u8 w 0xB3
    | OpIn                -> Writer.write_u8 w 0xB4
    | OpIncrementI        -> Writer.write_u8 w 0xC0
    | OpDecrementI        -> Writer.write_u8 w 0xC1
    | OpIncLocalI (local) -> Writer.write_u8 w 0xC2; Writer.write_u30 w local
    | OpDecLocalI (local) -> Writer.write_u8 w 0xC3; Writer.write_u30 w local
    | OpNegateI           -> Writer.write_u8 w 0xC4
    | OpAddI              -> Writer.write_u8 w 0xC5
    | OpSubtractI         -> Writer.write_u8 w 0xC6
    | OpMultiplyI         -> Writer.write_u8 w 0xC7
    | OpGetLocal0         -> Writer.write_u8 w 0xD0
    | OpGetLocal1         -> Writer.write_u8 w 0xD1
    | OpGetLocal2         -> Writer.write_u8 w 0xD2
    | OpGetLocal3         -> Writer.write_u8 w 0xD3
    | OpSetLocal0         -> Writer.write_u8 w 0xD4
    | OpSetLocal1         -> Writer.write_u8 w 0xD5
    | OpSetLocal2         -> Writer.write_u8 w 0xD6
    | OpSetLocal3         -> Writer.write_u8 w 0xD7
    | OpDebug (ty, idx, reg, extra) ->
                             Writer.write_u8 w 0xEF; Writer.write_u8 w ty;  Writer.write_u30 w idx;
                                                     Writer.write_u8 w reg; Writer.write_u30 w extra
    | OpDebugLine (line)  -> Writer.write_u8 w 0xF0; Writer.write_u30 w line
    | OpDebugFile (file)  -> Writer.write_u8 w 0xF1; Writer.write_u30 w file
    | OpBkptLine          -> Writer.write_u8 w 0xF2
    | OpTimestamp         -> Writer.write_u8 w 0xF3
  in
  (* Build an index *)
  let index = Array.create (Array.length opcodes) (-1) in
  opcodes |> Array.iteri (fun num opcode ->
    index.(num) <- Writer.offset w;
    write_opcode index opcode);
  Writer.clear w;
  (* Write opcodes *)
  Array.iter (write_opcode index) opcodes;
  index, Writer.to_string w

let dump_file file =
  let w = Writer.create () in
  let write_array f arr = write_array w f arr in
  (* abcFile {
      u16 minor_version
      u16 major_version
      cpool_info constant_pool
      u30 method_count
      method_info method[method_count]
      u30 metadata_count
      metadata_info metadata[metadata_count]
      u30 class_count
      instance_info instance[class_count]
      class_info class[class_count]
      u30 script_count
      script_info script[script_count]
      u30 method_body_count
      method_body_info method_body[method_body_count]
    } *)
  let rec write_file file =
    Writer.write_u16 w file.file_minor_version;
    Writer.write_u16 w file.file_major_version;
    write_constant_pool file.file_const_pool;
    write_array write_method file.file_methods;
    write_array write_metadata file.file_metadata;
    assert ((Array.length file.file_instances) = (Array.length file.file_classes));
    Writer.write_u30 w (Array.length file.file_instances);
    Array.iter write_instance file.file_instances;
    Array.iter write_class file.file_classes;
    write_array write_script file.file_scripts;
    write_array write_method_body file.file_method_bodies
  (* cpool_info {
      u30 int_count
      s32 integer[int_count]
      u30 uint_count
      u32 uinteger[uint_count]
      u30 double_count
      d64 double[double_count]
      u30 string_count
      string_info string[string_count]
      u30 namespace_count
      namespace_info namespace[namespace_count]
      u30 ns_set_count
      ns_set_info ns_set[ns_set_count]
      u30 multiname_count
      multiname_info multiname[multiname_count]
    } *)
  and write_constant_pool pool =
    let write_array f arr =
      (* Const pool has zero element omitted from file *)
      if (Array.length arr) = 0 then
        Writer.write_u30 w 0
      else
        Writer.write_u30 w ((Array.length arr) + 1);
      Array.iter f arr
    in
    write_array (Writer.write_s32 w) pool.const_pool_integers;
    write_array (Writer.write_u32 w) pool.const_pool_uintegers;
    write_array (Writer.write_d64 w) pool.const_pool_doubles;
    write_array write_string pool.const_pool_strings;
    write_array write_namespace pool.const_pool_namespaces;
    write_array write_ns_set pool.const_pool_ns_sets;
    write_array write_multiname pool.const_pool_multinames
  (* string_info {
      u30 size
      u8 utf8[size]
    } *)
  and write_string str =
    Writer.write_u30 w (String.length str);
    Writer.write_bytes w str
  (* namespace_info {
      u8 kind
      u30 name
    } *)
  and write_namespace ns =
    let kind =
      match ns.namespace_kind with
      | NSKindNamespace           -> 0x08
      | NSKindPackageNamespace    -> 0x16
      | NSKindPackageInternalNS   -> 0x17
      | NSKindProtectedNamespace  -> 0x18
      | NSKindExplicitNamespace   -> 0x19
      | NSKindStaticProtectedNS   -> 0x1A
      | NSKindPrivateNS           -> 0x05
    in
    Writer.write_u8 w kind;
    Writer.write_u30 w ns.namespace_name
  (* ns_set_info {
      u30 count
      u30 ns[count]
    } *)
  and write_ns_set ns_set =
    write_array (Writer.write_u30 w) ns_set
  (* multiname_info {
      u8 kind
      u8 data[]
    } *)
  and write_multiname multiname =
    match multiname with
    | QName (data)        -> Writer.write_u8 w 0x07; write_multiname_qname data
    | QNameA (data)       -> Writer.write_u8 w 0x0D; write_multiname_qname data
    | RTQName (data)      -> Writer.write_u8 w 0x0F; write_multiname_rtqname data
    | RTQNameA (data)     -> Writer.write_u8 w 0x10; write_multiname_rtqname data
    | RTQNameL            -> Writer.write_u8 w 0x11
    | RTQNameLA           -> Writer.write_u8 w 0x12
    | Multiname (data)    -> Writer.write_u8 w 0x09; write_multiname_multiname data
    | MultinameA (data)   -> Writer.write_u8 w 0x0E; write_multiname_multiname data
    | MultinameL (data)   -> Writer.write_u8 w 0x1B; write_multiname_multinamel data
    | MultinameLA (data)  -> Writer.write_u8 w 0x1C; write_multiname_multinamel data
    (* Undocumented in avm2overview. *)
    | GenericName (data)  -> Writer.write_u8 w 0x1D; write_multiname_genericname data
  (* multiname_kind_QName {
      u30 ns
      u30 name
    } *)
  and write_multiname_qname qname =
    Writer.write_u30 w qname.qname_ns;
    Writer.write_u30 w qname.qname_name
  (* multiname_kind_RTQName {
      u30 name
    } *)
  and write_multiname_rtqname rtqname =
    Writer.write_u30 w rtqname.rtqname_name
  (* multiname_kind_RTQNameL {
    } *)
  (* multiname_kind_Multiname {
      u30 name
      u30 ns_set
    } *)
  and write_multiname_multiname multiname =
    Writer.write_u30 w multiname.multiname_name;
    Writer.write_u30 w multiname.multiname_ns_set
  (* multiname_kind_MultinameL {
      u30 ns_set
    } *)
  and write_multiname_multinamel multinamel =
    Writer.write_u30 w multinamel.multinamel_ns_set
  (* Undocumented in avm2overview. *)
  and write_multiname_genericname genericname =
    Writer.write_u30 w genericname.genericname_name;
    write_array (Writer.write_u30 w) genericname.genericname_params
  (* Common code for option_detail and trait_slot. *)
  and unmake_constant const =
    match const with
    | ConstInt (idx)                 -> 0x03, idx
    | ConstUInt (idx)                -> 0x04, idx
    | ConstDouble (idx)              -> 0x06, idx
    | ConstUtf8 (idx)                -> 0x01, idx
    | ConstTrue                      -> 0x0B, 0x0B
    | ConstFalse                     -> 0x0A, 0x0A
    | ConstNull                      -> 0x0C, 0x0C
    | ConstUndefined                 -> 0x00, 0x00
    | ConstNamespace (idx)           -> 0x08, idx
    | ConstPackageNamespace (idx)    -> 0x16, idx
    | ConstPackageInternalNS (idx)   -> 0x17, idx
    | ConstProtectedNamespace (idx)  -> 0x18, idx
    | ConstExplicitNamespace (idx)   -> 0x19, idx
    | ConstStaticProtectedNS (idx)   -> 0x1A, idx
    | ConstPrivateNS (idx)           -> 0x05, idx
  (* method_info {
      u30 param_count
      u30 return_type
      u30 param_type[param_count]
      u30 name
      u8 flags
      option_info options
      param_info param_names
    } *)
  and write_method meth =
    Writer.write_u30 w (Array.length meth.method_param_types);
    Writer.write_u30 w meth.method_return_type;
    Array.iter (Writer.write_u30 w) meth.method_param_types;
    Writer.write_u30 w meth.method_name;
    (* NEED_ARGUMENTS  0x01
       NEED_ACTIVATION 0x02
       NEED_REST       0x04
       HAS_OPTIONAL    0x08
       SET_DXNS        0x40
       HAS_PARAM_NAMES 0x80 *)
    let flags =
      (if meth.method_need_arguments      then 0x01 else 0) lor
      (if meth.method_need_activation     then 0x02 else 0) lor
      (if meth.method_need_rest           then 0x04 else 0) lor
      (if meth.method_defaults <> None    then 0x08 else 0) lor
      (if meth.method_set_dxns            then 0x40 else 0) lor
      (if meth.method_param_names <> None then 0x80 else 0)
    in
    Writer.write_u8 w flags;
    (* option_info {
        u30 option_count
        option_detail option[option_count]
      } *)
    Option.may (write_array write_default) meth.method_defaults;
    Option.may (fun param_names ->
        assert ((Array.length param_names) = (Array.length meth.method_param_types));
        Array.iter write_string param_names)
      meth.method_param_names
  (* option_detail {
      u30 val
      u8 kind
    } *)
  and write_default const =
    let kind, index = unmake_constant const in
    Writer.write_u30 w index;
    Writer.write_u8  w kind
  (* metadata_info {
      u30 name
      u30 item_count
      item_info items[item_count]
    } *)
  and write_metadata md =
    Writer.write_u30 w md.metadata_name;
    write_array write_metadata_item md.metadata_items
  (* item_info {
      u30 key
      u30 value
    } *)
  and write_metadata_item mdi =
    Writer.write_u30 w mdi.metadata_key;
    Writer.write_u30 w mdi.metadata_value
  (* traits_info {
      u30 name
      u8 kind
      u8 data[]
      u30 metadata_count
      u30 metadata[metadata_count]
    } *)
  and write_trait trait =
    Writer.write_u30 w trait.trait_name;
    (* ATTR_Final    0x1
       ATTR_Override 0x2
       ATTR_Metadata 0x4 *)
    let flags =
      (if trait.trait_final            then 0x10 else 0) lor
      (if trait.trait_override         then 0x20 else 0) lor
      (if trait.trait_metadata <> None then 0x40 else 0)
    in
    (* trait_class {
        u30 slot_id
        u30 classi
      } *)
    (* trait_function {
        u30 slot_id
        u30 function
      } *)
    (* trait_method {
        u30 disp_id
        u30 method
      } *)
    let kind, data_writer =
      match trait.trait_data with
      | TraitSlot slot    -> 0, fun () -> write_trait_slot slot
      | TraitMethod idx   -> 1, fun () -> Writer.write_u30 w idx
      | TraitGetter idx   -> 2, fun () -> Writer.write_u30 w idx
      | TraitSetter idx   -> 3, fun () -> Writer.write_u30 w idx
      | TraitClass idx    -> 4, fun () -> Writer.write_u30 w idx
      | TraitFunction idx -> 5, fun () -> Writer.write_u30 w idx
      | TraitConst slot   -> 6, fun () -> write_trait_slot slot
    in
    Writer.write_u8 w (flags lor kind);
    Writer.write_u30 w trait.trait_slot_id;
    data_writer ();
    Option.may (write_array (Writer.write_u30 w)) trait.trait_metadata
  (* trait_slot {
      u30 slot_id
      u30 type_name
      u30 vindex
      u8  vkind
    } *)
  and write_trait_slot slot =
    Writer.write_u30 w slot.trait_slot_type;
    match slot.trait_slot_value with
    | None -> Writer.write_u30 w 0
    | Some const ->
      let kind, index = unmake_constant const in
      Writer.write_u30 w index;
      Writer.write_u8  w kind
  (* instance_info {
      u30 name
      u30 super_name
      u8 flags
      u30 protectedNs
      u30 intrf_count
      u30 interface[intrf_count]
      u30 iinit
      u30 trait_count
      traits_info trait[trait_count]
    } *)
  and write_instance inst =
    Writer.write_u30 w inst.instance_name;
    Writer.write_u30 w inst.instance_super_name;
    (* ClassSealed       0x01
       ClassFinal        0x02
       ClassInterface    0x04
       ClassProtectedNs  0x08 *)
    let flags =
      (if inst.instance_sealed               then 0x01 else 0) lor
      (if inst.instance_final                then 0x02 else 0) lor
      (if inst.instance_interface            then 0x04 else 0) lor
      (if inst.instance_protected_ns <> None then 0x08 else 0)
    in
    Writer.write_u8 w flags;
    Option.may (Writer.write_u30 w) inst.instance_protected_ns;
    write_array (Writer.write_u30 w) inst.instance_interfaces;
    Writer.write_u30 w inst.instance_initializer;
    write_array write_trait inst.instance_traits
  (* class_info {
      u30 cinit
      u30 trait_count
      traits_info traits[trait_count]
    } *)
  and write_class klass =
    Writer.write_u30 w klass.class_initializer;
    write_array write_trait klass.class_traits
  (* script_info {
      u30 init
      u30 trait_count
      traits_info trait[trait_count]
    } *)
  and write_script script =
    Writer.write_u30 w script.script_initializer;
    write_array write_trait script.script_traits
  (* method_body_info {
      u30 method
      u30 max_stack
      u30 local_count
      u30 init_scope_depth
      u30 max_scope_depth
      u30 code_length
      u8 code[code_length]
      u30 exception_count
      exception_info exception[exception_count]
      u30 trait_count
      traits_info trait[trait_count]
    } *)
  and write_method_body body =
    Writer.write_u30 w body.body_method;
    Writer.write_u30 w body.body_max_stack;
    Writer.write_u30 w body.body_local_count;
    Writer.write_u30 w body.body_init_scope_depth;
    Writer.write_u30 w body.body_max_scope_depth;
    let index, code = dump_code body.body_code in
    write_string code;
    write_array (write_exception index) body.body_exceptions;
    write_array write_trait body.body_traits
  (* exception_info {
    u30 from
    u30 to
    u30 target
    u30 exc_type
    u30 var_name
    } *)
  and write_exception index exc =
    Writer.write_u30 w index.(exc.exception_from);
    Writer.write_u30 w index.(exc.exception_to);
    Writer.write_u30 w index.(exc.exception_target);
    Writer.write_u30 w exc.exception_type;
    Writer.write_u30 w exc.exception_var_name
  in
  write_file file;
  Writer.to_string w

let dump_code opcodes =
  assert false
