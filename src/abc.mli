open Sexplib.Std

type uint32 = Uint32.t
val sexp_of_uint32 : uint32 -> Sexplib.Sexp.t
val uint32_of_sexp : Sexplib.Sexp.t -> uint32

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
and method_ref      = private int
and metadata_ref    = private int
and class_ref       = private int
and script_ref      = private int
and method_body_ref = private int
and constant_pool = {
  const_pool_integers   : int32 array;
  const_pool_uintegers  : uint32 array;
  const_pool_doubles    : float array;
  const_pool_strings    : string array;
  const_pool_namespaces : namespace array;
  const_pool_ns_sets    : ns_set array;
  const_pool_multinames : multiname array;
}
and integer_ref   = private int
and uinteger_ref  = private int
and double_ref    = private int
and string_ref    = private int
and namespace_ref = private int
and ns_set_ref    = private int
and multiname_ref = private int
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
(* Those constants ending in "A" (such as CONSTANT_QNameA) represent
   the names of attributes. *)
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
| (* 0x45; argc + 1 -> 1 *) OpCallSuper         of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x46; argc + 1 -> 1 *) OpCallProperty      of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x47; 0 -> 0 *) OpReturnVoid
| (* 0x48; 1 -> 0 *) OpReturnValue
| (* 0x49; argc + 1 -> 0 *) OpConstructSuper    of (* u30 argc *) int
| (* 0x4A; argc + 1 -> 1 *) OpConstructProperty of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x4C; argc + 1 -> 1 *) OpCallPropertyLex   of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x4E; argc + 1 -> 0 *) OpCallSuperVoid     of (* u30 prop *) multiname_ref * (* u30 argc *) int
| (* 0x4F; argc + 1 -> 0 *) OpCallPropertyVoid  of (* u30 prop *) multiname_ref * (* u30 argc *) int
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
| (* 0x6A; 1 -> 1 *) OpDeleteProperty     of (* u30 prop *) multiname_ref
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

val load_file   : string -> file
val dump_file   : file -> string

val integer     : file -> integer_ref     -> Int32.t
val uinteger    : file -> uinteger_ref    -> Uint32.t
val double      : file -> double_ref      -> float
val string      : file -> string_ref      -> string
val namespace   : file -> namespace_ref   -> namespace
val ns_set      : file -> ns_set_ref      -> ns_set
val multiname   : file -> multiname_ref   -> multiname
val method_     : file -> method_ref      -> method_
val metadata    : file -> metadata_ref    -> metadata
val instance    : file -> class_ref       -> instance
val class_      : file -> class_ref       -> class_
val script      : file -> script_ref      -> script
val method_body : file -> method_body_ref -> method_body

val string_of_namespace : file -> namespace_ref -> string
val string_of_ns_set    : file -> ns_set_ref -> string
val string_of_multiname : file -> multiname_ref -> string
val string_of_method    : file -> method_ref -> string
val string_of_class     : file -> class_ref -> string
val string_of_opcode    : file -> opcode -> string

val disassemble : file -> opcodes -> string
