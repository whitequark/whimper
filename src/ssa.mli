type value = private {
  mutable opcode       : opcode;
  mutable uses         : value list;
  mutable parent       : parent;
          hash         : int;
}
and func = private {
          abc_file     : Abc.file;
          name         : string;
          return_type  : Abc.multiname_ref;
  mutable arguments    : value list;
  mutable basic_blocks : value list;
}
and basic_block = private {
          func         : func;
  mutable instructions : value list;
}
and parent
and opcode =
(* Non-instructions *)
| Invalid
| Argument        of (* name *) string * (* type *) Abc.multiname_ref
| BasicBlock      of basic_block
| Const           of constant
(* Phi *)
| Phi             of ((* basic block *) value * (* value *) value) list
(* Terminators *)
| Jump            of value
| JumpIf          of value * (* if true *) value * (* if false *) value
| Switch          of value * (* default *) value * (* targets *) value list
| Return          of value
| Throw           of value
(* Local variables *)
| GetLocal        of (* index *) int
| SetLocal        of (* index *) int * value
| HasNext2        of (* obj *) int * (* reg *) int
(* Scopes *)
| GetScopeObject  of (* u8 index *) int
| PushScope       of value
| PushWith        of value
| PopScope
(* Constructors *)
| NewObject       of ((* key *) value * (* value *) value) list
| NewArray        of value list
| NewFunction     of Abc.method_ref
| NewClass        of Abc.class_ref
| NewActivation
| NewCatch        of (* catch_id *) int
(* Functions & Objects *)
| Call            of (* callable *) value      * (* args *) value list
| Construct       of (* constructable *) value * (* args *) value list
| Super           of value
| FindProperty    of property
| FindPropStrict  of property
| GetProperty     of (* obj *) value * property
| SetProperty     of (* obj *) value * property * (* val *) value
| InitProperty    of (* obj *) value * property * (* val *) value
| DeleteProperty  of (* obj *) value * property
(* for..in *)
| NextName        of (* obj *) value * (* idx *) value
| NextValue       of (* obj *) value * (* idx *) value
| HasNext         of (* obj *) value * (* idx *) value
(* Arithmetics *)
| UnaryOp         of unary_op  * value
| BinaryOp        of binary_op * value * value
and constant =
| ConstUndefined
| ConstBool       of bool
| ConstInt        of Int32.t
| ConstUint       of Uint32.t
| ConstNumber     of float
| ConstString     of string
| ConstNamespace  of Abc.namespace_ref
| ConstMethod     of Abc.method_ref
and property =
| Slot            of int
| QName           of (* namespace *) value * (* name *) value
| Multiname       of Abc.ns_set_ref * (* name *) value
and unary_op =
(* ToBoolean *)
| UnOpNot
(* ToNumber *)
| UnOpNegate
| UnOpIncrement
| UnOpDecrement
(* ToInt32 *)
| UnOpBitNot
| UnOpNegateInt
| UnOpIncrementInt
| UnOpDecrementInt
| UnOpSignExtend1
| UnOpSignExtend8
| UnOpSignExtend16
(* Type conversion *)
| UnOpCheckFilter
| UnOpConvertS
| UnOpConvertI
| UnOpConvertU
| UnOpConvertD
| UnOpConvertB
| UnOpConvertO
| UnOpCoerceB
| UnOpCoerceA
| UnOpCoerceI
| UnOpCoerceD
| UnOpCoerceS
| UnOpCoerceO
and binary_op =
(* ToNumber *)
| BinOpAdd
| BinOpSubtract
| BinOpMultiply
| BinOpDivide
| BinOpModulo
(* ToInt32 *)
| BinOpAddInt
| BinOpSubtractInt
| BinOpMultiplyInt
| BinOpLshift
| BinOpRshift
| BinOpUrshift
| BinOpBitAnd
| BinOpBitOr
| BinOpBitXor
(* Boolean *)
| BinOpEquals
| BinOpStrictEquals
| BinOpLessThan
| BinOpLessEquals
| BinOpGreaterThan
| BinOpGreaterEquals
| BinOpIn
| BinOpIsType
| BinOpInstanceOf
(* Type conversion *)
| BinOpAsType
| BinOpCoerce

module Valuetbl : Hashtbl.S with type key = value

val string_of_const : Abc.file -> constant -> string

val create_func     : name:string -> ?arg_names:string list ->
                      arg_types:Abc.multiname_ref list ->
                      return_type:Abc.multiname_ref -> Abc.file -> func
val entry           : func -> (* basic_block *) value
val string_of_func  : func -> string

val iter_blocks     : f:( (* basic_block *) value -> unit) -> func -> unit
val add_block       : func -> (* basic_block *) value
val remove_block    : (* basic_block *) value -> unit
val string_of_block : (* basic_block *) value -> string

val successors      : (* basic_block *) value -> value list
val predecessors    : (* basic_block *) value -> value list
val terminator      : (* basic_block *) value -> (* instruction *) value

val iter_insns      : f:( (* instruction *) value -> unit) ->
                      (* basic_block *) value -> unit
val append_insn     : ?before:(* instruction *) value -> opcode ->
                      (* basic_block *) value -> (* instruction *) value
val remove_insn     : (* instruction *) value -> unit
val replace_insn    : (* instruction *) value -> opcode -> unit
val string_of_insn  : (* instruction *) value -> string

val iter_uses             : f:( (* instruction *) value -> unit) -> value -> unit
val replace_all_uses_with : value -> value -> unit
