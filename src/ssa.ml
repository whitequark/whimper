open ExtList

type value = {
  mutable opcode       : opcode;
  mutable uses         : value list;
  mutable parent       : parent;
          hash         : int;
}
and func = {
          abc_file     : Abc.file;
          name         : string;
          return_type  : Abc.multiname_ref;
  mutable arguments    : value list;
  mutable basic_blocks : value list;
}
and basic_block = {
          func         : func;
  mutable instructions : value list;
}
and parent =
| ParentFunction   of func
| ParentBasicBlock of value
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


module ValueIdentity =
struct
  type t = value

  let equal = (==)
  let hash value = value.hash
end

module Valuetbl = Hashtbl.Make(ValueIdentity)

let create_func ~name ?arg_names ~arg_types ~return_type abc_file =
  let arg_names =
    match arg_names with
    | Some names -> names
    | None       -> List.mapi (fun idx _ -> Printf.sprintf "param%d" idx) arg_types
  in
  let func = {
    abc_file;
    name;
    return_type;
    arguments    = [];
    basic_blocks = []; } in
  let arguments =
    List.map2 (fun name ty ->
        { opcode = Argument (name, ty);
          uses   = [];
          parent = ParentFunction func;
          hash   = Hash_seed.make (); })
      arg_names arg_types
  in
  func.arguments <- arguments;
  func

let entry func =
  if func.basic_blocks = [] then
    failwith "Ssa.entry: empty function";
  List.hd func.basic_blocks

let block_of_value value =
  match value with
  | { opcode = BasicBlock block } -> block
  | _ -> failwith "Ssa: basic block expected"

let iter_blocks ~f func =
  List.iter f func.basic_blocks

let add_block func =
  let blockv = {
    opcode = BasicBlock { func; instructions = [] };
    uses   = [];
    parent = ParentFunction func;
    hash   = Hash_seed.make (); } in
  func.basic_blocks <- func.basic_blocks @ [blockv];
  blockv

let remove_block blockv =
  let block = block_of_value blockv in
  let func  =
    match blockv with
    | { parent = ParentFunction func } -> func
    | _ -> assert false
  in
  if not (List.memq blockv func.basic_blocks) then
    failwith "Ssa.remove_block: basic block not present in function";
  if block.instructions <> [] then
    failwith "Ssa.remove_block: basic block not empty";
  func.basic_blocks <- List.remove_if ((==) blockv) func.basic_blocks

let terminator blockv =
  let { instructions } = block_of_value blockv in
  if instructions = [] then
    failwith "Ssa.terminator: empty basic block";
  let terminator = List.nth instructions ((List.length instructions) - 1) in
  match terminator.opcode with
  | (Jump _ | JumpIf _ | Switch _ | Return _ | Throw _) -> terminator
  | _ -> failwith "Ssa.terminator: last instruction not a terminator"

let successors blockv =
  match (terminator blockv).opcode with
  | Jump target                   -> [target]
  | JumpIf (_, if_true, if_false) -> [if_true; if_false]
  | Switch (_, default, targets)  -> default :: targets
  | Return _ | Throw _            -> []
  | _                             -> assert false

let predecessors blockv =
  blockv.uses |>
  List.map (fun use ->
    match use.parent with
    | ParentBasicBlock block -> block
    | _ -> assert false) |>
  List.unique ~cmp:(==)

let operands { opcode } =
  let property_operands prop =
    match prop with
    | Slot _ -> []
    | QName (ns, name) -> [ns; name]
    | Multiname (ns_set, name) -> [name]
  in
  let unzip elements =
    List.fold_left (fun lst (blockv, value) ->
        blockv :: value :: lst)
      [] elements
  in
  match opcode with
  | Invalid | Argument _ | BasicBlock _ | Const _ -> assert false
  (* Phi *)
  | Phi operands -> unzip operands
  (* Terminators *)
  | Jump value | Return value | Throw value -> [value]
  | JumpIf (cond, if_true, if_false) -> [cond; if_true; if_false]
  | Switch (cond, default, targets)  -> [cond; default] @ targets
  (* Local variables *)
  | GetLocal _ | HasNext2 _ -> []
  | SetLocal (_, value) -> [value]
  (* Scopes *)
  | GetScopeObject _ | PopScope -> []
  | PushScope scope | PushWith scope -> [scope]
  (* Constructors *)
  | NewFunction _ | NewClass _ | NewActivation | NewCatch _ -> []
  | NewArray  elems -> elems
  | NewObject pairs -> unzip pairs
  (* Functions & Objects *)
  | Call (recv, args) | Construct (recv, args) -> recv :: args
  | Super value -> [value]
  | FindProperty prop | FindPropStrict prop ->
    property_operands prop
  | GetProperty (obj, prop) | DeleteProperty (obj, prop) ->
    obj :: property_operands prop
  | SetProperty (obj, prop, value) | InitProperty (obj, prop, value) ->
    obj :: value :: property_operands prop
  (* for..in *)
  | NextName (obj, idx) | NextValue (obj, idx) | HasNext (obj, idx) -> [obj; idx]
  (* Arithmetics *)
  | UnaryOp (op, value) -> [value]
  | BinaryOp (op, lhs, rhs) -> [lhs; rhs]

let operands { opcode } =
  let property_operands prop =
    match prop with
    | Slot _ -> []
    | QName (ns, name) -> [ns; name]
    | Multiname (ns_set, name) -> [name]
  in
  match opcode with
  | Invalid | Argument _ | BasicBlock _ | Const _ -> assert false
  (* Phi *)
  | Phi operands ->
    List.fold_left (fun lst (blockv, value) -> blockv :: value :: lst) [] operands
  (* Terminators *)
  | Jump value | Return value | Throw value -> [value]
  | JumpIf (cond, if_true, if_false) -> [cond; if_true; if_false]
  | Switch (cond, default, targets)  -> [cond; default] @ targets
  (* Local variables *)
  | GetLocal _ | HasNext2 _ -> []
  | SetLocal (_, value) -> [value]
  (* Scopes *)
  | GetScopeObject _ | PopScope -> []
  | PushScope scope | PushWith scope -> [scope]
  (* Constructors *)
  | NewFunction _ | NewClass _ | NewActivation | NewCatch _ -> []
  | NewArray  elems -> elems
  | NewObject pairs ->
    List.fold_left (fun lst (key, value) -> key :: value :: lst) [] pairs
  (* Functions & Objects *)
  | Call (recv, args) | Construct (recv, args) -> recv :: args
  | Super value -> [value]
  | FindProperty prop | FindPropStrict prop ->
    property_operands prop
  | GetProperty (obj, prop) | DeleteProperty (obj, prop) ->
    obj :: property_operands prop
  | SetProperty (obj, prop, value) | InitProperty (obj, prop, value) ->
    obj :: value :: property_operands prop
  (* for..in *)
  | NextName (obj, idx) | NextValue (obj, idx) | HasNext (obj, idx) -> [obj; idx]
  (* Arithmetics *)
  | UnaryOp (op, value) -> [value]
  | BinaryOp (op, lhs, rhs) -> [lhs; rhs]

let set_operands insnv operands =
  let set_property_operands prop operands =
    match prop, operands with
    | Slot _, [] -> prop
    | QName _, [ns; name] -> QName (ns, name)
    | Multiname (ns_set, _), [name] -> Multiname (ns_set, name)
    | _ -> assert false
  in
  let zip elements =
    List.fold_left (fun (lst, acc) elem ->
        match acc with
        | Some prev -> (prev, elem) :: lst, None
        | None      ->                 lst, Some elem)
      ([], None) elements |> fst
  in
  let opcode' =
    match insnv.opcode, operands with
    (* Phi *)
    | Phi _, operands -> Phi (zip operands)
    (* Terminators *)
    | Jump _, [value] -> Jump value
    | Return _, [value] -> Return value
    | Throw _, [value] -> Throw value
    | JumpIf _, [cond; if_true; if_false] -> JumpIf (cond, if_true, if_false)
    | Switch _, cond :: default :: targets  -> Switch (cond, default, targets)
    (* Local variables *)
    | GetLocal _, _ | HasNext2 _, _ -> insnv.opcode
    | SetLocal (local, _), [value] -> SetLocal (local, value)
    (* Scopes *)
    | (GetScopeObject _ | PopScope), [] -> insnv.opcode
    | PushScope _, [value] -> PushScope value
    | PushWith _, [value] -> PushWith value
    (* Constructors *)
    | (NewFunction _ | NewClass _ | NewActivation | NewCatch _), [] -> insnv.opcode
    | NewArray _, elems -> NewArray elems
    | NewObject _, pairs -> NewObject (zip pairs)
    (* Functions & Objects *)
    | Call _, recv :: args -> Call (recv, args)
    | Construct _, recv :: args -> Construct (recv, args)
    | Super _, [value] -> Super (value)
    | FindProperty (prop), prop' ->
      FindProperty (set_property_operands prop prop')
    | FindPropStrict (prop), prop' ->
      FindPropStrict (set_property_operands prop prop')
    | GetProperty (_, prop), obj :: prop' ->
      GetProperty (obj, set_property_operands prop prop')
    | DeleteProperty (_, prop), obj :: prop' ->
      DeleteProperty (obj, set_property_operands prop prop')
    | SetProperty (_, prop, _), obj :: value :: prop' ->
      SetProperty (obj, set_property_operands prop prop', value)
    | InitProperty (_, prop, _), obj :: value :: prop' ->
      InitProperty (obj, set_property_operands prop prop', value)
    (* for..in *)
    | NextName  _, [obj; idx] -> NextName  (obj, idx)
    | NextValue _, [obj; idx] -> NextValue (obj, idx)
    | HasNext   _, [obj; idx] -> HasNext   (obj, idx)
    (* Arithmetics *)
    | UnaryOp  (op, _),    [value]    -> UnaryOp  (op, value)
    | BinaryOp (op, _, _), [lhs; rhs] -> BinaryOp (op, lhs, rhs)
    (* Invalid *)
    | _ -> assert false
  in
  insnv.opcode <- opcode'

let replace_operand insnv src dst =
  set_operands insnv (List.map (fun op -> if op == src then dst else op) (operands insnv))

let add_uses insnv =
  List.iter (fun op ->
      if not (List.memq insnv op.uses) then
        op.uses <- insnv :: op.uses)
    (operands insnv)

let remove_uses insnv =
  List.iter (fun op ->
      op.uses <- List.remove_if ((==) insnv) op.uses)
    (operands insnv)

let iter_insns ~f blockv =
  let block = block_of_value blockv in
  List.iter f block.instructions

let append_insn ?before opcode blockv =
  let block = block_of_value blockv in
  let insnv = {
    opcode;
    uses    = [];
    parent  = ParentBasicBlock blockv;
    hash    = Hash_seed.make (); } in
  begin match before with
  | None ->
    block.instructions <- block.instructions @ [insnv]
  | Some beforev ->
    let rec append lst =
      match lst with
      | headv :: rest when headv == beforev ->
        insnv :: headv :: rest
      | []   -> failwith "Ssa.append_insn: ~before insn not found in basic block"
      | rest -> append rest
    in
    block.instructions <- append block.instructions
  end;
  add_uses insnv;
  insnv

let remove_insn insnv =
  match insnv with
  | { parent = ParentBasicBlock { opcode = BasicBlock block } } ->
    if not (List.memq insnv block.instructions) then
      failwith "Ssa.remove_insn: instruction not present in block";
    if insnv.uses <> [] then
      failwith "Ssa.remove_insn: instruction used";
    remove_uses insnv;
    block.instructions <- List.remove_if ((==) insnv) block.instructions
  | _ -> failwith "Ssa.remove_insn: instruction expected"

let replace_insn insnv opcode =
  remove_uses insnv;
  insnv.opcode <- opcode;
  add_uses insnv

let iter_uses ~f value =
  List.iter f value.uses

let replace_all_uses_with src dest =
  iter_uses (fun use -> replace_operand use src dest) src

let string_of_const file const =
  match const with
  | ConstUndefined    -> "undefined"
  | ConstBool value   -> string_of_bool   value
  | ConstInt  value   -> Int32.to_string  value
  | ConstUint value   -> Uint32.to_string value
  | ConstNumber value -> string_of_float  value
  | ConstString value -> Printf.sprintf "\"%s\"" (String.escaped value)
  | ConstNamespace ns -> Abc.string_of_namespace file ns
  | ConstMethod meth  -> Abc.string_of_method file meth

let is_named insnv =
  match insnv.opcode with
  | Jump _ | JumpIf _ | Switch _ | Return _ | Throw _
  | SetLocal _ | PushScope _ | PushWith _ | PopScope
  | SetProperty _ | InitProperty _
  -> false
  | Phi _ | GetLocal _ | HasNext2 _ | GetScopeObject _
  | NewObject _ | NewArray _ | NewFunction _ | NewClass _ | NewActivation | NewCatch _
  | Call _ | Construct _ | Super _ | FindProperty _ | FindPropStrict _
  | GetProperty _ | DeleteProperty _
  | NextName _ | NextValue _ | HasNext _ | UnaryOp _ | BinaryOp _
  -> true
  | Invalid | Argument _ | BasicBlock _ | Const _
  -> assert false

let string_of_use' file name insnv =
  match insnv.opcode with
  | Argument (name, _) -> Printf.sprintf "arg %s" (String.escaped name)
  | BasicBlock blockv  -> Printf.sprintf "label %s" (name insnv)
  | Const const        -> string_of_const file const
  | _                  -> Printf.sprintf "%%%s" (name insnv)

let string_of_insn' file buffer name insnv =
  let write = Buffer.add_string buffer in
  let use = string_of_use' file name in
  let writef fmt = Printf.kprintf write fmt in
  let property prop =
    match prop with
    | Slot idx -> Printf.sprintf "#%d" idx
    | QName (ns, name) -> Printf.sprintf "%s::%s" (use ns) (use name)
    | Multiname (ns_set, name) ->
      Printf.sprintf "%s::%s" (Abc.string_of_ns_set file ns_set) (use name)
  in
  if is_named insnv then
    write (Printf.sprintf "%%%s = " (name insnv));
  match insnv.opcode with
  (* Phi *)
  | Phi operands ->
    operands |> List.map (fun (block, value) ->
      Printf.sprintf "[ %s => %s ]" (use block) (use value)) |>
    String.concat ", " |>
    writef "phi %s"
  (* Terminators *)
  | Jump target -> writef "jump %s" (use target)
  | JumpIf (cond, if_true, if_false) ->
    writef "jumpif %s, %s, %s" (use cond) (use if_true) (use if_false)
  | Switch (cond, default, targets) ->
    writef "switch %s, %s, [%s]" (use cond) (use default)
                                 (List.map use targets |> String.concat ", ")
  | Return value -> writef "return %s" (use value)
  | Throw value -> writef "throw %s"  (use value)
  (* Local variables *)
  | GetLocal idx -> writef "getlocal %d" idx
  | SetLocal (idx, value) -> writef "setlocal %d, %s" idx (use value)
  | HasNext2 (obj, reg) -> writef "hasnext2 %d, %d" obj reg
  (* Scopes *)
  | GetScopeObject idx -> writef "getscopeobject %d" idx
  | PushScope value -> writef "pushscope %s" (use value)
  | PushWith  value -> writef "pushwith %s" (use value)
  | PopScope -> write "popscope"
  (* Constructors *)
  | NewObject pairs ->
    pairs |> List.map (fun (key, value) ->
      Printf.sprintf "%s => %s" (use key) (use value)) |>
    String.concat ", " |>
    writef "newobject {%s}"
  | NewArray elems -> writef "newarray [%s]" (String.concat ", " (List.map use elems))
  | NewFunction meth -> writef "newfunction %s" (Abc.string_of_method file meth)
  | NewClass cls -> writef "newclass %s" (Abc.string_of_class file cls)
  | NewActivation -> write "newactivation"
  | NewCatch idx -> writef "newcatch %d" idx
  (* Functions & Objects *)
  | Call (recv, args) ->
    writef "call %s(%s)" (use recv) (String.concat ", " (List.map use args))
  | Construct (recv, args) ->
    writef "construct %s(%s)" (use recv) (String.concat ", " (List.map use args))
  | FindProperty prop -> writef "findproperty %s" (property prop)
  | FindPropStrict prop -> writef "findpropstrict %s" (property prop)
  | GetProperty (obj, prop) ->
    writef "getproperty %s, %s" (use obj) (property prop)
  | SetProperty (obj, prop, value) ->
    writef "setproperty %s, %s, %s" (use obj) (property prop) (use value)
  | InitProperty (obj, prop, value) ->
    writef "initproperty %s, %s, %s" (use obj) (property prop) (use value)
  | DeleteProperty (obj, prop) ->
    writef "deleteproperty %s, %s" (use obj) (property prop)
  (* for..in *)
  | NextName  (obj, idx) -> writef "nextname %s, %s"  (use obj) (use idx)
  | NextValue (obj, idx) -> writef "nextvalue %s, %s" (use obj) (use idx)
  | HasNext   (obj, idx) -> writef "hasnext %s, %s"   (use obj) (use idx)
  (* Arithmetics *)
  | UnaryOp (op, value) ->
    let op =
      match op with
      | UnOpNot -> "not"
      (* ToNumber *)
      | UnOpNegate -> "negate"
      | UnOpIncrement -> "increment"
      | UnOpDecrement -> "decrement"
      (* ToInt32 *)
      | UnOpBitNot -> "bitnot"
      | UnOpNegateInt -> "negate_i"
      | UnOpIncrementInt -> "increment_i"
      | UnOpDecrementInt -> "decrement_i"
      | UnOpSignExtend1 -> "extend1"
      | UnOpSignExtend8 -> "extend8"
      | UnOpSignExtend16 -> "extend16"
      (* Type conversion *)
      | UnOpCheckFilter -> "checkfilter"
      | UnOpConvertS -> "convert_s"
      | UnOpConvertI -> "convert_i"
      | UnOpConvertU -> "convert_u"
      | UnOpConvertD -> "convert_d"
      | UnOpConvertB -> "convert_b"
      | UnOpConvertO -> "convert_o"
      | UnOpCoerceB -> "coerce_b"
      | UnOpCoerceA -> "coerce_a"
      | UnOpCoerceI -> "coerce_i"
      | UnOpCoerceD -> "coerce_d"
      | UnOpCoerceS -> "coerce_s"
      | UnOpCoerceO -> "coerce_o"
    in
    writef "%s %s" op (use value)
  | BinaryOp (op, lhs, rhs) ->
    let op =
      match op with
      (* ToNumber *)
      | BinOpAdd -> "add"
      | BinOpSubtract -> "subtract"
      | BinOpMultiply -> "multiply"
      | BinOpDivide -> "divide"
      | BinOpModulo -> "modulo"
      (* ToInt32 *)
      | BinOpAddInt -> "add_i"
      | BinOpSubtractInt -> "subtract_i"
      | BinOpMultiplyInt -> "multiply_i"
      | BinOpLshift -> "lshift"
      | BinOpRshift -> "rshift"
      | BinOpUrshift -> "urshift"
      | BinOpBitAnd -> "bitand"
      | BinOpBitOr -> "bitor"
      | BinOpBitXor -> "bitxor"
      (* Boolean *)
      | BinOpEquals -> "equals"
      | BinOpStrictEquals -> "strictequals"
      | BinOpLessThan -> "lessthan"
      | BinOpLessEquals -> "lessequals"
      | BinOpGreaterThan -> "greaterthan"
      | BinOpGreaterEquals -> "greaterequals"
      | BinOpIn -> "in"
      | BinOpIsType -> "istype"
      | BinOpInstanceOf -> "instanceof"
      (* Type conversion *)
      | BinOpAsType -> "astype"
      | BinOpCoerce -> "coerce"
    in
    writef "%s %s, %s" op (use lhs) (use rhs)
  (* Invalid *)
  | Invalid | Argument _ | BasicBlock _ | Const _ | Super _
  -> assert false

let string_of_block' file buffer name blockv =
  let write = Buffer.add_string buffer in
  let block = block_of_value blockv in
  write (name blockv); write ":\n";
  List.iteri (fun idx insn ->
      write "    "; string_of_insn' file buffer name insn;
      write "\n")
    block.instructions

let string_of_func' file buffer name func =
  let write = Buffer.add_string buffer in
  write (Abc.string_of_multiname file func.return_type);
  write (Printf.sprintf " function %s(" func.name);
  List.iteri (fun idx arg ->
      match arg with
      | { opcode = Argument (name, ty) } ->
        if idx > 0 then write ", ";
        write name; write ": "; write (Abc.string_of_multiname file ty)
      | _ -> assert false)
    func.arguments;
  write ") {\n";
  List.iteri (fun idx block ->
      if idx > 0 then write "\n";
      string_of_block' file buffer name block)
    func.basic_blocks;
  write "}"

let wrap_string_of f file_of value =
  let idtbl  = Valuetbl.create 0 in
  let nextid = ref 0 in
  let name value =
    try
      Valuetbl.find idtbl value
    with Not_found ->
      let id = string_of_int !nextid in
      incr nextid;
      Valuetbl.add idtbl value id;
      id
  in
  let buffer  = Buffer.create 0 in
  f (file_of value) buffer name value;
  Buffer.contents buffer

let rec abc_file_of value =
  match value.parent with
  | ParentFunction   func  -> func.abc_file
  | ParentBasicBlock block -> abc_file_of block

let string_of_insn  = wrap_string_of string_of_insn'  abc_file_of
let string_of_block = wrap_string_of string_of_block' abc_file_of
let string_of_func  = wrap_string_of string_of_func'  (fun func -> func.abc_file)
