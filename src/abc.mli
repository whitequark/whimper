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
  body_code             : string;
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
with sexp

val load_file : string -> file
val dump_file : file -> string

val integer     : file -> integer_ref     -> Int32.t
val uinteger    : file -> uinteger_ref    -> Uint32.t
val double      : file -> double_ref      -> float
val string      : file -> string_ref      -> string
val namespace   : file -> namespace_ref   -> namespace
val ns_set      : file -> ns_set_ref      -> ns_set
val multiname   : file -> multiname_ref   -> multiname
val method_     : file -> method_ref      -> method_
val metadata    : file -> metadata_ref    -> metadata
val class_      : file -> class_ref       -> class_
val script      : file -> script_ref      -> script
val method_body : file -> method_body_ref -> method_body

