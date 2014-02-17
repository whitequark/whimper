module Reader : sig
  type t

  val create     : string -> t

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

  let read_u8 r =
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
    if s24 land 0x800000 <> 0 then s24 - 0x100000 else s24

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

let integer     file idx = file.file_const_pool.const_pool_integers.(idx)
let uinteger    file idx = file.file_const_pool.const_pool_uintegers.(idx)
let double      file idx = file.file_const_pool.const_pool_doubles.(idx)
let string      file idx = file.file_const_pool.const_pool_strings.(idx)
let namespace   file idx = file.file_const_pool.const_pool_namespaces.(idx)
let ns_set      file idx = file.file_const_pool.const_pool_ns_sets.(idx)
let multiname   file idx = file.file_const_pool.const_pool_multinames.(idx)
let method_     file idx = file.file_methods.(idx)
let metadata    file idx = file.file_metadata.(idx)
let class_      file idx = file.file_classes.(idx)
let script      file idx = file.file_scripts.(idx)
let method_body file idx = file.file_method_bodies.(idx)

let load_file input =
  let r = Reader.create input in
  let rec read_some f cnt =
    let rec loop cnt rest =
      if cnt > 0 then loop (cnt - 1) ((f ()) :: rest) else rest
    in
    Array.of_list (List.rev (loop cnt []))
  in
  let read_array f =
    read_some f (Reader.read_u30 r)
  in
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
    let code             = read_string () in
    let exceptions       = read_array read_exception in
    let traits           = read_array read_trait in
    { body_method           = method_idx;
      body_max_stack        = max_stack;
      body_local_count      = local_count;
      body_init_scope_depth = init_scope_depth;
      body_max_scope_depth  = max_scope_depth;
      body_code             = code;
      body_exceptions       = exceptions;
      body_traits           = traits; }
  (* exception_info {
    u30 from
    u30 to
    u30 target
    u30 exc_type
    u30 var_name
    } *)
  and read_exception () =
    let from_pc   = Reader.read_u30 r in
    let to_pc     = Reader.read_u30 r in
    let target_pc = Reader.read_u30 r in
    let exc_type  = Reader.read_u30 r in
    let var_name  = Reader.read_u30 r in
    { exception_from     = from_pc;
      exception_to       = to_pc;
      exception_target   = target_pc;
      exception_type     = exc_type;
      exception_var_name = var_name; }
  in
  read_file ()

let dump_file file =
  let w = Writer.create () in
  let rec write_some f arr =
    let rec loop lst =
      match lst with
      | elem :: rest -> f elem; loop rest
      | [] -> ()
    in
    loop (Array.to_list arr)
  in
  let write_array f arr =
    Writer.write_u30 w (Array.length arr);
    write_some f arr
  in
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
    write_some write_instance file.file_instances;
    write_some write_class file.file_classes;
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
      write_some f arr
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
    write_some (Writer.write_u30 w) meth.method_param_types;
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
        write_some write_string param_names)
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
    Option.map (write_array (Writer.write_u30 w)) trait.trait_metadata
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
    write_string body.body_code;
    write_array write_exception body.body_exceptions;
    write_array write_trait body.body_traits
  (* exception_info {
    u30 from
    u30 to
    u30 target
    u30 exc_type
    u30 var_name
    } *)
  and write_exception exc =
    Writer.write_u30 w exc.exception_from;
    Writer.write_u30 w exc.exception_to;
    Writer.write_u30 w exc.exception_target;
    Writer.write_u30 w exc.exception_type;
    Writer.write_u30 w exc.exception_var_name
  in
  write_file file;
  Writer.to_string w
