let _ =
  let filename   = Sys.argv.(1) in
  let method_ref = int_of_string Sys.argv.(2) in

  let abc  = Io.input_file filename in
  let file = Abc.load_file abc in

  let body = file.Abc.file_method_bodies.(method_ref) in
  print_endline (Abc.disassemble file body.Abc.body_code);

  let func  = Ssa.create_func ~name:"foo" ~arg_types:[Obj.magic 10] ~return_type:(Obj.magic 2) file in
  let block = Ssa.add_block func in
  ignore (Ssa.append_insn (Ssa.Jump block) block);
  print_endline (Ssa.string_of_func func)
  (* let func = Ssa.function_of_method_body file body in
  print_endline (Ssa.disassemble file func) *)
