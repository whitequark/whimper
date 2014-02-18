let _ =
  let filename  = Sys.argv.(1) in
  let abc  = Io.input_file filename in
  let file = Abc.load_file abc in
  let body = file.Abc.file_method_bodies.(int_of_string Sys.argv.(2)) in
  print_endline (Abc.disassemble file body.Abc.body_code)
