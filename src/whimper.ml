let _ =
  let filename  = Sys.argv.(1) in
  let abc  = Io.input_file filename in
  let file = Abc.load_file abc in
  let meth = file.Abc.file_methods.(0) in
  let name = Abc.multiname file meth.Abc.method_return_type in
  let sexp = Sexplib.Sexp.to_string_hum (Abc.sexp_of_multiname name) in
  print_endline sexp
