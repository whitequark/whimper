let _ =
  let filename  = Sys.argv.(1) in
  let abc       = Io.input_file filename in
  let file      = Abc.load_file abc in
  let sexp      = Sexplib.Sexp.to_string_hum (Abc.sexp_of_file file) in
  print_endline sexp;
  let filename' = Sys.argv.(2) in
  let file'     = Abc.dump_file file in
  Io.output_file filename' file'
