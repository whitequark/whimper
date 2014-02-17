let open_in filename =
  if filename = "-" then stdin
  else open_in filename

let open_out filename =
  if filename = "-" then stdout
  else open_out filename

let input_all channel =
  let rec input_some chunk pos len =
    let new_pos = pos + (input channel chunk pos (len - pos)) in
    if new_pos = pos then
      String.sub chunk 0 pos
    else if new_pos < len then
      input_some chunk new_pos len
    else
      let new_len   = len * 2 in
      let new_chunk = String.create new_len in
      String.blit chunk 0 new_chunk 0 new_pos;
      input_some new_chunk new_pos new_len
  in
  input_some (String.create 4096) 0 4096

let input_file filename =
  let chan = open_in filename in
  let buf  = input_all chan in
  close_in chan;
  buf

let output_file filename buf =
  let chan = open_out filename in
  output_string chan buf;
  close_out chan
