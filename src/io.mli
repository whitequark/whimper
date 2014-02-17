val open_in     : string -> in_channel
val open_out    : string -> out_channel

val input_all   : in_channel -> string

val input_file  : string -> string
val output_file : string -> string -> unit
