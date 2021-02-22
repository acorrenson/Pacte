open Location

type parse_error = (location * string) list

val push : parse_error -> location -> string -> parse_error

val mk_error : location -> string -> parse_error

val show : out_channel -> parse_error -> unit
