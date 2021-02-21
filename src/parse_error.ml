open Location

type parse_error = (location * string) list

let push (err : parse_error) (loc : location) (msg : string) =
  (loc, msg)::err

let mk_error (loc : location) (msg : string) =
  [loc, msg]

let show (out : out_channel) (err : parse_error) =
  List.iter (fun (l, m) ->
      Printf.fprintf out "@ offset %d - %s\n" l.offset m
    ) err