open Location

type parse_error = (location * string) list

let push (err : parse_error) (loc : location) (msg : string) =
  (loc, msg)::err

let mk_error (loc : location) (msg : string) =
  [loc, msg]

let show (out : out_channel) (err : parse_error) =
  match err with
  | [] -> ()
  | (x, _)::_ ->
    let n = String.length x.reference in
    Printf.fprintf out "%s\n" x.reference;
    Printf.fprintf out "\x1b[1;31m%s\x1b[0m\n" (String.init n (fun i -> if i = x.offset then '^' else ' '));
    List.iter (fun (l, m) ->
      Printf.fprintf out "@ offset %d - %s\n" l.offset m
    ) err