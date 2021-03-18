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
    match x.reference with
    | String s ->
      Printf.fprintf out "%s\n" s;
      Printf.fprintf out "\x1b[1;31m%s\x1b[0m\n" (String.init x.offset (fun i -> if i = x.offset - 1 then '^' else ' '));
      List.iter (fun (l, m) ->
          Printf.fprintf out "@ offset %d - %s\n" l.offset m
        ) err
    | File s ->
      Printf.fprintf out "\x1b[1;31mparse error\x1b[0m in file %s\n" s;
      List.iter (fun (l, m) ->
          Printf.fprintf out "@ offset %d - %s\n" l.offset m
        ) err