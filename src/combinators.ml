open Parse_error
open Location

type 'a result =
  | Ok of ('a * int)
  | Ko of (parse_error * bool)

type 'a parser = location -> 'a result

let return (x : 'a) _ =
  Ok (x, 0)

let char (c : char) (loc : location) =
  match loc.data with
  | []   -> Ko (mk_error loc "unexpected EOF", false)
  | x::_ ->
    if x == c then Ok (c, 1)
    else Ko (mk_error loc (Printf.sprintf "expected %c found %c" c x), false)

let rec skip (n : int) (l : 'a list) =
  if n = 0 then l
  else skip (n - 1) (List.tl l)

let update_loc (loc : location) (n : int) = {
  loc with
  offset = loc.offset + n;
  data = skip n loc.data;
}

let commit_if (b : bool) (res : 'a result) = 
  match res with
  | Ko (err, contrib) -> Ko (err, contrib || b)
  | _ -> res

let advance_sucess (n : int) (res : 'a result)=
  match res with
  | Ok (ret, m) -> Ok (ret, m + n)
  | _ -> res

let (>>=) (p : 'a parser) (f : 'a -> 'b parser) (loc : location) : 'b result =
  match p loc with
  | Ok (res, n) ->
      (f res) (update_loc loc n)
      |> commit_if (n != 0)
      |> advance_sucess n
  | Ko _ as err -> err

let (>=>) (p : 'a parser) (f : 'a -> (location * location) -> 'b parser) (loc : location) =
  match p loc with
  | Ok (res, n) ->
      let loc' = update_loc loc n in 
      (f res (loc, loc')) loc'
      |> commit_if (n != 0)
      |> advance_sucess n
  | Ko _ as err -> err

let (let*) = (>>=)

let (<|>) (p : 'a parser) (q : 'a parser) (loc : location) : 'a result =
  match p loc with
  | Ko (_, false) -> q loc
  | _ as res -> res

let (=>) (p : 'a parser) (f : 'a -> 'b) =
    p >>= (fun x -> return (f x))

let (=>>) (p : 'a parser) (f : 'a -> (location * location) -> 'b) =
    p >=> (fun x l -> return (f x l))

let (<~>) px pxs =
  let* r = px in
  let* rs = pxs in
  return (r::rs)

let ( *> ) p q = p >>= fun _ -> q

let ( <* ) p q = p >>= fun x -> q >>= fun _ -> return x

let (<?>) (msg : string) (p : 'a parser) (loc : location) =
  match p loc with
  | Ok _ as res -> res
  | Ko (err, b) ->
      match err with
      | [] -> Ko (mk_error loc msg, b)
      | (l, _)::_ -> Ko (mk_error l msg, b)

let (<+?>) (name : string) (p : 'a parser) (loc : location) =
  match p loc with
  | Ok _ as res -> res
  | Ko (err, b) -> Ko (push err loc ("while parsing " ^ name), b)

let (<!>) (error : string) (p : 'a parser) (loc : location) =
  match p loc with
  | Ok (_, off) -> Ko (mk_error (update_loc loc off) error, true)
  | Ko _ as err -> err

let one_of (msg : string) (pl : 'a parser list) =
  match pl with
  | [] -> invalid_arg "one_of"
  | x::xs -> msg <?> (List.fold_left (<|>) x xs)

let rec many p =
  let inner (loc : location) = begin
    let* r = p in
    let* rs = many p in
    return (r::rs)
  end loc
  in
  (inner <|> return [])

let many1 p =
  p <~> many p


let chainl (p : 'a parser) (op : ('a -> 'a -> 'a) parser) =
  let rec loop t1 = begin
    let* f = op in
    let* t2 = p in
    loop (f t1 t2)
  end <|> return t1
  in
  p >>= loop

let rec chainr (p : 'a parser) (op : ('a -> 'a -> 'a) parser) =
  let* t1 = p in
  begin
    let* f = op in
    let* t2 = chainr p op in
    return (f t1 t2)
  end <|> return t1

let list_of_string (s : string) =
  let len = String.length s in
  let rec step (i : int) (acc : char list) =
    if i < 0 then acc
    else step (i - 1) (s.[i]::acc)
  in
  step (len - 1) []

let run (p : 'a parser) (s : string) =
  match p {
    offset = 0;
    data = list_of_string s;
    reference = s;
  } with
  | Ok (res, _) -> res
  | Ko (err, _) -> Printf.printf "%a" show err; exit 1