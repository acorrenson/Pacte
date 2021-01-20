
type 'a result =
  | Ok of string * 'a
  | Ko of string list

type 'a parser = string -> 'a result

let rem s =
  String.(sub s 1 ((length s) - 1))

let satisfy (name : string) (pred : char -> bool) : char parser = fun input ->
  match input with
  | "" -> Ko [name; "End of buffer"]
  | _ ->
    if pred input.[0] then Ok (rem input, input.[0])
    else Ko [name]

let char (c : char) = satisfy ("char " ^ String.make 1 c) ((=) c)

let (<|>) (p1 : 'a parser) (p2 : 'a parser) : 'a parser = fun input ->
  match p1 input with
  | Ok _ as res -> res
  | Ko _ -> p2 input

let (<?>) name p = fun inp ->
  match p inp with
  | Ok _ as res -> res
  | Ko l -> Ko (name::l)

let rec many1 p = fun inp ->
  match p inp with
  | Ko _ as k -> k
  | Ok (next, res) ->
    match many1 p next with
    | Ok (next', res') -> Ok (next', res::res')
    | Ko _ as k -> k

let run p inp =
  match p inp with
  | Ko l ->
    List.iter (Printf.printf "Error @ %s\n") l;
    exit (-1)
  | Ok (n, r) -> n, r

let main () =
  let p = "a | b" <?> (char 'a' <|> char 'b') in
  let q = "(a | b)*" <?> (many1 p) in
  run q "cba"


