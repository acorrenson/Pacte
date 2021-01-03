type 'a parser = string -> (string * 'a)

exception ParseFail

let rem s =
  String.(sub s 1 ((length s) - 1))

let satisfy (pred : char -> bool) : char parser = fun input ->
  match input with
  | "" -> failwith "no more characters"
  | _ ->
    if pred input.[0] then rem input, input.[0]
    else raise ParseFail

let char (c : char) = satisfy ((=) c)

let (<|>) (p1 : 'a parser) (p2 : 'a parser) : 'a parser = fun input ->
  try p1 input
  with ParseFail -> p2 input



