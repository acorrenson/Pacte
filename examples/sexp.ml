open Pacte
open Combinators

type sexp =
  | Nil
  | Atom of string
  | Cons of sexp * sexp

let rec show out = function
  | Nil -> Printf.fprintf out "()"
  | Atom s -> Printf.fprintf out "%s" s
  | Cons (car, cdr) ->
    Printf.fprintf out "(%a %a)" show car show cdr

let rec of_list = function
  | [] -> Nil
  | x::xs -> Cons (x, of_list xs)

let ascii_filter i =
  (i = 33) ||
  (35 <= i && i <= 39) || 
  (42 <= i && i <= 126)

let alpha_nums =
  let open List in
  init 94 ((+) 33)
  |> filter ascii_filter
  |> map char_of_int

let parens p =
  char '(' *> p <* char ')'

let implode (l : char list) =
  List.fold_left
    (fun acc c -> acc ^ String.make 1 c) "" l


let blank =
  (char '\t' <|> char ' ' <|> char '\n')

let spaced p =
  many blank *> p <* many blank


let alpha_num =
  one_of "alpha numerical char expected" (List.map char alpha_nums)

let ident =
  many1 alpha_num => implode

let atom =
  ident => (fun s -> Atom s)

let rec sexp input =
  let psexp =
    one_of "'sexp' expected, you may have forgot a ')' ?"
      [atom; parens (many sexp => of_list)]
  in spaced psexp input

let anti_pattern =
  "extra ')' found" <!> (
    let* s = sexp in
    let* _ = spaced (char ')') in
    return s
  )

let _ =
  parse_string sexp "(+ (* a b) (/ a d))"
  |> show stdout
  |> print_newline