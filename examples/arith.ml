open Pacte.Combinators
open Pacte.Location

let ab =
  let* a = char 'a' in
  let* b = char 'b' in
  return (a, b)

let mab = ("mab" <+?> many ("ab" <+?> ab))

type expr = 
  | Sum of expr * expr
  | Prod of expr * expr
  | Val of char

let rec print_expr out = function
  | Sum (a, b) -> Printf.printf "(%a + %a)" print_expr a print_expr b
  | Prod (a, b) -> Printf.printf "(%a * %a)" print_expr a print_expr b
  | Val c -> Printf.fprintf out "%c" c

let sum = char '+' *> return (fun x y -> Sum (x, y))

let prod = char '*' *> return (fun x y -> Prod (x, y))

let rec expr (l : location) =
  (chainl term sum) l

and term (l : location) =
  (chainl fact prod) l

and fact (l : location) =
  (one_of "a factor is expected" [
    var;
    char '(' *> expr <* char ')';
  ]) l

and var (l : location) =
  (char 'x' <|> char 'y' <|> char 'z' => (fun x -> Val x)) l

let _ = 
  run expr "x+y*z+x*(x+x+y*)"
  |> print_expr stdout
  |> print_newline