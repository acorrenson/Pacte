open Parse_error
open Location

type 'a result =
  | Ok of ('a * int)
  | Ko of (parse_error * bool)

type 'a parser = location -> 'a result

val return : 'a -> 'a parser

val char : char -> char parser

val (<|>) : 'a parser -> 'a parser -> 'a parser

val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser

val (>=>) : 'a parser -> ('a -> (location * location) -> 'b parser) -> 'b parser

val (let*) : 'a parser -> ('a -> 'b parser) -> 'b parser

val (=>) : 'a parser -> ('a -> 'b) -> 'b parser

val (=>>) : 'a parser -> ('a -> (location * location) -> 'b) -> 'b parser

val (<~>) : 'a parser -> 'a list parser -> 'a list parser

val one_of : string -> 'a parser list -> 'a parser

val many  : 'a parser -> 'a list parser

val many1 : 'a parser -> 'a list parser

val ( *> ) : 'a parser -> 'b parser -> 'b parser

val ( <* ) : 'a parser -> 'b parser -> 'a parser

val chainl : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser

val chainr : 'a parser -> ('a -> 'a -> 'a) parser -> 'a parser

val (<?>) : string -> 'a parser -> 'a parser

val (<+?>) : string -> 'a parser -> 'a parser

val (<!>) : string -> 'a parser -> 'a parser

val parse : 'a parser -> location -> 'a

val parse_file : 'a parser -> string -> 'a

val parse_string : 'a parser -> string -> 'a