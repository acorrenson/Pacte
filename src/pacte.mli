type 'a result =   | Ok of string * 'a
                   | Ko of string list

type 'a parser

val satisfy : string -> (char -> bool) -> char parser

val char : char -> char parser

val (<|>) : char parser -> char parser -> char parser

val many1 : 'a parser -> 'a list parser

val main : unit -> string * char list