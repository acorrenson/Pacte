type 'a parser

val satisfy : (char -> bool) -> char parser

val char : char -> char parser

val (<|>) : char parser -> char parser -> char parser