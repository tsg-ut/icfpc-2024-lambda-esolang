{
	open ParserLazyK
}

let white = [' ' '\t' '\n']
let comb = ['s' 'k' 'i' 'S' 'K' 'I']

rule token = parse
| white { token lexbuf }
| '(' { LPAR }
| ')' { RPAR }
| '*' { ASTER }
| '`' { GRAVE }
| comb { COMBINATOR (Lexing.lexeme lexbuf) }
| ('0'|'1')+ { COMBINATOR ("J" ^ Lexing.lexeme lexbuf) }
| eof { EOF }
| _   { failwith ("Unknown token: " ^ (Lexing.lexeme lexbuf)) }

