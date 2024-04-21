{
	open Parser
}

let white = [' ' '\t' '\n']
let alpha = ['a'-'z']
let num = ['0'-'9']
let ident = alpha(alpha|num)*

rule token = parse
| white { token lexbuf }
| "/*" { comment lexbuf; token lexbuf }
| '$'ident {
		let s = Lexing.lexeme lexbuf in
		DOLLAR (String.sub s 1 (String.length s - 1))
	}
| '.'['0'-'9''a'-'z''A'-'Z''_'] {
		let s = Lexing.lexeme lexbuf in
		PUTC (String.get s 1)
	}
| '.' { DOT }
| '(' { LPAR }
| ')' { RPAR }
| '?' { QUEST }
| '*' { ASTER }
| num+ { NUM (int_of_string (Lexing.lexeme lexbuf)) }
| ident { IDENT (Lexing.lexeme lexbuf) }
| ['A'-'Z'] { COMBINATOR (Lexing.lexeme lexbuf) }
| "->" { ARROW }
| eof { EOF }
| _   { failwith ("Unknown token: " ^ (Lexing.lexeme lexbuf)) }

and comment = parse
| "*/" { () }
| "/*" { comment lexbuf; comment lexbuf }
| eof { failwith "Unexpected eof" }
| _ { comment lexbuf }
