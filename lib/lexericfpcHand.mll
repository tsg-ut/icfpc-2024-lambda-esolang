{
	open ParsericfpcHand
	open Syntax.Icfpc

	let tail lexbuf = 
		let s = Lexing.lexeme lexbuf in
		(String.sub s 1 (String.length s - 1))

}

let base94 = ['!'-'~']
let white = [' ' '\t' '\n']
let alpha = ['a'-'z']
let num = ['0'-'9']
let ident = alpha(alpha|num)*

rule token = parse
| white { token lexbuf }
| "/*" { comment lexbuf; token lexbuf }
| '(' { LPAR }
| ')' { RPAR }
| eof { EOF }
| "T" { TRUE }
| "F" { FALSE }
| "I"(base94*) { 
		let s = tail lexbuf in
		let s = s2z s in
		INT s
	}
| 'S'(base94*) {
		let s = tail lexbuf in
		let s = s2read s in
		STR s
	}
| "U"(base94*) { 
		let s = tail lexbuf in
		UOP s
	}
| "B$" { BAPP }
| "B"(base94*) { 
		let s = tail lexbuf in
		BOP s
	}
| "?" { IF }
| "L"(base94*) {
		let s = tail lexbuf in
		let s = s2int s in
		LABS(s)
	}
| "v"(base94*) {
		let s = tail lexbuf in
		let s = s2int s in
		LVAR(s)
	}
| num+ {
		let s = Lexing.lexeme lexbuf in
		INT (Z.of_string s)
	}

| '"'(alpha|num)*'"' {
		let s = tail lexbuf in
		let s = String.sub s 0 (String.length s - 1) in
		STR s
	}
| "." { DOT }
| ident {
		let s = Lexing.lexeme lexbuf in
		IDENT (s2int s)
	}


and comment = parse
| "*/" { () }
| "/*" { comment lexbuf; comment lexbuf }
| eof { failwith "Unexpected eof" }
| _ { comment lexbuf }
