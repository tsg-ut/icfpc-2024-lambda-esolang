{
	open Parsericfpc
	open Syntax.Icfpc

	let tail lexbuf = 
		let s = Lexing.lexeme lexbuf in
		(String.sub s 1 (String.length s - 1))

}

let base94 = ['!'-'~']
let white = [' ' '\t' '\n']

rule token = parse
| white { token lexbuf }
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

and comment = parse
| eof { failwith "Unexpected eof" }
| _ { comment lexbuf }
