{
	open Parsericfpc

	let tail lexbuf = 
		let s = Lexing.lexeme lexbuf in
		(String.sub s 1 (String.length s - 1))
	
	let s2int s =
		String.fold_left (fun i c -> i * 94 + (Char.code c - Char.code '!')) 0 s
	
	let read2s =
		let table = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
		in
		fun s -> (
		String.map (fun c ->
			let ti =
				match String.index_from_opt table 0 c with
				| Some i -> i
				| None -> 
						Format.eprintf "Invalid char: %c@." c;
						assert false 
			in
			(Char.chr (Char.code '!' + ti)) ) s
		)

	let s2read =
		let table = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
		in
		fun s -> (
		String.map (fun c -> 
			String.get table (Char.code c - Char.code '!')) s
		)
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
		let s = s2int s in
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
| "?" { BOP("IF") }
| "L"(base94*) {
		let s = tail lexbuf in
		LABS(s)
	}
| "v"(base94*) {
		let s = tail lexbuf in
		LVAR(s)
	}

and comment = parse
| eof { failwith "Unexpected eof" }
| _ { comment lexbuf }
