{
(*
  type lexeme =
    | EOF
    | IDENT of string
	| PACKAGE
	| DOT
    | SEMICOLON
    | OPEN_PAR | CLOSE_PAR
    | OPEN_CURL | CLOSE_CURL
    | OPEN_BRAC | CLOSE_BRAC
    | QUOTE | DOUBLE_QUOTE
*)

  let print_lexeme = function
    | EOF     -> print_string "EOF"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"
	| PACKAGE -> print_string "PACKAGE"
	| DOT -> print_string "DOT"
	| SEMICOLON -> print_string "SEMICOLON"
    | OPEN_PAR -> print_string "OPEN_PAR" | CLOSE_PAR -> print_string "CLOSE_PAR"
    | OPEN_CURL -> print_string "OPEN_CURL" | CLOSE_CURL -> print_string "CLOSE_CURL"
    | OPEN_BRAC -> print_string "OPEN_BRAC" | CLOSE_BRAC -> print_string "CLOSE_BRAC"
    | QUOTE -> print_string "QUOTE" | DOUBLE_QUOTE -> print_string "DOUBLEQUOTE"

}



let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let real = digit* ('.' digit*)?
let ident = letter (letter | digit | '_')*
let space = [' ' '\t' '\n']


rule nexttoken = parse
  | space+        { nexttoken lexbuf }
  | eof           { EOF }
  (* | real as nb    { FLOAT (float_of_string nb) } *)
  | "package"	  { PACKAGE }
  | '.'			  { DOT } (* si le point est entouré d'espaces, les espcaces ne seront pas détectés*)
  | ';'			  { SEMICOLON }
  | ident as str  { IDENT str }
  | '('             { OPEN_PAR }         
  | ')'             { CLOSE_PAR }         
  | '{'             { OPEN_CURL }         
  | '}'             { CLOSE_CURL }         
  | '['             { OPEN_BRAC }         
  | ']'             { CLOSE_BRAC }         
  | '''             { QUOTE }         
  | '"'             { DOUBLE_QUOTE }         
	
	



{ 
  let rec examine_all lexbuf =
    let res = nexttoken lexbuf in
    print_lexeme res;
    print_string " ";
    match res with
    | EOF -> ()
    | _   -> examine_all lexbuf
		    
  let compile file =
  print_string ("File "^file^" is being treated!\n");
  try
    let input_file = open_in file in
    let lexbuf = Lexing.from_channel input_file in
    examine_all lexbuf;
    print_newline ();
    close_in (input_file)
  with Sys_error s ->
    print_endline ("Can't find file '" ^ file ^ "'")
}
