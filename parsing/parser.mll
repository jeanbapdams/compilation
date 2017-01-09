{
  let print_lexeme = function
    | EOF     -> print_string "EOF"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"
	| PACKAGE -> print_string "PACKAGE"
	| DOT -> print_string "DOT"
    | COMMA -> print_string "COMMA"
	| SEMICOLON -> print_string "SEMICOLON"
    | OPEN_PAR -> print_string "OPEN_PAR" | CLOSE_PAR -> print_string "CLOSE_PAR"
    | OPEN_CURL -> print_string "OPEN_CURL" | CLOSE_CURL -> print_string "CLOSE_CURL"
    | OPEN_BRAC -> print_string "OPEN_BRAC" | CLOSE_BRAC -> print_string "CLOSE_BRAC"
    | QUOTED_CHAR c -> print_string "QUOTED_CHAR("; print_string c; print_string ")";
    | QUOTED_STRING s -> print_string "QUOTED_STRING("; print_string s; print_string ")";
    | QUOTE -> print_string "QUOTE" | DOUBLE_QUOTE -> print_string "DOUBLEQUOTE"
    | PUBLIC -> print_string "PUBLIC" | PROTECTED -> print_string "PROTECTED" | PRIVATE -> print_string "PRIVATE" | STATIC -> print_string "STATIC"
    | INTEGER n -> print_string "INTEGER("; print_string n; print_string ")"
    | REAL n -> print_string "REAL("; print_string n; print_string ")"
    | PLUS -> print_string "PLUS" | MINUS -> print_string "MINUS" | MUL -> print_string "MUL" | DIV -> print_string "DIV" | MOD -> print_string "MOD" | POWER -> print_string "POWER"
    | TRUE -> print_string "TRUE" | FALSE -> print_string "FALSE"
    | _ -> print_string "to be implemented"
}



let space = [' ' '\t' '\n']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let real = digit+ '.' digit+
let quoted_char =  ''' _ '''
let quoted_string = '"'[^'"']*'"'
let ident = letter (letter | digit | '_')*
let operator = "=" | ">" | "<" | "!" | "~" | "?" | ";" | "==" | ">=" | "<=" | "!=" | "&" | "| " | "&&" | "| | " | "++" | "--" | "+" | "-" | "*" | "/" | "^" | "%" | "+=" | "-=" | "*=" | "/=" | "^=" | "%=" | "<<" | "<<<" | ">>" | ">>>" | "<<=" | "<<<=" | ">>=" | ">>>="

rule nexttoken = parse
  | space+        { nexttoken lexbuf }
  | eof           { EOF }
  | "package"	  { PACKAGE }
  | '.'			  { DOT } (* si le point est entouré d'espaces, les espcaces ne seront pas détectés*)
  | ','             { COMMA }
  | ';'			  { SEMICOLON }
  | '('             { OPEN_PAR }
  | ')'             { CLOSE_PAR }
  | '{'             { OPEN_CURL }
  | '}'             { CLOSE_CURL }
  | '['             { OPEN_BRAC }
  | ']'             { CLOSE_BRAC }
  | quoted_char as c    { QUOTED_CHAR c }
  | quoted_string as s  { QUOTED_STRING s }
  | '''             { QUOTE }
  | '"'             { DOUBLE_QUOTE }
  | "public"        { PUBLIC }
  | "protected"     { PROTECTED }
  | "private"       { PRIVATE }
  | "static"        { STATIC }
  | integer as n    { INTEGER n }
  | real as n       { REAL n }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { MUL }
  | '/'             { DIV }
  | '%'             { MOD }
  | '^'             { POWER }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | operator as op  { string_to_operator op }
  | ident as str  { if str="class" then IDENT "ceci est une classe" else IDENT str }

	
	



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

