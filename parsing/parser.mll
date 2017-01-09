{

  let print_lexeme = function
    | EOF     -> print_string "EOF"
    | IDENT s -> print_string "IDENT("; print_string s; print_string ")"
	| DOT -> print_string "DOT"
    | COMMA -> print_string "COMMA"
	| SEMICOLON -> print_string "SEMICOLON"
    | OPEN_PAR -> print_string "OPEN_PAR" | CLOSE_PAR -> print_string "CLOSE_PAR"
    | OPEN_CURL -> print_string "OPEN_CURL" | CLOSE_CURL -> print_string "CLOSE_CURL"
    | OPEN_BRAC -> print_string "OPEN_BRAC" | CLOSE_BRAC -> print_string "CLOSE_BRAC"
    | QUOTED_CHAR c -> print_string "QUOTED_CHAR("; print_string c; print_string ")";
    | QUOTED_STRING s -> print_string "QUOTED_STRING("; print_string s; print_string ")";
    | QUOTE -> print_string "QUOTE" | DOUBLE_QUOTE -> print_string "DOUBLEQUOTE"
    | ABSTRACT -> print_string "ABSTRACT"
    | CONTINUE -> print_string "CONTINUE"
    | ASSERT -> print_string "ASSERT"
    | BOOLEAN -> print_string "BOOLEAN"
    | BREAK -> print_string "BREAK"
    | BYTE -> print_string "BYTE"
    | CASE -> print_string "CASE"
    | CATCH -> print_string "CATCH"
    | CHAR -> print_string "CHAR"
    | CLASS -> print_string "CLASS"
    | CONST -> print_string "CONST"
    | FOR -> print_string "FOR"
    | DEFAULT -> print_string "DEFAULT"
    | DO -> print_string "DO"
    | DOUBLE -> print_string "DOUBLE"
    | ELSE -> print_string "ELSE"
    | ENUM -> print_string "ENUM"
    | EXTENDS -> print_string "EXTENDS"
    | FINAL -> print_string "FINAL"
    | FINALLY -> print_string "FINALLY"
    | FLOAT -> print_string "FLOAT"
    | NEW -> print_string "NEW"
    | IF -> print_string "IF"
    | GOTO -> print_string "GOTO"
    | IMPLEMENTS -> print_string "IMPLEMENTS"
    | IMPORT -> print_string "IMPORT"
    | INSTANCEOF -> print_string "INSTANCEOF"
    | INT -> print_string "INT"
    | INTERFACE -> print_string "INTERFACE"
    | LONG -> print_string "LONG"
    | NATIVE -> print_string "NATIVE"
    | SWITCH -> print_string "SWITCH"
    | PACKAGE -> print_string "PACKAGE"
    | PRIVATE -> print_string "PRIVATE"
    | PROTECTED -> print_string "PROTECTED"
    | PUBLIC -> print_string "PUBLIC"
    | RETURN -> print_string "RETURN"
    | SHORT -> print_string "SHORT"
    | STATIC -> print_string "STATIC"
    | STRICTFP -> print_string "STRICFP"
    | SUPER -> print_string "SUPER"
    | SYNCHRONIZED -> print_string "SYNCHRONIZED"
    | THIS -> print_string "THIS"
    | THROW -> print_string "THROW"
    | THROWS -> print_string "THROWS"
    | TRANSIENT -> print_string "TRANSIENT"
    | TRY -> print_string "TRY"
    | VOID -> print_string "VOID"
    | VOLATILE -> print_string "VOLATILE"
    | WHILE  -> print_string "WHILE"
    | INTEGER n -> print_string "INTEGER("; print_string n; print_string ")"
    | REAL n -> print_string "REAL("; print_string n; print_string ")"
    | PLUS -> print_string "PLUS" | MINUS -> print_string "MINUS" | MUL -> print_string "MUL" | DIV -> print_string "DIV" | MOD -> print_string "MOD" | POWER -> print_string "POWER"
    | TRUE -> print_string "TRUE" | FALSE -> print_string "FALSE"
    | _ -> print_string "to be implemented"
    
let string_to_keyword s = match s with
|"abstract" ->      ABSTRACT     
|"continue" ->      CONTINUE
|"assert" ->        ASSERT
|"boolean" ->       BOOLEAN
|"break" ->         BREAK
|"byte" ->          BYTE
|"case" ->          CASE
|"catch" ->         CATCH
|"char" ->          CHAR
|"class" ->         CLASS
|"const" ->         CONST
|"for" ->           FOR
|"default" ->       DEFAULT
|"do" ->            DO
|"double" ->        DOUBLE
|"else" ->          ELSE
|"enum" ->          ENUM
|"extends" ->       EXTENDS
|"final" ->         FINAL
|"finally" ->       FINALLY
|"float" ->         FLOAT
|"new" ->           NEW
|"if" ->            IF
|"goto" ->          GOTO
|"implements" ->    IMPLEMENTS
|"import" ->        IMPORT
|"instanceof" ->    INSTANCEOF
|"int" ->           INT
|"interface" ->     INTERFACE
|"long" ->          LONG
|"native" ->        NATIVE
|"switch" ->        SWITCH
|"package" ->       PACKAGE
|"private" ->       PRIVATE
|"protected" ->     PROTECTED
|"public" ->        PUBLIC
|"return" ->        RETURN
|"short" ->         SHORT
|"static" ->        STATIC
|"stricfp" ->       STRICTFP
|"super" ->         SUPER
|"synchronized"->   SYNCHRONIZED
|"this" ->          THIS
|"throw" ->         THROW
|"throws" ->        THROWS
|"transient" ->     TRANSIENT
|"try" ->           TRY
|"void" ->          VOID
|"volatile" ->      VOLATILE
|"while" ->         WHILE

}



let space = [' ' '\t' '\n']
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let real = digit+ '.' digit+
let quoted_char =  ''' _ '''
let quoted_string = '"'[^'"']*'"'
let ident = letter (letter | digit | '_')*
let space = [' ' '\t' '\n']
let comment = ("//" [^'\n']* '\n')

let keywords = "abstract" | "continue" | "assert" | "boolean" | "break" | "byte" | "case" | "catch" | "char" | "class" | "const" | "for" | "default" | "do" | "double" | "else" | "enum" | "extends" | "final" | "finally" | "float" | "new" | "if" | "goto" | "implements" | "import" | "instanceof" | "int" | "interface" | "long" | "native" | "switch" | "package" | "private" | "protected" | "public" | "return" | "short" | "static" | "strictfp" | "super" | "synchronized" | "this" | "throw" | "throws" | "transient" | "try" | "void" | "volatile" | "while"

let operator = "=" | ">" | "<" | "!" | "~" | "?" | ";" | "==" | ">=" | "<=" | "!=" | "&" | "| " | "&&" | "| | " | "++" | "--" | "+" | "-" | "*" | "/" | "^" | "%" | "+=" | "-=" | "*=" | "/=" | "^=" | "%=" | "<<" | "<<<" | ">>" | ">>>" | "<<=" | "<<<=" | ">>=" | ">>>="

rule nexttoken = parse
  | space+          { nexttoken lexbuf }
  | comment         { nexttoken lexbuf }
  | eof             { EOF }
  | '.'			    { DOT } (* si le point est entouré d'espaces, les espcaces ne seront pas détectés*)
  | ','             { COMMA }
  | ';'			    { SEMICOLON }
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
  | keywords as kw  { string_to_keyword kw }
  | ident as str    { IDENT str }
  | integer as n    { INTEGER n }
  | real as n       { REAL n }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | operator as op  { string_to_operator op }


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

