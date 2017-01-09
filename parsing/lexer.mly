%{
	open Types
	(*type package = string list*)
%}

%token EOF
%token PACKAGE
%token <string> IDENT
%token DOT
%token SEMICOLON
%token OPEN_PAR
%token CLOSE_PAR
%token OPEN_CURL
%token CLOSE_CURL
%token OPEN_BRAC
%token CLOSE_BRAC
%token <string> QUOTED_CHAR
%token <string> QUOTED_STRING
%token QUOTE
%token DOUBLE_QUOTE
%token PUBLIC
%token PROTECTED
%token PRIVATE
%token STATIC
%token <string> REAL
%token PLUS
%token MINUS
%token MUL
%token DIV
%token MOD
%token POWER

%start prog
%type <package> prog



%%

prog:
	| c = code EOF		{c}

code:
	| PACKAGE l=separated_list(DOT, IDENT) SEMICOLON {l}



%%
