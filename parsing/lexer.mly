%{
	open Types
	(*type package = string list*)
%}

%token EOF
%token PACKAGE
%token <string> IDENT
%token DOT
%token SEMICOLON

%start prog
%type <package> prog



%%

prog:
	| c = code EOF		{c}

code:
	| PACKAGE l=separated_list(DOT, IDENT) SEMICOLON {l}



%%
