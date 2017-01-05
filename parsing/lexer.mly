%{
	(*open Types*)
	type package = string list
%}

%token EOF
%token PACKAGE
%token <string> IDENT
%token DOT

%start prog
%type <package> prog



%%

prog:
	| c = code EOF		{c}

code:
	| PACKAGE l=IDENT DOT r=IDENT DOT s=IDENT {[l;r;s]}



%%
