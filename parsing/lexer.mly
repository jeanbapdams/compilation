%{
	open Types
	(*type package = string list*)
%}

%token EOF
%token <string> IDENT
%token DOT
%token COMMA
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

%token <string> INTEGER
%token <string> REAL
%token TRUE
%token FALSE

%start prog
%type <package * variableDeclarator list> prog



%%

prog:
	| p=package c=code EOF		{(p,c)}
    | c = code EOF          {(NoPackage,c)}
    | EOF                   {(NoPackage,[])}

package:
	| PACKAGE l=separated_list(DOT, IDENT) SEMICOLON {Package l}

code:
    | f=fieldDeclaration SEMICOLON c=code   { f::c }
    | EOF              {[]}

fieldDeclaration:
    | fm=fieldModifier fd=fieldDeclaration 
    {
        match fd with 
        | ModTypeId (l,f,i) -> ModTypeId(fm::l,f,i)
        | ModTypeIdInit (l,f,i,init) -> ModTypeIdInit(fm::l,f,i,init)
    }
        | i1=IDENT i2=IDENT { ModTypeId([],i1,i2) }

fieldModifier:
    | STATIC    { STATIC }
    | FINAL     { FINAL }
    | TRANSIENT { TRANSIENT }
    | VOLATILE  { VOLATILE }


%%
