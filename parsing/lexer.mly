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

%start <program> prog
(*%type <program> prog*)



%%

prog:
	| c = code EOF		{c}

code:		
	| PACKAGE p=package IMPORT i=imports CLASS c=classDeclaration { {package_name= p; imports_list = i; class_or_interface= c} }
	| IMPORT i=imports CLASS c=classDeclaration { {package_name= []; imports_list= i; class_or_interface= c} }
	| PACKAGE p=package CLASS c=classDeclaration { {package_name= p; imports_list= []; class_or_interface= c} }
	| CLASS c=classDeclaration { {package_name= []; imports_list= []; class_or_interface= c} }
	

package:
	| l=separated_list(DOT, IDENT) SEMICOLON {l}

imports:
	| i=import IMPORT l=imports { i::l }
	| i=import {[i]}
import:
	| l=separated_list(DOT, IDENT) SEMICOLON {l}
	| l=separated_list(DOT, IDENT) DOT MUL SEMICOLON {l}

classDeclaration:
	| s=IDENT {s}
	

%%
