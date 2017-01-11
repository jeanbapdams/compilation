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
    | c = code EOF          {c}
    | EOF                   {{package_name=NoPackage; imports_list= []; class_or_interface= ""}}

code:		
	| PACKAGE p=package IMPORT i=imports CLASS c=classDeclaration { {package_name= Package(p); imports_list = i; class_or_interface= c} }
	| IMPORT i=imports CLASS c=classDeclaration { {package_name= NoPackage; imports_list= i; class_or_interface= c} }
	| PACKAGE p=package CLASS c=classDeclaration { {package_name= Package(p); imports_list= []; class_or_interface= c} }
	| CLASS c=classDeclaration { {package_name= NoPackage; imports_list= []; class_or_interface= c} }

package:
	| l=separated_list(DOT, IDENT) SEMICOLON {l}

imports:
	| i=import IMPORT l=imports{ i::l }
	| i=import {[i]}
import:
	| l=IDENT SEMICOLON {[l]}
	| s=idents DOT l=IDENT SEMICOLON {s@[l]}
	(* | l=separated_list(DOT, all) SEMICOLON {l} *)

idents:
	| l=separated_list(DOT, IDENT) {l}


classDeclaration:
	| s=IDENT {s}

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
