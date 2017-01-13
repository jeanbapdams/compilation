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
    (*| EOF                   {{package_name=NoPackage; imports_list= []; class_or_interface= }}*)

code:		
	| p=packageOption i=importOption c=classDeclaration { {package_name= p; imports_list = i; class_or_interface= c} }

packageOption:
    | { NoPackage }
    | PACKAGE p=package { Package(p) }
    
importOption:
    | { [] }
    | IMPORT i=imports { i }

package:
	| l=separated_nonempty_list(DOT, IDENT) SEMICOLON {l}

imports:
	| i=import IMPORT l=imports{ i::l }
	| i=import {[i]}
import:
	| i=IDENT SEMICOLON {[i]}
	| MUL SEMICOLON {["*"]}
	| i=IDENT DOT l=import {i::l}

classDeclaration:
	| v=visibilityOption CLASS name=IDENT super=extendOption inters=implementOption OPEN_CURL test=IDENT CLOSE_CURL { {visibilityModifier= v; classIdentifier= name; super= super; interfaces= inters; classBody= test} }
	
visibilityOption:
    | { Public } (*default case*)
    | v=visibility { v }
    
extendOption:
    | { "" }
    | EXTENDS i=IDENT { i }
    
implementOption:
    | { [] }
    | IMPLEMENTS l=separated_nonempty_list(COMMA, IDENT) { l }

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
    
visibility:
    | PUBLIC     { Public }
    | PROTECTED  { Protected }
    | PRIVATE    { Private }

%%
