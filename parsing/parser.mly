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

%start <statement> statement

%%

prog:
    | c = code EOF          {c}
    (*| EOF                   {{package_name=NoPackage; imports_list= []; class_or_interface= }}*)

code:		
	| p=packageOption i=importOption c=classDeclaration { {packageName= p; importsList = i; classOrInterface= c} }

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
	 | v=visibilityOption CLASS name=IDENT typeParam=typeParameterOption super=extendOption inters=implementOption OPEN_CURL body=classBodyDeclarations CLOSE_CURL { {visibilityModifier= v; classIdentifier= name; typeParameters= typeParam; super= super; interfaces= inters; classBody= body} }

typeParameterOption:
    | { [] }
    | SMALLER l=separated_nonempty_list(COMMA, javaType) GREATER { l }
	
visibilityOption:
    | { Public } (*default case*)
    | v=visibility { v }
    
extendOption:
    | { "" }
    | EXTENDS i=IDENT { i }
    
implementOption:
    | { [] }
    | IMPLEMENTS l=separated_nonempty_list(COMMA, IDENT) { l }

classBodyDeclarations:
    | { [] }
    | l=classBodyDeclarations d=classBodyDeclaration { l@[d] }
    
classBodyDeclaration:
    | cmd=classMemberDeclaration { ClassMemberDeclaration(cmd) }
    
classMemberDeclaration: 
    | f=fieldDeclaration SEMICOLON { FieldDeclaration(f) }

fieldDeclaration:
    | fm=fieldModifier fd=fieldDeclaration 
    {
        match fd with 
        | ModTypeId (l,f,i) -> ModTypeId(fm::l,f,i)
        | ModTypeIdInit (l,f,i,init) -> ModTypeIdInit(fm::l,f,i,init)
    }
    | i1=javaType i2=IDENT { ModTypeId([],i1,i2) }

fieldModifier:
    | STATIC    { STATIC }
    | FINAL     { FINAL }
    | TRANSIENT { TRANSIENT }
    | VOLATILE  { VOLATILE }
    
visibility:
    | PUBLIC     { Public }
    | PROTECTED  { Protected }
    | PRIVATE    { Private }

block:
    | OPEN_CURL b=blockStatements CLOSE_CURL    {b}

blockStatements:
    | s=statement b=blockStatements  {match b with Block l -> Block (s::l) }
    | s=statement   { Block [s] } 
statement:
        | SEMICOLON { EmptyStatement }
        | e=expression SEMICOLON    {Expression e}
        | IF OPEN_PAR e=expression CLOSE_PAR s=statement    { IfThen(e,s) }
        | IF OPEN_PAR e=expression CLOSE_PAR s1=statement ELSE s2=statement    { IfThenElse(e,s1,s2) }
        | ASSERT e=expression SEMICOLON { Assert e }
(*        | SWITCH OPEN_PAR e=expression CLOSE_PAR s=switchblock  { Siwtch(e,s) } plus tard*)
        | WHILE OPEN_PAR e=expression CLOSE_PAR s=statement { While(e,s) }
        | DO s=statement WHILE OPEN_PAR e=expression CLOSE_PAR SEMICOLON  {Do(s,e) }
(*      | FOR ... later *)
        | BREAK SEMICOLON { Break }
        | CONTINUE SEMICOLON { Continue }
(* et plus encore plus tard *)
        | b=block   { b }

expression:
    | p=primaryExpression   {PrimaryExpression p}

primaryExpression:
    | l=literal {Literal l}
    | i=IDENT DOT CLASS {ClassLiteral i}
    | THIS  {This}
    | OPEN_PAR e=expression CLOSE_PAR   {ParExpr e}
    | NEW i=IDENT OPEN_PAR a=separated_list(COMMA,expression) CLOSE_PAR  { ClassInstanceCreation(i,a) } (* manque possiblement des morceaux *)
    | NEW i=IDENT d=dims    {ArrayInstanceCreation(i,d)} (* manque des formes *)
    | fa=separated_nonempty_list(DOT, IDENT)    {FieldAccess fa}
    | i=IDENT OPEN_PAR l=separated_list(COMMA, expression) CLOSE_PAR {MethodInvocation(i,l)}
    | i=IDENT OPEN_BRAC e=expression CLOSE_BRAC {ArrayAccess(i,e)}

literal:
    | i=INTEGER {Integer i}
    | r=REAL    {Real r}
    | TRUE      {Bool true}
    | FALSE     {Bool false}
    | qc=QUOTED_CHAR    {String qc}
    | qs=QUOTED_STRING  {Char qs}
(*    | n=NULL    {Null}  still needs to be added to lexer *)

dims:
    | OPEN_BRAC CLOSE_BRAC d=dims {Dim::d}
    | OPEN_BRAC e=expression CLOSE_BRAC d=dims {(DimExpr e)::d}
    | OPEN_BRAC CLOSE_BRAC  {[Dim]}
    | OPEN_BRAC e=expression CLOSE_BRAC {[DimExpr e]}


javaType:
    | t=primitiveType { PrimitiveType(t) }
    | t=referenceType { ReferenceType(t) }

primitiveType:
    | t=numericType { NumericType(t) }
    | BOOLEAN { BOOL }
    
numericType:
    | t=integralType { IntegralType(t) }
    | t=floatingPointType { FloatingPointType(t) }
    
integralType:
    | BYTE  { BYTE }
    | SHORT { SHORT }
    | INT   { INT }
    | LONG  { LONG }
    | CHAR  { CHAR }

floatingPointType:
    | DOUBLE    { DOUBLE }
    | FLOAT     { FLOAT }

referenceType:
    | t=classOrInterfaceType    { ClassOrInterfaceType(t) }
    | t=typeVariable            { TypeVariable(t) }
    | t=arrayType               { ArrayType(t) }
    
arrayType:
    | t=javaType OPEN_BRAC CLOSE_BRAC { t }

typeVariable:
    | t=IDENT   { t }
    
classOrInterfaceType:
    | t=classType   { ClassType(t) }
    | t=interfaceType   { InterfaceType(t) }
    
classType:
    | t=IDENT   { t }

interfaceType:
    | t=IDENT   { t }

%%
