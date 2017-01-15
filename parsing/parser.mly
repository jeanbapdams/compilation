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
    | f=fieldDeclaration    { LocalVariableDeclaration f }
    | e=expression SEMICOLON    {Expression e}
    | IF OPEN_PAR e=expression CLOSE_PAR s=statement    { IfThen(e,s) }
    | IF OPEN_PAR e=expression CLOSE_PAR s1=statement ELSE s2=statement    { IfThenElse(e,s1,s2) }
    | ASSERT e=expression SEMICOLON { Assert e }
(*        | SWITCH OPEN_PAR e=expression CLOSE_PAR s=switchblock  { Siwtch(e,s) } plus tard*)
    | WHILE OPEN_PAR e=expression CLOSE_PAR s=statement { While(e,s) }
    | DO s=statement WHILE OPEN_PAR e=expression CLOSE_PAR SEMICOLON  {Do(s,e) }
    | FOR OPEN_PAR f=fieldDeclaration SEMICOLON e2=expression SEMICOLON e3=expression CLOSE_PAR s=statement   { For(f,e2,e3,s) }
    | BREAK SEMICOLON { Break }
    | CONTINUE SEMICOLON    { Continue }
    | RETURN SEMICOLON  { ReturnVoid }
    | RETURN e=expression   { Return e }
(* et plus encore plus tard *)
    | b=block   { b }


expression:
    | a=assignmentExpression    { AssignmentExpression a }

primaryExpression:
    | l=literal {Literal l}
    | i=javaType DOT CLASS {ClassLiteral i}
    | THIS  {This}
    | OPEN_PAR e=expression CLOSE_PAR   {ParExpr e}
    | NEW i=javaType OPEN_PAR a=separated_list(COMMA,expression) CLOSE_PAR  { ClassInstanceCreation(i,a) } (* manque possiblement des morceaux *)
    | NEW i=javaType d=dims    {ArrayInstanceCreation(i,d)} (* manque des formes *)
    | fa=fieldAccess {FieldAccess fa}
    | mi=methodInvocation   { MethodInvocation mi }
    | ar=arrayAccess    { ArrayAccess ar }

literal:
    | i=INTEGER {IntegerLiteral i}
    | r=REAL    {RealLiteral r}
    | TRUE      {BoolLiteral true}
    | FALSE     {BoolLiteral false}
    | qc=QUOTED_CHAR    {StringLiteral qc}
    | qs=QUOTED_STRING  {CharLiteral qs}
(*    | n=NULL    {Null}  still needs to be added to lexer *)

dims:
    | OPEN_BRAC CLOSE_BRAC d=dims {Dim::d}
    | OPEN_BRAC e=expression CLOSE_BRAC d=dims {(DimExpr e)::d}
    | OPEN_BRAC CLOSE_BRAC  {[Dim]}
    | OPEN_BRAC e=expression CLOSE_BRAC {[DimExpr e]}

fieldAccess:
    | p=primaryExpression DOT i=IDENT   { ExprAcc(p,i) }
    | SUPER DOT i=IDENT {SuperAcc i}
    | i1=javaType DOT SUPER DOT i2=IDENT   { ClassSuperAcc(i1,i2) }

arrayAccess:
    | e1=expression OPEN_BRAC e2=expression CLOSE_BRAC {(e1,e2)}

methodInvocation:
    | i=IDENT OPEN_PAR l=separated_list(COMMA, expression) CLOSE_PAR { Invoc(i,l) }
    | p=primaryExpression DOT i=IDENT OPEN_PAR l=separated_list(COMMA, expression) CLOSE_PAR { ExprInvoc(p,i,l) }
    | SUPER DOT i=IDENT OPEN_PAR l=separated_list(COMMA, expression) CLOSE_PAR { SuperInvoc(i,l) }
    | i1=javaType DOT SUPER DOT i2=IDENT OPEN_PAR l=separated_list(COMMA, expression) CLOSE_PAR { ClassSuperInvoc(i1,i2,l) }
    | i1=javaType DOT i2=IDENT OPEN_PAR l=separated_list(COMMA, expression) CLOSE_PAR { TypeInvoc(i1,i2,l) }

postfixExpression:
    | i=IDENT   { ExpressionName i }
    | p=primaryExpression { PostfixPrimaryExpression p }
    | p=postfixExpression INC { PostIncExpr p }
    | p=postfixExpression DEC { PostDecExpr p }

unaryExpression:
    | INC u=unaryExpression { PreIncExpr u }
    | DEC u=unaryExpression { PreDecExpr u }
    | PLUS u=unaryExpression    { PlusExpr u }
    | MINUS u=unaryExpression   { MinusExpr u }
    | TILDE u=unaryExpression   { TildeExpr u }
    | EXCLAMATION u=unaryExpression { ExclamationExpr u }
    | pf=postfixExpression  { UnaryPostfixExpression pf }

multiplicativeExpression:
    | u=unaryExpression {MultiplicativeUnaryExpression u}
    | m=multiplicativeExpression MUL u=unaryExpression  { Mul(m,u) }
    | m=multiplicativeExpression DIV u=unaryExpression  { Div(m,u) }
    | m=multiplicativeExpression MOD u=unaryExpression  { Mod(m,u) }

additiveExpression:
    | m=multiplicativeExpression    { AdditiveMultiplicativeExpression m }
    | a=additiveExpression PLUS m=multiplicativeExpression  { Plus(a,m) }
    | a=additiveExpression MINUS m=multiplicativeExpression  { Minus(a,m) }

shiftExpression:
    | a=additiveExpression  { ShiftAdditiveExpression a }
    | s=shiftExpression DOUBLECHEVRONLEFT a=additiveExpression  { ShiftTwoLeft(s,a) }
    | s=shiftExpression DOUBLECHEVRONRIGHT a=additiveExpression  { ShiftTwoRight(s,a) }
    | s=shiftExpression TRIPLECHEVRONLEFT a=additiveExpression  { ShiftThreeLeft(s,a) }
    | s=shiftExpression TRIPLECHEVRONRIGHT a=additiveExpression  { ShiftThreeRight(s,a) }

relationalExpression:
    | s=shiftExpression { RelationalShiftExpression s }
    | r=relationalExpression SMALLER s=shiftExpression  { Smaller(r,s) }
    | r=relationalExpression GREATER s=shiftExpression  { Greater(r,s) }
    | r=relationalExpression SMALLEROREQUAL s=shiftExpression   { SmallerOrEqual(r,s) }
    | r=relationalExpression GREATEROREQUAL s=shiftExpression   { GreaterOrEqual(r,s) }
    | r=relationalExpression INSTANCEOF i=javaType { InstanceOf(r,i) }

equalityExpression:
    | r=relationalExpression    { EqualityRelationalExpression r }
    | e=equalityExpression DOUBLEEQUAL r=relationalExpression    { Equal(e,r) }
    | e=equalityExpression DIFFERENT r=relationalExpression { Equal(e,r) }


(* plusieurs morceaux séparés dans la spec' sont ici entassé pour gagner du temps *)
logicalExpression:
    | e=equalityExpression  { LogicalEqualityExpression e }
    | l1=logicalExpression AND l2=logicalExpression { And(l1,l2) }
    | l1=logicalExpression POWER l2=logicalExpression { ExcOr(l1,l2) }
    | l1=logicalExpression OR l2=logicalExpression { IncOr(l1,l2) }
    | l1=logicalExpression DOUBLEAND l2=logicalExpression { DoubleAnd(l1,l2) }
    | l=logicalExpression QUESTION e1=expression COLON e2=expression    { Conditional(l,e1,e2) }

assignmentExpression:
    | l=logicalExpression   { AssignmentLogicalExpression l }
    | i=assignmentLeftHand EQUAL e=assignmentExpression    { Assignment(i,EQUAL,e) }
    | i=assignmentLeftHand MULEQUAL e=assignmentExpression    { Assignment(i,MULEQUAL,e) }
    | i=assignmentLeftHand DIVEQUAL e=assignmentExpression    { Assignment(i,DIVEQUAL,e) }
    | i=assignmentLeftHand MODEQUAL e=assignmentExpression    { Assignment(i,MODEQUAL,e) }
    | i=assignmentLeftHand PLUSEQUAL e=assignmentExpression    { Assignment(i,PLUSEQUAL,e) }
    | i=assignmentLeftHand MINUSEQUAL e=assignmentExpression    { Assignment(i,MINUSEQUAL,e) }
    | i=assignmentLeftHand DOUBLECHEVRONLEFTEQUAL e=assignmentExpression    { Assignment(i,DOUBLECHEVRONLEFTEQUAL,e) }
    | i=assignmentLeftHand TRIPLECHEVRONLEFTEQUAL e=assignmentExpression    { Assignment(i,TRIPLECHEVRONLEFTEQUAL,e) }
    | i=assignmentLeftHand DOUBLECHEVRONRIGHTEQUAL e=assignmentExpression    { Assignment(i,DOUBLECHEVRONRIGHTEQUAL,e) }
    | i=assignmentLeftHand TRIPLECHEVRONRIGHTEQUAL e=assignmentExpression    { Assignment(i,TRIPLECHEVRONRIGHTEQUAL,e) }

assignmentLeftHand:
    | i=IDENT   { LeftHandExpressionName i }
    | fa=fieldAccess    { LeftHandFieldAccess fa }
    | ar=arrayAccess    { LeftHandArrayAccess ar }


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
