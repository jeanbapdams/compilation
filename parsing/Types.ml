type ident = string

(* définition des types Java *)

type java_type = 
	| PrimitiveType of primitiveType
	| ReferenceType of referenceType

and
primitiveType =
	| NumericType of numericType
	| BOOL

and
numericType = 
	| IntegralType of integralType
	| FloatingPointType of floatingPointType

and
integralType = BYTE | SHORT | INT | LONG | CHAR

and
floatingPointType = FLOAT | DOUBLE


and
referenceType =
	| ClassOrInterfaceType of classOrInterfaceType
	| TypeVariable of typeVariable
	| ArrayType of arrayType


and
arrayType = java_type (*TODO brac*)

and
typeVariable = string

and
classOrInterfaceType = ClassType of classType | InterfaceType of interfaceType
and
classType = string
and
interfaceType = string

(* problème de compréhension de ce qu'est un classOrInterfaceType *)


type fieldModifier = STATIC | FINAL | TRANSIENT | VOLATILE
type variableInitializer = string (* more later maybe *)
type variableDeclarator = ModTypeId of fieldModifier list*java_type*ident | ModTypeIdInit of fieldModifier list*java_type * ident * variableInitializer

type visibility = Public | Protected | Private


type normalClassDeclaration = {
	visibilityModifier: visibility; (*public by default*)
	classIdentifier: ident;
	typeParameters: java_type list;
	super: classType; (*normal_class;*) (* extends *)
	interfaces: interfaceType list; (*interface list;*) 
	classBody: classBodyDeclaration list }
and
classBodyDeclaration = 
    | ClassMemberDeclaration of classMemberDeclaration
    (*| InstanceInitializer of instanceInitializer
    | StaticInitializer of staticInitializer
    | ConstructorDeclaration of constructorDeclaration*)
and
classMemberDeclaration =
    | FieldDeclaration of variableDeclarator
    (*| MethodDeclaration of methodDeclaration
    | ClassDeclaration of normal_class
    | InterfaceDeclaration of interface*)
    


type classOrInterfaceDeclaration = normalClassDeclaration

type import = ident list

type package = Package of ident list | NoPackage

type program = {
	packageName: package;	
	importsList: import list;
	classOrInterface: classOrInterfaceDeclaration}

type literal =
    | Integer of string
    | Real of string
    | Bool of bool
    | String of string
    | Char of string


(* pb de récursion : expression a besoin de primaryExpression qui a besoin de dims qui a besoin de expression ; d'ou le "and" *)
type expression = 
    | Expression of string
    | PrimaryExpression of primaryExpression
and
dim =
    | Dim
    | DimExpr of expression

and
primaryExpression =
    | Literal of literal
    | ClassLiteral of string
    | This
    | ParExpr of expression
    | ClassInstanceCreation of ident * expression list
    | ArrayInstanceCreation of ident * dim list
    | FieldAccess of ident list
    | MethodInvocation of ident * expression list
    | ArrayAccess of ident * expression


type statement =
    | EmptyStatement
    | Expression of expression
    | IfThen of expression * statement
    | IfThenElse of expression * statement * statement
    | Assert of expression
    | Switch of expression * statement list
    | While of expression * statement
    | Do of statement * expression
(*  | FOR of ... *)
    | Break
    | Continue
    | Block of statement list




