type ident = string

(* définition des types Java *)

type javaType = 
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
arrayType = javaType (*TODO brac*)

and
typeVariable = string

and
classOrInterfaceType = ClassType of classType | InterfaceType of interfaceType
and
classType = string
and
interfaceType = string

(* problème de compréhension de ce qu'est un classOrInterfaceType *)

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


type fieldModifier = STATIC | FINAL | TRANSIENT | VOLATILE
type variableInitializer = string (* more later maybe *)
type variableDeclarator = ModTypeId of fieldModifier list*javaType*ident | ModTypeIdInit of fieldModifier list*javaType * ident * variableInitializer

type methodDeclaration = {
	methodModifiers : methodModifier list;
	(*typeParameters : javaType list;*) (* cause des conflits shift/reduce avec resultType*)
	resultType : resultType;
	methodDeclarator : methodDeclarator;
	methodBody : methodBody
	}
and 
methodModifier = PUBLIC | PROTECTED | PRIVATE | ABSTRACT | STATIC | FINAL | SYNCHRONIZED | NATIVE | STRICTFP

and
resultType = TYPE of javaType | VOID

and 
methodDeclarator = ident * ((javaType * ident) list)

and 
methodBody = statement

	


type visibility = Public | Protected | Private


type normalClassDeclaration = {
	visibilityModifier: visibility; (*public by default*)
	classIdentifier: ident;
	typeParameters: javaType list;
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
    | MethodDeclaration of methodDeclaration
    (* | ClassDeclaration of normal_class
    | InterfaceDeclaration of interface*)
   


type classOrInterfaceDeclaration = normalClassDeclaration

type import = ident list

type package = Package of ident list | NoPackage

type program = {
	packageName: package;	
	importsList: import list;
	classOrInterface: classOrInterfaceDeclaration}





