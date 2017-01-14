type ident = string
type fieldType = string (* more later maybe *)

type fieldModifier = STATIC | FINAL | TRANSIENT | VOLATILE
type variableInitializer = string (* more later maybe *)
type variableDeclarator = ModTypeId of fieldModifier list*fieldType*ident | ModTypeIdInit of fieldModifier list*fieldType * ident * variableInitializer

type visibility = Public | Protected | Private

type normal_class = {
	visibilityModifier: visibility; (*public by default*)
	classIdentifier: string;
	(*typeParameters: typeParameter list; *)
	super: string; (*normal_class;*) (* extends *)
	interfaces: string list; (*interface list;*) 
	classBody: string (*list*) }



type class_or_interface = normal_class

type import = string list

type package = Package of string list | NoPackage

type program = {
	package_name: package;	
	imports_list: import list;
	class_or_interface: class_or_interface}

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
