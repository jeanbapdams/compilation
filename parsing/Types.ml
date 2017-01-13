type ident = string
type fieldType = string (* more later maybe *)

type fieldModifier = STATIC | FINAL | TRANSIENT | VOLATILE
type variableInitializer = string (* more later maybe *)
type variableDeclarator = ModTypeId of fieldModifier list*fieldType*ident | ModTypeIdInit of fieldModifier list*fieldType * ident * variableInitializer

type visibility = Public | Protected | Private

type normal_class = string

	(* {
	visibility_modifier: visibility;
	class_identifier: string;
	typeParameters: list typeParameter; 
	super: normal_class; (* extends *)
	interfaces: interface list; 
	classBody: string list } *)

type class_or_interface = normal_class

type import = string list

type package = Package of string list | NoPackage

type program = {
	package_name: package;	
	imports_list: import list;
	class_or_interface: class_or_interface}

type expression = Expression of string

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
