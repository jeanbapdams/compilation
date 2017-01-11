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

