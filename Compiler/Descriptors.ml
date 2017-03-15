type method_ident = string
type class_ident = string

type attribute_descriptor = {
      mutable amodifiers : modifier list;
      aname : string;
      atype : Type.t;
      adefault : expression option;
    }

type method_descriptor = {
    mutable mmodifiers : modifier list;
    mname : string;
    mreturntype : Type.t;
    margstype : argument list;
    mthrows : Type.ref_type list;
    mbody : statement list;}

type method_table = method_descriptor list

type object_descriptor = {
    ident : string;
    attributes_list : attribute_descriptor list}

type class_descriptor = {
    ident : string;
    method_list : (class_ident, method_ident) list}
    
    (*todo : static attributes*)
    
type stack_element = CLASS of class_descriptor | METHOD_CALL of method_ident

type heap_element = string (*TODO : a real thing*)
