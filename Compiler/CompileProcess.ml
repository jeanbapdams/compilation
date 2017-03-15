open Descriptors

(* asttype -> (asttype list) -> (stack_element list) -> method_table -> (object_descriptor list) -> ((stack_element list) * method_table * (object_descriptor list)) *)
let rec compile class_to_compile tast stack table objects = match class_to_compile, stack with
                                | {_; AST.id; {{_; AST.tid}; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} }, [] -> (* mother class not compiled yet *)
                                        begin let rec mother_class_search tast, id_class_to_find = match tast with
                                            | [] -> failwith "No such mother class"
                                            | {AST.modifiers; AST.id; AST.info}::t when id != id_class_to_find -> mother_class_search t id_class_to_find
                                            | {AST.modifiers; AST.id; AST.info}::t when id == id_class_to_find -> {AST.modifiers; AST.id; AST.info}
                                            
                                            in 
                                            compile (mother_class_search tast, tid) tast stack table objects(*mother class compilation *)
                                            compile class_to_compile tast stack table objects
                                             end
                                             
                                             
                                | {_; AST.id; {{_; AST.tid}; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} }, CLASS({parent_id,_})::t when tid != parent_id -> (*mother class perhaps compiled*) compile class_to_compile tast t 
                                
                                | {_; AST.id; {{_; AST.tid}; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} }, CLASS({parent_id, parent_methods})::t when tid == parent_id -> (* mother class already compiled *)
                                            let rec create_method_list_for_class_descriptor id_daughter daughter_methods result = match daughter_methods with
                                                    | [] -> result
                                                    | {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody}::t -> begin
                                                        
                                                        let rec insert_in_class_list id_d mname list_ = match list_ with
                                                        | [] -> (id_d, mname)::[]
                                                        | (_, method_id)::t when method_id == mname -> (id_d ^ mname)::t
                                                        | (id_m, method_id)::t when method_id != mname -> (id_m, method_id)::(insert_without_doubles id_d mname t)
                                                        in insert_in_class_list id_daughter mname result
                                                        end                                                    
                                            and
                                            let rec create_object_descriptor id id_mother objects = 
                                                    (*TODO *)
                                            
                                            
                                                
                                                in (CLASS({ident = id; method_list = create_method_list_for_class_descriptor id cmethods parent_methods})::stack, cmethods@table, create_object_descriptor id parent_id objects)
                                            

let create_stack = 
    (* class Object *)
    let object_class = {ident = "Object"; method_list = []}
    

    

let compiler (*tast*) = 
    
    
    (* first we need to allocate particulars classes *)
    
    
