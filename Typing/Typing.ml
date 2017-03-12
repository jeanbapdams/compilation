open Parser
open Type

let rec process_t t =
    match t with
    | Void -> print_string "Void\n"
    | Array(t,i) -> process_t t; print_int i;
    | Primitive p -> print_string ("primitive "^(Type.stringOf t)^"\n");
    | Ref r -> print_string ("ref "^(Type.stringOf t)^"\n");;

(*the big stuff*)
let rec process_expression exp =
    match exp with
    | _ -> print_string "todo"

let rec process_astattributes astattributes =
    match astattributes with 
    | [] -> ();
    | {AST.amodifiers;aname;atype;adefault}::l ->
        print_string ("modifiers: "^(ListII.concat_map " " AST.stringOf_modifier amodifiers)^"\n");
        print_string ("name: "^aname^"\n");
        process_t atype;
        (
        match adefault with
        | None -> print_string "No default\n"
        | Some exp -> process_expression exp;
        );
        process_astattributes l;;
        
let rec process_arguments arguments =
    match arguments with
    | [] -> ()
    | {AST.final;vararg;ptype;pident}::l ->
        process_t ptype;
        process_arguments l;;

let rec process_astmethods astmethods =
    match astmethods with
    | [] -> ();
    | {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody}::l ->
        print_string ("modifiers: "^(ListII.concat_map " " AST.stringOf_modifier mmodifiers)^"\n");
        print_string ("name: "^mname^"\n");
        process_t mreturntype;
        process_arguments margstype;
        process_astmethods l;;

let process_astclass ac =
    match ac with {AST.cparent; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} ->
        print_string "\nattributes\n";
        process_astattributes cattributes;
        print_string "\nmethods\n";
        process_astmethods cmethods;;

let rec process_type_list l =
    match l with
    | [] -> ()
    | {AST.modifiers; AST.id; AST.info}::l ->
        print_string ( "modifiers: "^(ListII.concat_map " " AST.stringOf_modifier modifiers)^"\n");
        print_string ("id: "^id^"\n");
        (
            match info with
                | Class astclass -> process_astclass astclass
                | Inter -> print_string "Inter\n"
        );
        process_type_list l;;

