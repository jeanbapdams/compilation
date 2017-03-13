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
    match exp with {AST.edesc} ->
        match edesc with
        | New(Some name,id,params) -> print_string ("new "^name^": "^(AST.string_of_expression_desc edesc));
        | NewArray(_,_,_) -> print_string "newarray todo\n";
        | Call(o,id,args) -> print_string ("call "^id^" todo\n");
        | Attr(expr,id) -> process_expression expr; print_string ("."^id^"\n");
        | If (condition, iftrue, iffalse) -> print_string ("if\n"); process_expression condition; process_expression iftrue; process_expression iffalse;
        | Val value -> print_string ("value: "^(AST.string_of_value value)^"\n");
        | Name name -> print_string ("name: "^name^"\n");
        | Post(exp,op) -> process_expression exp; print_string ((AST.string_of_postfix_op op)^"\n");
        | Pre(op,exp) -> print_string ((AST.string_of_prefix_op op)^"\n"); process_expression exp;
        | Op(exp1,op,exp2) -> process_expression exp1; print_string ((AST.string_of_infix_op op)^"\n"); process_expression exp2;
        | Cast (t,exp) -> print_string ("InstanceOf: "^(Type.stringOf t)); process_expression exp;
        | Type t -> print_string ("type: "^(Type.stringOf t));
        | ClassOf t -> print_string ("ClassOf: "^(Type.stringOf t));
        | Instanceof (exp,t) -> print_string ("InstanceOf: "^(Type.stringOf t)); process_expression exp;
        | _ -> print_string "expression todo\n";;

let rec process_statement statement =
    match statement with
    | AST.Block statements -> print_string "start block\n"; List.iter process_statement statements; print_string "end block\n";
    | Nop -> print_string "nope\n"
    | While(exp,statement) -> print_string "while\n"; process_expression exp; print_string "do\n"; process_statement statement; print_string "end do\n";
    | For(_,_,_,statement) -> print_string "for todo\n"; process_statement statement; print_string "end for\n";
    | If(cond,iftrue,iffalse) -> print_string "if\n"; process_expression cond; "then\n"; process_statement iftrue; print_string "else\n"; (match iffalse with None -> print_string "nothing\n" | Some statement -> process_statement statement); print_string "end if\n";
    | Return exp -> (match exp with None -> print_string "return void\n" | Some exp -> print_string "return\n"; process_expression exp; print_string "end return\n");
    | AST.Expr expr -> process_expression expr
    | _ -> print_string "statement todo\n";


;;

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
        List.iter process_statement mbody;
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

