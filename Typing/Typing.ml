open Parser
open Type


(* How does this make any sense ? Typing a type ? Was used for verbosity purpose, will be removed before the typer is over. *)
let rec process_t t =
    match t with
    | Void -> print_string "Void\n"
    | Array(t,i) -> process_t t; print_int i;
    | Primitive p -> print_string ("primitive "^(Type.stringOf t)^"\n");
    | Ref r -> print_string ("ref "^(Type.stringOf t)^"\n");;

(*the big stuff, adding the type information to the expressions*)
let rec process_expression exp = 
    match exp with {AST.edesc; AST.etype} ->
        match etype with
        | Some etype -> print_string ("Expression of type: "^(stringOf etype)^"\n"); exp;
        | None -> print_string "Etype not defined yet\n";
        (
        match edesc with
            | New(Some name,id,params) -> print_string ("new "^name^": "^(AST.string_of_expression_desc edesc)); exp;
            | NewArray(_,_,_) -> print_string "newarray todo\n"; exp;
            | Call(o,id,args) -> print_string ("call "^id^" todo\n"); exp;
            | Attr(expr,id) -> process_expression expr; print_string ("."^id^"\n"); exp;
            | If (condition, iftrue, iffalse) -> print_string ("if\n"); {edesc=AST.If((process_expression condition), (process_expression iftrue), (process_expression iffalse));etype=None};
            | Val value -> print_string ("value: "^(AST.string_of_value value)^"\n"); exp;
            | Name name -> print_string ("name: "^name^"\n"); exp;
            | Post(exp,op) -> process_expression exp; print_string ((AST.string_of_postfix_op op)^"\n"); exp;
            | Pre(op,exp) -> print_string ((AST.string_of_prefix_op op)^"\n"); process_expression exp;
            | Op(exp1,op,exp2) -> process_expression exp1; print_string ((AST.string_of_infix_op op)^"\n"); process_expression exp2;
            | Cast (t,exp) -> print_string ("InstanceOf: "^(Type.stringOf t)); process_expression exp;
            | Type t -> print_string ("type: "^(Type.stringOf t)); exp;
            | ClassOf t -> print_string ("ClassOf: "^(Type.stringOf t)); exp;
            | Instanceof (exp,t) -> print_string ("InstanceOf: "^(Type.stringOf t)); process_expression exp;
            | _ -> print_string "expression todo\n"; exp;
        )
        ;;

let rec process_statement statement =
    match statement with
    | AST.Block statements -> print_string "start block\n"; let r = AST.Block(List.map process_statement statements) in print_string "end block\n"; r;
    | Nop -> print_string "nope\n"; statement;
    | While(exp,statement) -> print_string "while\n"; let r = AST.While(process_expression exp, process_statement statement) in print_string "end while\n"; r;
    | For(_,_,_,_) -> print_string "for todo\n"; print_string "end for\n"; statement;
    | If(cond,iftrue,iffalse) -> print_string "if\n"; let r = AST.If(process_expression cond, process_statement iftrue, (match iffalse with None -> None | Some statement -> Some (process_statement statement))) in print_string "end if\n"; r;
    | Return exp -> (match exp with None -> print_string "return void\n"; AST.Return None | Some exp -> print_string "return\n"; let r = AST.Return( Some (process_expression exp)) in print_string "end return\n"; r);
    | AST.Expr expr -> Expr (process_expression expr);
    | _ -> print_string "statement todo\n"; statement;


;;

let rec process_astattributes astattributes =
    match astattributes with 
    | [] -> [];
    | {AST.amodifiers;aname;atype;adefault}::l ->
        print_string ("modifiers: "^(ListII.concat_map " " AST.stringOf_modifier amodifiers)^"\n");
        print_string ("name: "^aname^"\n");
        process_t atype;
        let def=
        (
        match adefault with
        | None -> print_string "No default\n"; None;
        | Some exp -> Some (process_expression exp);
        ) in
        {AST.amodifiers;aname;atype;adefault=def}::(process_astattributes l);;
        
let rec process_arguments arguments =
    match arguments with
    | [] -> ()
    | {AST.final;vararg;ptype;pident}::l ->
        process_t ptype;
        process_arguments l;;

let rec process_astmethods astmethods =
    match astmethods with
    | [] -> [];
    | {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody}::l ->
        print_string ("modifiers: "^(ListII.concat_map " " AST.stringOf_modifier mmodifiers)^"\n");
        print_string ("name: "^mname^"\n");
        process_t mreturntype;
        process_arguments margstype;
        let b = (List.map process_statement mbody);
        in
        {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody=b}::(process_astmethods l);;

let process_astclass ac =
    match ac with {AST.cparent; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} ->
        print_string "\nattributes\n";
        process_astattributes cattributes;
        print_string "\nmethods\n";
        {AST.cparent;cattributes; cinits; cconsts; cmethods=(process_astmethods cmethods); ctypes; cloc};;

let rec process_type_list l =
    match l with
    | [] -> []
    | {AST.modifiers; AST.id; AST.info}::l ->
        print_string ( "modifiers: "^(ListII.concat_map " " AST.stringOf_modifier modifiers)^"\n");
        print_string ("id: "^id^"\n");
        let i = 
        (
            match info with
                | Class astclass -> AST.Class (process_astclass astclass);
                | Inter -> print_string "Inter\n"; Inter;
        ) in
        {AST.modifiers;id;info=i}::(process_type_list l);;

