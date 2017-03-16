open Parser
open Type

exception NonBooleanCondition;;
exception BadTypeDefaultValue;;
exception IncoherentTypes;;

(* How does this make any sense ? Typing a type ? Was used for verbosity purpose, will be removed before the typer is over. *)
let rec process_t t =
    match t with
    | Void -> print_string "Void\n"
    | Array(t,i) -> print_string "array\ntype: ";process_t t; print_string "dim: ";print_int i;print_string "\n";
    | Primitive p -> print_string ("primitive "^(Type.stringOf t)^"\n");
    | Ref r -> print_string ("ref "^(Type.stringOf t)^"\n");;

(*the big stuff, adding the type information to the expressions*)
let rec process_expression exp:AST.expression = 
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

            |  CondOp(condition, iftrue, iffalse) -> 
                    print_string ("condop\n");
                    let r =  AST.CondOp((process_expression condition), (process_expression iftrue), (process_expression iffalse)) in
                    (
                    match r with CondOp({edesc=_;etype=etypecond}, {edesc=_;etype=etypetrue}, {edesc=_;etype=etypefalse}) ->
                        if etypecond=Some (Primitive Boolean)
                        then print_string "condition is boolean, ok\n"
                        else begin 
                            print_string "condition is NOT boolean, NOT OK\n"; 
                            raise NonBooleanCondition;
                        end;
                        if etypetrue=etypefalse
                        then print_string "true and false case have same type, ok\n"
                        else begin
                            print_string "true and false case have NOT same type, NOT ok\n"; 
                            raise IncoherentTypes;
                        end
                        print_string "end condop\n";
                        {edesc=r; etype=etypetrue};
                    );

            (* This is put on hold as there seems to be interferences between the two definitions of If as
 * If of expression * statement * statement option
 * which means if truc then machin else chose
 * and
 * If of expression * expression * expression
 * which means truc?machin:chose
 * (I think)
 * First case is a statement, second case is an expression
 * here despite  using :AST.expression, ocaml keep thinking that If(...) is a stament, when it should be an expression
 * turns out, I was mistaking If for CondOp, but it does not solve the If problem if it arrises later
 * also I don't know what the If is supposed to be as an expression, I wonder if it is not actually used, was replaced by condop and forgotten ... poor If expression
 * for the record, the error was on the following :
 * AST.If((process_expression condition), (process_expression iftrue), (process_expression iffalse))
 *)

            | If(_,_,_) -> exp;

            | Val value -> print_string ("value: "^(AST.string_of_value value)^"\n");
            (
                print_string "++defining value type\n";
                match value with
                | String _ -> print_string "String todo\n"; exp;
                | Int _ -> {edesc;etype=Some (Primitive Int)};
                | Float _ -> {edesc;etype=Some (Primitive Float)};
                | Char _ -> {edesc;etype=Some (Primitive Char)};
                | Boolean _ -> {edesc;etype=Some (Primitive Boolean)};
                | Null _ -> print_string "String todo\n"; exp;
            );
            | Name name -> print_string ("name: "^name^"\n"); exp;
            | Post(exp,op) -> 
                    let r = process_expression exp in
                    (
                    match r with
                    | {edesc;etype=Some t} ->
                            if (t=Primitive Short || t=Primitive Int || t=Primitive Long || t=Primitive Float || t=Primitive Double || t=Primitive Char ||t=Primitive Byte)
                            then print_string ("sure you can "^(AST.string_of_postfix_op op)^" a "^(stringOf t)^".\n")
                            else (print_string ("are you sure that you can "^(AST.string_of_postfix_op op)^" a "^(stringOf t)^" ? I'm not\n"); raise IncoherentTypes);
                            {AST.edesc=Post(r,op);etype=Some t};
                    | {edesc;etype=None} -> 
                            print_string "troubles\n";
                            {AST.edesc=Post(r,op);etype=None};
                    );
            | Pre(op,exp) -> 
                    let r = process_expression exp in
                    (
                    match r with {edesc;etype} -> {AST.edesc=Pre(op,r);etype}; (* needs more verbosity and verifications as in previous case ... *)
                    );
            | Op(exp1,op,exp2) -> 
                    let r1 = process_expression exp1 and r2 = process_expression exp2 in
                    (
                        match r1,r2 with 
                        | {edesc=_;etype=Some t1},{edesc=_;etype=Some t2} ->
                            if (t1=t2)
                            then ()
                            else (print_string ((Type.stringOf t1)^(AST.string_of_infix_op op)^(Type.stringOf t2)^" not allowed\n"); raise IncoherentTypes);
                            (* once again, more cases needed, float+int is allowed and is a float *)
                            {edesc=AST.Op(r1,op,r2);etype=Some t1}
                        | _ -> 
                                print_string "troubles\n"; 
                                {edesc=AST.Op(r1,op,r2);etype=None};
                    );
            | Cast (t,exp) ->
                    (
                    match (process_expression exp) with {edesc;etype = Some etype} ->
                        print_string ("casting from "^(stringOf etype)^" to "^(stringOf t)^" and I don't care if it makes any sense\n");
                        {edesc;etype=Some t};
                    );
            (* not sure about this one *)
            | Type t -> print_string ("This expression is a type, I don't know what is the type of this type: "^(Type.stringOf t)); exp;
            (* not sure about the following two either ...*)
            | ClassOf t -> print_string ("ClassOf: "^(Type.stringOf t)); exp;
            | Instanceof (exp,t) -> print_string ("InstanceOf: "^(Type.stringOf t)); process_expression exp;
            | _ -> print_string "expression todo\n"; print_string (AST.string_of_expression_desc edesc); print_string "\nend todo\n";  exp;
        )
        ;;

let rec process_statement statement =
    match statement with
    | AST.Block statements ->
            print_string "start block\n";
            let r = AST.Block(List.map process_statement statements) in
            print_string "end block\n";
            r;
    | Nop -> print_string "nope\n"; statement;
    | While(exp,statement) -> 
            print_string "while\n";
            let r = AST.While(process_expression exp, process_statement statement) in
            (
                match r with AST.While({edesc;etype},_) ->
                    if etype=Some (Primitive Boolean)
                    then print_string "condition is boolean OK\n"
                    else (print_string "condtion is not boolean NOT OK\n"; raise NonBooleanCondition)
                    print_string "end while\n";
            ); r;
    | For(truc1,truc2,truc3,statement) ->
            print_string "for need some more work\n";
            let r = AST.For(truc1,truc2,truc3, process_statement statement) in
            print_string "end for\n";
            r;
    | If(cond,iftrue,iffalse) -> 
            print_string "if\n";
            let r = AST.If(process_expression cond, process_statement iftrue, (match iffalse with None -> None | Some statement -> Some (process_statement statement))) in
            (
                match r with 
                    | AST.If({edesc;etype},_,_) ->
                        if etype=Some (Primitive Boolean)
                        then print_string "condition is boolean OK\n"
                        else (print_string "condtion is not boolean NOT OK\n"; raise NonBooleanCondition);
            );
            print_string "end if\n";
            r;
    | Return exp ->
            (
                match exp with 
                | None -> print_string "return void\n"; AST.Return None;
                | Some exp -> print_string "return\n"; let r = AST.Return( Some (process_expression exp)) in print_string "end return\n"; r;
            );
    | AST.Expr expr -> Expr(process_expression expr);
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
        ( 
            match def with Some {AST.edesc; AST.etype} ->
                match etype with 
                | None -> 
                        print_string "Default not typed\n" ;
                | Some t -> 
                        print_string ("default typed as "^(stringOf t)^"\n");
                        if atype=t 
                        then print_string "init value type correct\n"
                        else (print_string ("wrong init value type ! "^(stringOf atype)^" vs "^(stringOf t)^"\n"); raise BadTypeDefaultValue;)
        );
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
        print_string "return type: "; process_t mreturntype;
        print_string "args: "; process_arguments margstype;
        print_string "body: \n";
        let b = (List.map process_statement mbody);
        in
        {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody=b}::(process_astmethods l);;

let process_astclass ac =
    match ac with {AST.cparent; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} ->
        print_string "\nattributes\n";
        let tribute = process_astattributes cattributes in
        print_string "\nmethods\n";
        {AST.cparent;cattributes=tribute; cinits; cconsts; cmethods=(process_astmethods cmethods); ctypes; cloc};;

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

