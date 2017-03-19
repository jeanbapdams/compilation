open Parser
open Type

exception NonBooleanCondition;;
exception BadTypeDefaultValue;;
exception IncoherentTypes;;
exception UnknownMethod;;
exception UnknownAttribute;;

(* registers known functions and variables at any point in the AST *)
type env = {methods: AST.astmethod list ; attributes: AST.astattribute list};;

(* add the list of attributes and methods of a given class to the preexisting env *)
let build_env env ac = 
    let rec aux methods =
        match methods with
        | [] -> []
        | {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody}::l ->
                print_string ("adding method "^mname^" to env\n");
                {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody=[]}::(aux l);
    in 
    let rec aux2 attributes =
        match attributes with
        | [] -> []
        | {AST.amodifiers;aname;atype;adefault}::l ->
                print_string ("adding attribute "^aname^" to the env\n");
                {AST.amodifiers;aname;atype;adefault=None}::(aux2 l);
    in          
    match env,ac with {methods;attributes},{AST.cparent; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} ->
        {methods=(aux cmethods)@methods;attributes=(aux2 cattributes)};;
        
(* the big stuff, adding the type information to the expressions *)
(* if the field etype is Some type, we consider the expression was already typed, not verifications made *)
(* if it is None, we try to fill it with the information of env and of the edesc field *)
(* the edesc field is unmodified *)
(* still very verbose, helps with debugging *)
let rec process_expression env exp:AST.expression = 
    match exp with {AST.edesc; AST.etype} ->
        match etype with
        | Some etype -> print_string ("Expression "^(AST.string_of_expression_desc edesc)^" of type: "^(stringOf etype)^"\n"); exp;
        | None -> print_string ("type of "^(AST.string_of_expression_desc edesc)^"  not defined yet\n");
        (
            match edesc with

            | New(name,id,params) -> 
                    let rec aux l =
                        match l with
                        | [] -> print_string "I don't think this should be happening ...\n"; [],"";
                        | [x] -> [],x;
                        | x::l -> match aux l with tpath,tid -> x::tpath,tid;
                    in
                    let (tpath,tid) = aux id in
                    print_string ("new "^tid^"\n");
                    (
                    match name with
                    | None -> print_string "no name\n";
                    | Some name -> print_string ("name "^name^"\n");
                    );
                    print_string "expressions\n";
                    let e = List.map (process_expression env) params in 
                    print_string "end new\n";
                    {edesc=AST.New (name,id,e);etype=Some (Ref {tpath;tid})};

            | NewArray(t,l,init) ->
                    print_string ("new "^(Type.stringOf t)^" array\n");
                    let aux e = match e with
                    | None -> None;
                    | Some e -> Some (process_expression env e);
                    in
                    let l = List.map aux l in
                    let init = aux init in 
                    {edesc=NewArray(t,l,init);etype=Some (Array(t,List.length l))};

            | Call(o,id,args) ->
                    print_string ("calling "^id^"\n");
                    print_string "evaluating args\n";
                    let args = List.map (process_expression env) args in (* type the arguments *)
                    print_string ("looking up "^id^" in env.methods\n");
                    (* look for a function with the right signature in the env *)
                    let rec aux methods = match methods with
                    | [] -> print_string "I don't think this method is in env\n"; None
                    | m::methods ->
                            if m.AST.mname=id (* right name*)
                            then 
                            (
                                if (List.length args) = (List.length m.margstype) (* check the number of arguments *)
                                then
                                (
                                    (* checks if each argument as the right type compared to the function signature *)
                                    let rec aux2 args argstype =
                                        match args,argstype with
                                        | [],[] -> true;
                                        | {AST.edesc;etype=Some a}::args,t::argstype ->
                                                if a=t.AST.ptype
                                                then aux2 args argstype
                                                else false;
                                        | {AST.edesc;etype=None}::args,t::argstype ->
                                                print_string "Couldn't type one of the argument\n";
                                                false; (* refuse to call a function if we're not sure it's the right argument type, safer but ... *)

                                    in
                                    if (aux2 args m.margstype)
                                    then (print_string "ok this is the right method\n"; Some m;)
                                    else aux methods;
                                )
                                else aux methods;
                                
                            )
                            else aux methods;
                    in 
                    (
                        match aux env.methods with
                        | None -> raise UnknownMethod; (* couldn't find the method in env *)
                        | Some m -> {edesc;etype=Some m.mreturntype}; (* the Call expression as the type of the returned type of the method called *)
                    );

            | Attr(expr,id) -> print_string "attribute "; print_string (id^" of\n"); process_expression env expr; 
            (* this is wrong, we should type expr, and use env (?) to see if this is an object with an attribute named id *)
            (* but the env does not hold enough information for this at the moment, I think (only consider one class at a time *)

            | CondOp(condition, iftrue, iffalse) -> 
                    print_string ("condop\n");
                    let r =  AST.CondOp((process_expression env condition), (process_expression env iftrue), (process_expression env iffalse)) in (* types each of the expressions *)
                    (
                        (* checks that the condition is boolean and both possibles return values have the same type *)
                        match r with CondOp({edesc=_;etype=etypecond}, {edesc=_;etype=etypetrue}, {edesc=_;etype=etypefalse}) ->
                            if etypecond=Some (Primitive Boolean)
                            then ()
                            else begin 
                                print_string "condition is NOT boolean, NOT OK\n"; 
                                raise NonBooleanCondition;
                            end;
                            if etypetrue=etypefalse
                            then ()
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
 *
 * *later*
 * turns out, I was mistaking If expression for CondOp expression, but it does not solve the problem
 * also I don't know what the If is supposed to be as an expression, I wonder if it is not actually used, was replaced by condop and forgotten ... poor If expression
 * for the record, the error was on the following :
 * AST.If((process_expression env condition), (process_expression env iftrue), (process_expression env iffalse))
 *)

            | If(_,_,_) -> print_string "If you see this at runtime, please fix it\n"; exp;

            | Val value -> 
            (
                match value with
                | String _ -> {edesc;etype=Some(Ref({Type.tpath=["Object"];tid="String"}))}; (* not sure if this is how it's supposed to be used *)
                | Int _ -> {edesc;etype=Some (Primitive Int)};
                | Float _ -> {edesc;etype=Some (Primitive Float)};
                | Char _ -> {edesc;etype=Some (Primitive Char)};
                | Boolean _ -> {edesc;etype=Some (Primitive Boolean)};
                | Null _ -> {edesc;etype=Some(Ref({Type.tpath=[];tid="Null"}))}; (* not sure about this either, according to internet Null does not have a type *)
            );

            | Name name -> 
                    print_string ("naming "^name^"\n");
                    print_string ("looking up "^name^" in env.attributes\n");
                    let rec aux attributes = match attributes with
                    | [] -> print_string "I don't think this attribute is in env.\n"; None
                    | a::attributes -> match a with {AST.amodifiers;aname;atype;adefault} ->
                            if aname=name
                            then (print_string "found it !\n"; Some a;)
                            else aux attributes;
                    in 
                    (
                        match aux env.attributes with
                        | None -> raise UnknownAttribute; (* no attribute with such a name found *)
                        | Some a -> {edesc;etype=Some a.atype} (* we found the attribtue in env so we know its type *)
                   );

            | ArrayInit l ->
                    print_string "array init\n";
                    let l = List.map (process_expression env) l in
                    (* check that all the values in the init array have the same type, in which cas that's the type of the array, otherwise, raise error *)
                    let rec aux l =
                        match l with
                        | [] -> print_string "I don't think this case should happen ever ...\n"; ()
                        | [x] -> ()
                        | x::y::l -> 
                                match x.AST.etype,y.etype with
                                | Some xt, Some yt ->
                                    if xt=yt
                                    then aux (y::l)
                                    else (print_string ("ArrayInit as inconsitent types : "^(Type.stringOf xt)^" vs "^(stringOf yt)^"\n"); raise IncoherentTypes; aux (y::l));
                                | _,_ -> print_string "Couldn't type the init expression\n";
                    in
                    aux l;
                    (
                        (* if the type of the init elements is an array, then we have an array of dim +1, otherwise it's a 1D array *)
                        (* but something braks with not 1D arrays *)
                        match l with {edesc;etype}::_ -> match etype with
                        | Some(Array(t,i)) -> {edesc=ArrayInit l;etype=Some(Array(Array(t,i),i+1))};
                        | Some t -> {edesc=ArrayInit l; etype=Some(Array(t,1))};
                        | None -> {edesc=ArrayInit l;etype=None} (* yeah we could look further into the array for Some t, but whatever*)
                    );

                    (* don't expect the following to work if do something more complicated than accessing a 1D array *)
            | Array (e,l) -> 
                    let e = process_expression env e in
                    let l = List.map (fun x -> match x with None -> print_string "Probably not supported\n"; None | Some x -> Some (process_expression env x)) l in
                    (
                        match e with
                        | {edesc;etype=Some(Array(t,i))} -> 
                                if i=(List.length(l))
                                then {edesc;etype=Some t} (* easy case where you access a single element *)
                                else {edesc;etype=Some(Array(t,i-List.length(l)))}; (* if you access a sub-array, something like int[][] t = ... ; t[0] would be an int[] *)
                        | _ -> "What ? Aren't you accessing an array ?\n"; exp; (* if etype is not Some Array *)
                    );

            | AssignExp (_,_,_) -> print_string "assignexp\n"; exp;

            (* for the two follwing cases, we consider that those operations are only defined on primitive types *)
            (* not sure if it is correct, even if you can't overload operators in java ... *)
            | Post(exp,op) -> 
                    let r = process_expression env exp in
                    (
                    match r with
                    | {edesc;etype=Some t} ->
                            if (t=Primitive Short || t=Primitive Int || t=Primitive Long || t=Primitive Float || t=Primitive Double || t=Primitive Char ||t=Primitive Byte)
                            then print_string ("sure you can "^(AST.string_of_postfix_op op)^" a "^(stringOf t)^".\n")
                            else (print_string ("are you sure that you can "^(AST.string_of_postfix_op op)^" a "^(stringOf t)^" ? I'm not\n"); raise IncoherentTypes);
                            {AST.edesc=Post(r,op);etype=Some t};
                    | {edesc;etype=None} -> 
                            print_string "troubles typing expression\n";
                            {AST.edesc=Post(r,op);etype=None};
                    );

            | Pre(op,exp) -> 
                    let r = process_expression env exp in
                    (
                    match r with
                    | {edesc;etype=Some t} ->
                            if (t=Primitive Short || t=Primitive Int || t=Primitive Long || t=Primitive Float || t=Primitive Double || t=Primitive Char ||t=Primitive Byte)
                            then print_string ("sure you can "^(AST.string_of_prefix_op op)^" a "^(stringOf t)^".\n")
                            else (print_string ("are you sure that you can "^(AST.string_of_prefix_op op)^" a "^(stringOf t)^" ? I'm not\n"); raise IncoherentTypes);
                            {AST.edesc=Pre(op,r);etype=Some t};
                    | {edesc;etype=None} -> 
                            print_string "troubles typing expression\n";
                            {AST.edesc=Pre(op,r);etype=None};
                    );

            (* should add the same verifications as the previous case ... *)
            (* for the moment just check that both sides of the operators are of the same type, and consider that the return typed is the same *)
            (* some counter-example could probably be found *)
            | Op(exp1,op,exp2) -> 
                    let r1 = process_expression env exp1 and r2 = process_expression env exp2 in
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

            (* does not check that the cast makes any sense *)
            | Cast (t,exp) ->
                    (
                    match (process_expression env exp) with {edesc;etype = Some etype} ->
                        print_string ("casting from "^(stringOf etype)^" to "^(stringOf t)^" and I don't care if it makes any sense\n");
                        {edesc;etype=Some t};
                    );

            (* not sure about this one, doesn't type *)
            | Type t -> print_string ("This expression is a type, I don't know what is the type of this type: "^(Type.stringOf t)); exp;

            (* not sure about the following two either ...*)
            | ClassOf t -> print_string ("ClassOf: "^(Type.stringOf t)); exp;
            | Instanceof (exp,t) -> print_string ("InstanceOf: "^(Type.stringOf t)); {edesc=Instanceof(process_expression env exp,t);etype=None};

            (* anything that I missed *)
            | _ -> print_string "expression todo\n"; print_string (AST.string_of_expression_desc edesc); print_string "\nend todo\n";  exp;
        )
        ;;

let rec process_statement env statement =
    match statement with
    | AST.VarDecl l ->
            print_string "VarDecl\n";
            let rec aux l =
                (
                match l with
                | [] -> []
                | (t,s,None)::l -> (t,s,None)::(aux l);
                | (t,s,Some e)::l ->
                        let e = process_expression env e in
                        match e.AST.etype with
                        | None -> print_string "Couldn't type the init expression ...\n"; (t,s,Some e)::l
                        | Some t2 ->
                                if t=t2
                                then ()
                                else (print_string ("init expression has a different type than variable\ "^(Type.stringOf t)^" vs "^(stringOf t2)^"\n"); raise IncoherentTypes;);
                                (t,s,Some e)::l
                ) in
            let rec aux2 l =
                (
                match l with
                | [] -> []
                | (t,s,def)::l -> {AST.amodifiers=[];aname=s;atype=t;adefault=def}::(aux2 l)
                ) in
            {methods=env.methods;attributes=(aux2 l)@env.attributes},AST.VarDecl (aux l);

    | AST.Block statements ->
            print_string "start block\n";
            let rec aux env statements =
                match statements with
                | [] -> [];
                | statement::statements ->
                        let env,statement = process_statement env statement in (* here env as local variables added, e.g if the first statement is a VarDecl *)
                        statement::(aux env statements);                        (* and thus the local variable is available in the next statements *)
            in
            let r = AST.Block(aux env statements) in
            print_string "end block\n";
            env,r;  (* but here the env is the same as we received, so the local variables stay local *)

    | Nop -> print_string "nope\n"; env,statement;

    | While(exp,statement) -> 
            print_string "while\n";
            let r = AST.While(process_expression env exp, (match process_statement env statement with env,statement -> statement)) in
            (
                match r with AST.While({edesc;etype},_) ->
                    if etype=Some (Primitive Boolean)
                    then print_string "condition is boolean OK\n"
                    else (print_string "condtion is not boolean NOT OK\n"; raise NonBooleanCondition)
                    print_string "end while\n";
            ); env,r;

    | For(truc1,truc2,truc3,statement) ->
            print_string "for need some more work\n";
            let r = AST.For(truc1,truc2,truc3, (match process_statement env statement with env,statement -> statement)) in
            print_string "end for\n";
            env,r;

    | If(cond,iftrue,iffalse) -> 
            print_string "if\n";
            let r = AST.If(process_expression env cond, (match process_statement env iftrue with env,statement -> statement), (match iffalse with None -> None | Some statement -> Some (match process_statement env statement with env,statement -> statement))) in
            (
                match r with 
                    | AST.If({edesc;etype},_,_) ->
                        if etype=Some (Primitive Boolean)
                        then print_string "condition is boolean OK\n"
                        else (print_string "condtion is not boolean NOT OK\n"; raise NonBooleanCondition);
            );
            print_string "end if\n";
            env,r;
    | Return exp ->
            (
                match exp with 
                | None -> print_string "return void\n"; env, AST.Return None;
                | Some exp -> print_string "return\n"; let r = AST.Return( Some (process_expression env exp)) in print_string "end return\n"; env,r;
            );

    | AST.Expr expr -> env,Expr(process_expression env expr);

    | _ -> print_string "statement todo\n"; env,statement;
;;

let rec process_astattributes env astattributes =
    match astattributes with 
    | [] -> [];
    | {AST.amodifiers;aname;atype;adefault}::l ->
        print_string ("modifiers: "^(ListII.concat_map " " AST.stringOf_modifier amodifiers)^"\n");
        print_string ("name: "^aname^"\n");
        let def=
        (
            match adefault with
            | None -> print_string "No default\n"; None;
            | Some exp -> Some (process_expression env exp);
        ) in
        ( 
            match def with
            | Some {AST.edesc; AST.etype} ->
                    (
                    match etype with 
                    | None -> 
                            print_string "Default not typed\n" ;
                    | Some t -> 
                            print_string ("default typed as "^(stringOf t)^"\n");
                            if atype=t 
                            then print_string "init value type correct\n"
                            else (print_string ("wrong init value type ! "^(stringOf atype)^" vs "^(stringOf t)^"\n"); raise BadTypeDefaultValue;)
                    )
            | None -> ();
        );
        {AST.amodifiers;aname;atype;adefault=def}::(process_astattributes env l);;
        
let rec process_arguments arguments =
    match arguments with
    | [] -> []
    | {AST.final;vararg;ptype;pident}::l ->
            print_string ((Type.stringOf ptype)^" "^pident^"\n");
            {AST.amodifiers=[];aname=pident;atype=ptype;adefault=None}::(process_arguments l);;

let rec process_astmethods env astmethods =
    match astmethods with
    | [] -> [];
    | {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody}::l ->
        print_string ("method name : "^mname^"\n");
        print_string "return type: "; print_string ((Type.stringOf mreturntype)^"\n");
        let args = process_arguments margstype in
        print_string "arguments :\n";
        let env = {methods=env.methods;attributes=args@env.attributes} in (* adds the arguments of the method to the list of the local variables *)
        print_string "body: \n";
        (* this is copypasted from the Block statement *)
        let rec aux env statements =
            match statements with
            | [] -> [];
            | statement::statements ->
                    let env,statement = process_statement env statement in (* here env as local variables added, e.g if the first statement is a VarDecl *)
                    statement::(aux env statements);                        (* and thus the local variable is available in the next statements *)
        in
        let b = aux env mbody in 
        print_string "end body\n\n";
        {AST.mmodifiers;mname;mreturntype;margstype;mthrows;mbody=b}::(process_astmethods env l);;

let process_astclass env ac =
    match ac with {AST.cparent; AST.cattributes; AST.cinits; AST.cconsts; cmethods; ctypes; cloc} ->
        let env = build_env env ac in 
        print_string "\nattributes\n";
        let tribute = process_astattributes env cattributes in
        print_string "\nmethods\n";
        {AST.cparent;cattributes=tribute; cinits; cconsts; cmethods=(process_astmethods env cmethods); ctypes; cloc};;

let rec process_type_list l =
    let env={methods=[];attributes=[]} in
    match l with
    | [] -> []
    | {AST.modifiers; AST.id; AST.info}::l ->
        print_string ( "modifiers: "^(ListII.concat_map " " AST.stringOf_modifier modifiers)^"\n");
        print_string ("id: "^id^"\n");
        let i = 
        (
            match info with
                | Class astclass -> AST.Class (process_astclass env astclass);
                | Inter -> print_string "Inter\n"; Inter;
        ) in
        {AST.modifiers;id;info=i}::(process_type_list l);;

