open Parser
open Typing

let execute lexbuf verbose = 
  try 
    let ast = compilationUnit Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;
    
    match ast with {package; type_list} ->
        (
            match package with 
            | None -> print_string "No package\n";
            | Some l -> AST.print_package l;
        );
        (* the same tree, but all the expressions are typed *)
        let tast = Typing.process_type_list type_list in 
        print_string "\n\n+++++++++++++++++++++++++++++++++\nsecond pass, should display the types of the expressions as they have been determined at the previous step\n(and someday it will !)\n(and now it kinda does !)\n\n";
        let tast = Typing.process_type_list tast in ();

   (*     let tast = Typing.typing ast in (); *)

    
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l


