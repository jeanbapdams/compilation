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
        Typing.process_type_list type_list;

   (*     let tast = Typing.typing ast in (); *)

    
  with 
    | Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l


