open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open Str;;

let rec process_line line =
  let r = regexp ";;" in
    if string_match r line (length line - 2) then string_before line (length line - 2)
    else begin
      print_string "  ";
      flush stdout;
      process_line (line ^ " " ^ read_line ())
    end
;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      let line = trim (read_line ()) in
      if String.length line = 0 then loop ctx
      else
        let input = process_line line in
        let tm = s token (from_string input) in
        print_endline ("Expression: " ^ string_of_term ~prec:0 tm);
        let tyTm = typeof ctx tm in
        print_endline ("Result: " ^ string_of_term ~prec:0 (eval tm) ^ " : " ^ string_of_ty tyTm);
        loop ctx
    with
    | Lexical_error ->
        print_endline "lexical error";
        loop ctx
    | Parse_error ->
        print_endline "syntax error";
        loop ctx
    | Type_error e ->
        print_endline ("type error: " ^ e);
        loop ctx
    | End_of_file ->
        print_endline "...bye!!!"
  in
  loop emptyctx
;;

top_level_loop ()
;;
