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
  let rec loop (ctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let line = trim (read_line ()) in
      if length line = 0 then loop (ctx, tctx)
      else let entire_line = string_before (process_line line) (length line - 2) in
      let c = s token (from_string(entire_line)) in
      loop (execute (ctx, tctx) c)
    with
    | Lexical_error ->
        print_endline "lexical error";
        loop (ctx, tctx)
    | Parse_error ->
        print_endline "syntax error";
        loop (ctx, tctx)
    | Type_error e ->
        print_endline ("type error: " ^ e);
        loop (ctx, tctx)
    | End_of_file ->
        print_endline "...bye!!!"
  in
  loop (emptyctx, emptyctx)
;;

top_level_loop ()
;;
