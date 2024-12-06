open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open Str;;


let build_multiline input =
  
  let buffer = Buffer.create 100 in
  Buffer.add_string buffer (String.trim input);
  let rec loop () =
    if String.ends_with ~suffix:";;" (Buffer.contents buffer) then
      let result = Buffer.contents buffer in
      String.sub result 0 (String.length result - 2) 
    else begin
      print_string "  ";  
      flush stdout;
      let next_line = read_line () in
      Buffer.add_char buffer ' ';  
      Buffer.add_string buffer (String.trim next_line);

      loop ()  
    end
  in
  loop ()  
;;


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let line = trim (read_line ()) in
      if length line = 0 then loop (vctx, tctx)
      else let whole_line = build_multiline line in
      let c = s token (from_string(whole_line)) in
      loop (execute (vctx, tctx) c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
    
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyctx, emptyctx)
  ;;

top_level_loop ()
;;