open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

open String;;
open Str;;

let build_multiline input =
  (* Crear la libreta (Buffer) para guardar las líneas *)
  let buffer = Buffer.create 100 in

  (* Escribir la primera línea en la libreta *)
  Buffer.add_string buffer (String.trim input);

  (* Aquí comienza el bucle para pedir más líneas si es necesario *)
  let rec loop () =
    (* Verifica si la última línea escrita termina con ";;" *)
    if String.ends_with ~suffix:";;" (Buffer.contents buffer) then
      let result = Buffer.contents buffer in
      String.sub result 0 (String.length result - 2)  (* Si termina, junta todo y devuelve la historia *)
    else begin
      print_string "  ";  (* Pide otra línea mostrando un espacio como prompt *)
      flush stdout;

      (* Lee una nueva línea y guárdala en la libreta *)
      let next_line = read_line () in
      Buffer.add_char buffer ' ';  (* Añade un espacio entre líneas *)
      Buffer.add_string buffer (String.trim next_line);

      loop ()  (* Repite el proceso *)
    end
  in
  loop ()  (* Inicia el bucle *)
;;



let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let line = trim (read_line ()) in
      if String.length line = 0 then loop (vctx, tctx)
      else let whole_line = build_multiline line in
      let c = s token (Lexing.from_string whole_line) in
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
