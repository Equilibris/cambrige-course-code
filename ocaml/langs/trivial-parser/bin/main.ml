open Lexer

let () =
  try
    while true do
      print_string "> ";
      flush stdout;  (* Ensure the prompt is displayed immediately *)

      (* Read a line of input from stdin *)
      let input_line = input_line stdin in

      (* Lex the input line and parse it *)
      let lexbuf = Lexing.from_string input_line in
      let result = Parser.s Lexer.token lexbuf in

      Printf.printf "Result: %d\n" result;
    done
  with
  | Parser.Error ->
    Printf.eprintf "Syntax error\n";
    exit 1
  | End_of_file -> exit 0
