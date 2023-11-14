open Lexer

let rec print_tree = function
    | L2.Val a -> Int.to_string a
    | L2.Par a -> "(" ^ (print_tree a) ^ ")"
    | L2.Add(a, b) -> "(+ " ^ (print_tree a) ^ " " ^ (print_tree b) ^ ")"
    | L2.Sub(a, b) -> "(- " ^ (print_tree a) ^ " " ^ (print_tree b) ^ ")"
    | L2.Mul(a, b) -> "(* " ^ (print_tree a) ^ " " ^ (print_tree b) ^ ")"
    | L2.Div(a, b) -> "(/ " ^ (print_tree a) ^ " " ^ (print_tree b) ^ ")"


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

      Printf.printf "Result: %s\n" (print_tree result);
    done
  with
  | Parser.Error ->
    Printf.eprintf "Syntax error\n";
    exit 1
  | End_of_file -> exit 0
