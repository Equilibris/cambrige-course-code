open Lexer
open Lang

let rec print_tree = function
    | Cond (cond, a, b) -> "[IF " ^ (print_tree cond) ^ " " ^ (print_tree a) ^ " " ^ (print_tree b) ^ "]"
    | Decl (name, a, b) -> "[LET( " ^ name ^ " ) " ^ (print_tree a) ^ " " ^ (print_tree b) ^ "]"
    | App  (a, b)       -> "(" ^ (print_tree a) ^ " " ^ (print_tree b) ^ ")"
    | Ab   (arg, value) -> "[FUN( " ^ arg ^ " ) " ^ (print_tree value) ^ "]"
    | Id name           -> name

let rec print_tree_syntax = function
    | Cond (cond, a, b) -> "if " ^ (print_tree_syntax cond) ^ " then " ^ (print_tree_syntax a) ^ " else " ^ (print_tree_syntax b)
    | Decl (name, a, b) -> "let " ^ name ^ " = " ^ (print_tree_syntax a) ^ " in " ^ (print_tree_syntax b)
    | App  (a, b)       -> (print_tree_syntax a) ^ " " ^ (print_tree_syntax b)
    | Ab   (arg, value) -> "(\\"^arg^ " " ^ (print_tree_syntax value) ^")"
    | Id name           -> name

let print_lex = let open Parser in function
    | THEN -> "then"
    | RPAR -> ")"
    | LPAR -> "("
    | LET -> "let"
    | IN  -> "in"
    | IF  -> "if"
    | FUN -> "\\"
    | EQ -> "="
    | EOF -> "$EOF"
    | ELSE -> "else"
    | ID a -> "id:" ^ a

let rec beta_reduce name v = function
    | Cond (cond, a, b)   -> Cond (beta_reduce name v cond, beta_reduce name v a, beta_reduce name v b)
    | Decl (v_name, a, b) -> if name != v_name then Decl(v_name, beta_reduce name v a, beta_reduce name v b) else Decl(name, a, b)
    | App  (a, b)         -> App(beta_reduce name v a, beta_reduce name v b)
    | Ab   (arg, value)   -> if arg != name then Ab(arg, beta_reduce name v value) else Ab(arg, value)
    | Id value            -> if value = name then v else Id value

let rec reduce = function
    | App (Ab(arg, value), v) -> (beta_reduce arg v value, false)
    | App (a, b) -> let (reda, aterm) = reduce a in let (redb, bterm) = reduce b in (App (reda, redb), aterm && bterm)
    | Decl (var_name, var_val, expr) -> (beta_reduce var_name var_val expr, false)
    | terminal -> (terminal, true)

let rec n_step_reduce n value = if
    n != 0
then match reduce value with
    | (v, true) ->   print_tree_syntax v |> print_endline
    | (v, false) -> (print_tree_syntax v |> print_endline; n_step_reduce (n - 1) v)
else print_endline "Max simplification reached"

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

      (* Printf.printf "Result: %s\n" (print_tree_syntax result); *)
      (print_tree result |> print_endline;
      (print_tree_syntax result |> print_endline; n_step_reduce 20 result))
    done
  with
  | Parser.Error ->
    Printf.eprintf "Syntax error\nTS:"
  | End_of_file -> exit 0
