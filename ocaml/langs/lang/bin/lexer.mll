{
    open Parser
}

let digit = ['0'-'9']
let whitespace = [' ' '\t' '\n' '\r']

rule token = parse
  | [^ ' ' '\t' '\n' '\r' '\\' '(' ')' '=']+ as id {
      (* Check if the identifier is a keyword *)
      match id with
      | "if"    -> IF
      | "then"  -> THEN
      | "else"  -> ELSE
      | "let"   -> LET
      | "in"    -> IN
      | _ -> ID(id)
    }
  | whitespace+       { token lexbuf }   (* Skip whitespace *)
  | '\\'              { FUN }
  | '('               { LPAR }
  | ')'               { RPAR }
  | '='               { EQ }
  | eof               { EOF }
