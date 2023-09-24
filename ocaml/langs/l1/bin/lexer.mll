{
  open Parser
}

let digit = ['0'-'9']
let whitespace = [' ' '\t' '\n']

rule token = parse
  | whitespace+       { token lexbuf }   (* Skip whitespace *)
  | digit+ as number  { INT(int_of_string number) }
  | '+'               { PLUS }
  | '*'               { TIMES }
  | eof               { EOF }
