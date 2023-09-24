%token PLUS TIMES EOF
%token <int> INT
%type <int> expr
%start <int> s

%left PLUS
%left TIMES

%%

s:
  expr EOF { $1 }

expr:
  | INT { $1 }
  | expr PLUS expr { $1 + $3 }
  | expr TIMES expr { $1 * $3 }
