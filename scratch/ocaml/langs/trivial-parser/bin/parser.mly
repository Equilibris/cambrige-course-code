%token PLUS TIMES EOF RPAR LPAR DIV MINUS
%token <int> INT
%type <int> expr
%start <int> s

%left MINUS
%left PLUS
%left DIV
%left TIMES
%left PEREN

%%

s:
  expr EOF { $1 }

expr:
  | INT { $1 }
  | LPAR; e = expr; RPAR %prec PEREN     { e }
  | expr PLUS expr                   { $1 + $3 }
  | expr MINUS expr                  { $1 - $3 }
  | expr DIV expr                    { $1 / $3 }
  | expr TIMES expr                  { $1 * $3 }
