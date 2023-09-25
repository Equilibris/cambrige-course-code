%token PLUS TIMES EOF RPAR LPAR DIV MINUS
%token <int> INT
%type <L2.syntax_tree> expr
%start <L2.syntax_tree> s

%left MINUS
%left PLUS
%left DIV
%left TIMES
%left PEREN

%%

s:
  expr EOF { $1 }

expr:
  | INT { L2.Val $1 }
  | LPAR; e = expr; RPAR %prec PEREN { L2.Par e }
  | expr PLUS expr                   { L2.Add ($1, $3) }
  | expr MINUS expr                  { L2.Sub ($1, $3) }
  | expr DIV expr                    { L2.Div ($1, $3) }
  | expr TIMES expr                  { L2.Mul ($1, $3) }
