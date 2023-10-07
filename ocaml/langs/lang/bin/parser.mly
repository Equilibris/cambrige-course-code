%token IF THEN ELSE LET IN LPAR RPAR FUN EQ EOF
%token <string> ID
%type <Lang.expr> expr
%type <Lang.stmt> stmt
%start <Lang.stmt> s

%left FUN
%left COND
%left LET
%left LPAR
%left ID
%left APP

%%

s:
  stmt EOF { $1 }

stmt:
    | LPAR; RPAR; EQ; value = expr { Lang.Main value }
    | id = ID; EQ; value = expr { Lang.TopDecl(id, value) }

expr:
    | LPAR; expr; RPAR;                  { $2 }
    | LET; name = ID; EQ; value = expr; IN; tail = expr;
                                         { Lang.Decl(name, value, tail) }
    | IF; condition = expr; THEN; main = expr; ELSE; aux = expr %prec COND
                                         { Lang.Cond(condition, main, aux) }
    | FUN; name = ID; e = expr %prec FUN { Lang.Ab(name, e) }
    | f = expr; arg = expr     %prec APP { Lang.App(f, arg) }
    | ID                                 { Lang.Id($1) }
