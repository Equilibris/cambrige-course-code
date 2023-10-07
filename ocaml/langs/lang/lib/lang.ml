type expr =
            (* if    then   else *)
    | Cond of expr * expr * expr
            (* let _ = _ in _  *)
    | Decl of string * expr * expr
    | App  of expr * expr
    | Ab   of string * expr
    | Id   of string

type stmt =
    | TopDecl of string * expr
    | Main of expr




