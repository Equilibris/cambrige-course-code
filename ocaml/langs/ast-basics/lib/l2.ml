type syntax_tree =
    | Val of int
    | Par of syntax_tree
    | Add of syntax_tree * syntax_tree
    | Sub of syntax_tree * syntax_tree
    | Mul of syntax_tree * syntax_tree
    | Div of syntax_tree * syntax_tree

