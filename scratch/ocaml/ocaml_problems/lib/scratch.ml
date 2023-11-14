let fact n = Array.init n (fun n -> n + 1) |> Array.fold_left ( * ) 1

let n_join_apply join apply unit =
    let rec aux idx final = if idx = final then unit else join (apply idx) (aux (idx + 1) final)
    in aux 0

(* let%test _ = let l = [l] in l *)

let eapprox n =
    let rec inner k = if n = k then 1.
        else 1. +. (inner (k + 1)) /. (Int.to_float k)
    in inner 1

let%test_unit _ = [1;2;3;10] |> List.map eapprox |> List.map Float.to_string |>List.map print_endline |> (fun _ -> ())

let rec gcd a b =
    if a = b then a else
    match a mod 2, b mod 2 with
        | 0, 0 -> 2 * gcd (a/2) (b/2)
        | 1, 0 -> gcd a (b / 2)
        | 0, 1 -> gcd b a
        | 1, 1 -> gcd b (a/2 - b/2)
        | _ -> failwith "Unreachable"

let%test_unit _ = gcd 2 10 |> Int.to_string |> print_endline

let rec cons_all c = function
    | [] -> []
    | cs :: css -> (c::cs):: cons_all c css

(* change [2, 1] 3 *)
(* change [2, 1] 1 *)
(* change [1] 1 *)
let rec change till amt = 
    if amt = 0 then
        [ [] ]
    else
        match till with
        | [] -> []
        | c :: till ->
            if amt < c then
                change till amt
            else 
                let next = change (c::till) (amt - c) in
                let without_c = change till amt in
                cons_all c next @ without_c

