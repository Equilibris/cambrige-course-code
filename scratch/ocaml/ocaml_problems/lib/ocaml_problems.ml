(* exts *)
module Int = struct
    include Int
    let rec pow a b = if b = 0 then 1 else if b = 1 then a else (if b mod 2 = 1 then a else 1) * (pow (a * a) (b / 2))
end
module List = struct
    include List

    let pmap f a b = let inner a b = List.map (fun v -> f a v) b in List.concat_map (fun v -> inner v b) a
end

(* utils *)
let rec rep v c = if c = 0 then [] else v::(c-1 |> rep v)

let join sep = function
    | [] -> ""
    | h :: t -> List.fold_left (fun a b -> a ^ sep ^ b) h t

let print_ls ls = ls |> join " ; " |> (fun v -> "[" ^ v ^ "]") |> print_endline


exception ArbitraryFail;;

(* exs *)
module Fact = struct
    let rec fact n = if n = 1 then 1 else n * fact (n - 1)

    let%test _ = fact 5 = 120
end

module Last = struct
    let rec last = function
        | [] -> None
        | [ x ] -> Some x
        | _ :: t -> last t

    let%test _ = last ["a" ; "b" ; "c" ; "d"] = Some "d";;
    let%test _ = last [] = None;;
end

module LastTwo = struct
    let rec last_two = function
        | [] | [ _ ] -> None
        | [ a ; b ] -> Some(a, b)
        | _ :: t -> last_two t

    let%test _ = last_two ["a"; "b"; "c"; "d"] = Some ("c" , "d");;
    let%test _ = last_two [] = None;;
end 

module Nth = struct
    let rec nth v = function
        | [] -> None
        | h :: t -> if v = 0 then Some(h) else nth (v - 1) t

    let%test _ = nth 1 [0; 1; 2; 3] = Some 1;;
    let%test _ = nth 10 [0; 1; 2; 3] = None;;
end

module Len = struct
    let rec len = function
        | [] -> 0
        | _ :: t -> 1 + len t

    let%test _ = len [] = 0;;
    let%test _ = len [1 ; 2 ; 3] = 3;;
end

module Rev = struct
    let rev ls = let rec internal state = function 
        | [] -> state
        | h :: t -> internal (h :: state) t
    in internal [] ls

    let%test _ = rev [] = [] ;;
    let%test _ = rev [1 ; 2 ; 3] = [3 ; 2 ; 1];;
end

module Palindrome = struct
    open Rev

    let is_palindrome a = a = rev a

    let%test _ = is_palindrome [] = true;;
    let%test _ = is_palindrome [1 ; 2 ; 1] = true;;
    let%test _ = is_palindrome [1 ; 2 ; 2] = false;;
end

module Flatten = struct
    open Rev

    type 'a node =
      | One of 'a 
      | Many of 'a node list

    let flatten v = let rec aux ls = function 
        | [ ] -> ls
        | One v :: tail -> aux (v :: ls) tail
        | Many v :: tail -> aux (aux ls v) tail
    in rev (aux [] v)

    let%test _ = flatten [] = [];;
    let%test _ = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"];;
end

module Compress = struct
    open Rev

    let compress = let rec aux out last = function 
        | [] -> out
        | current :: tail -> if current = last then aux out last tail else aux (current :: out) current tail
    in function 
        | [] -> []
        | v :: tail -> rev (aux [v] v tail)

    let%test _ = compress [] = [];;
    let%test _ = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"];;
    (* let%test_unit _ = print_endline (List.fold_left (fun a b -> a ^b) "" (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]));; *)
    let%test _ = compress ["a"] = ["a"]
end

module Pack = struct
    open Rev

    let pack = let rec aux out current_ls last = function
        | [] -> current_ls :: out
        | current :: tail -> if
            last = current
        then aux out (current :: current_ls) current tail
        else aux (current_ls :: out) [current] current tail
    in function
        | [] -> []
        | h :: t -> rev(aux [] [h] h t)

    let%test _ = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]];;
    let%test _ = pack [] = [];;
end

module Encode = struct
    open Pack
    open Len

    let encode v = pack v |> List.map (fun ls -> (len ls, List.hd ls)) 

    let%test _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")];;
    let%test _ = encode ["a"] = [(1, "a")];;
    let%test _ = encode [] = [];;
end

module EncodeShare = struct
    type 'a rle =
      | One of 'a
      | Many of int * 'a
end

module Encode2 = struct
    open Pack
    open Len
    open EncodeShare

    let encode v = pack v |> List.map (fun ls -> if len ls = 1 then One(List.hd ls) else Many (len ls, List.hd ls)) 

    let%test _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
    let%test _ = encode ["a"] = [One "a"];;
    let%test _ = encode [] = [];;
end

module Decode = struct
    open EncodeShare

    let decode ls = ls 
        |> (List.map (
            function
                | Many (count, value) -> rep value count
                | One value -> [value]
            )
        )
        |> (List.fold_left List.append [])

    let%test _ = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
    let%test _ = decode [] = [];;
end

module EncodeDirect = struct
    open EncodeShare

    let encode = let rec aux count current = function
        | [] -> [if count = 1 then One current else Many (count, current)]
        | h :: t -> if
            h = current
        then aux (count + 1) current t
        else (if count = 1 then One current else Many (count, current))::(aux 1 h t)
    in function
        | [] -> []
        | h :: t -> aux 1 h t

    let%test _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
    let%test _ = encode ["a"] = [One "a"];;
    let%test _ = encode [] = [];;
end

module Duplicate = struct
    let duplicate = List.concat_map (fun v -> [ v ; v ])

    let%test _ = duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"];;
    let%test _ = duplicate [] = [];;
end

module Replicate = struct

    let replicate count = List.concat_map (fun v -> rep v count)

    let%test _ = replicate 2 ["a"; "b"; "c"; "c"; "d"]  = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"];;
    let%test _ = replicate 10 [] = [];;
end

module DropN = struct
    let drop n ls = let rec aux curr = function
        | [] -> []
        | h :: t -> if curr = 1 then aux n t else h :: (aux (curr - 1) t)
    in aux n ls

    (* let%test_unit _ = drop 3 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] |> List.fold_left (fun a -> fun b -> a ^ b) "" |> print_endline;; *)
    let%test _ = drop 3 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] = ["a"; "b"; "d"; "e"; "g"; "h"; "j"];;
    let%test _ = drop 1 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] = [];;
    let%test _ = drop 3 [] = [];;
end

module Split = struct
    let split ls count = let rec aux fst ls count = match ls with
        | [] -> (List.rev fst, ls)
        | h :: t -> if count = 0 then (List.rev fst, ls) else count - 1 |> aux (h :: fst) t
    in aux [] ls count

    let%test _ = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]);;
    let%test _ = split ["a"; "b"; "c"; "d"] 5 = (["a"; "b"; "c"; "d"], []);;
    let%test _ = split [] 5 = ([], []);;
end

module Slice = struct
    let rec slice ls start stop = match ls with
        | [] -> []
        | h :: t -> if 
            start != 0
        then slice t (start - 1) (stop - 1)
        else if stop = -1 then [] (* A bit cursed but thats life *)
        else h :: (slice t 0 (stop - 1))

    (* let%test_unit _ = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 |> List.fold_left (fun a -> fun b -> a ^ b) "" |> print_endline;;  *)
    let%test _ = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"];;
    let%test _ = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 2 = ["c"];;
end

module Rot = struct
    let rotate = let rec aux fst ls count = match ls with 
        | [] -> List.append ls (List.rev fst)
        | h :: t -> if
            count = 0
        then List.append ls (List.rev fst)
        else aux (h :: fst) t (count - 1)
    in aux []

    (* let%test_unit _ = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 |> List.fold_left (fun a -> fun b -> a ^ b) "" |> print_endline;;  *)
    let%test _ = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"];
end

module Remove = struct
    let remove_at = let rec aux fst idx = function
        | [] -> fst
        | h :: t -> if idx = 0
        then List.append fst t
        else aux (h :: fst) (idx - 1) t
    in aux []

    let%test _ = remove_at 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"];;
end

module Insert = struct
    let insert_at = let rec aux fst v idx = function
        | [ ] -> List.rev (v :: fst)
        | h :: t -> if idx = 1 then List.append (List.rev(v :: h :: fst)) t
                               else aux (h :: fst) v (idx - 1) t
    in aux []

    (* let%test_unit _ = insert_at "alfa" 1 ["a"; "b"; "c"; "d"] |> print_ls ;; *)
    let%test _ = insert_at "alfa" 1 ["a"; "b"; "c"; "d"] = ["a"; "alfa"; "b"; "c"; "d"];;
    let%test _ = insert_at "alfa" 10 ["a"; "b"; "c"; "d"] = ["a"; "b"; "c"; "d"; "alfa"];;
end

module Range = struct
    let range' n = let rec aux = function
        | 0 -> []
        | v -> (v - 1) :: (aux (v - 1))
    in aux n |> List.rev

    let range f t = t - f + 1 |> range' |> List.map ((+) f)

    let%test _ = range 4 9 = [4; 5; 6; 7; 8; 9];;
end

module Combos = struct
    let rec combos n ls = if List.length ls = n then [ls]
        else match ls with 
            | [] -> [] (* TODO: no idea what to do here *)
            | h :: t -> List.append (List.map (fun v -> h :: v) (combos (n - 1) t)) (combos n t)

    let%test _ = combos 2 ["a"; "b"; "c"; "d"] = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]];;
    let%test _ = combos 4 ["a"; "b"; "c"; "d"] = [["a"; "b"; "c"; "d"]];;
    let%test _ = combos 1 ["a"; "b"; "c"; "d"] = [["a"]; ["b"]; ["c"]; ["d"]];;
    let%test_unit _ = combos 3 ["a"; "b"; "c"; "d"] |> List.map (join ",") |> print_ls
end

module Disjoint = struct
    open Split
    open Combos

    let rec multipart ls = function
        | [] -> []
        | h :: t -> let (a, b) = (split ls h) in a :: (multipart b t)

        (* Cant be bothered to test this  *)
    let group ls sets = combos (List.fold_left (+) 0 sets) ls |> List.map (fun v -> multipart v sets)

    let%test _ = multipart [1 ; 2 ; 3 ; 4] [1 ; 2 ; 1] = [[1] ; [2 ; 3] ; [4]];;
    (* let%test _ = group ["a"; "b"; "c"; "d"] [2; 1] =  *)
    (*     [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]]; *)
    (*      [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]]; *)
    (*      [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]]; *)
    (*      [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]];; *)
end

module IsPrime = struct
    let is_prime n = if n < 2 then false else let rec aux k = if k * k > n then true else if n mod k = 0 then false else aux (k + 1) in aux 2

    let%test _ = is_prime 1 |> not;;
    let%test _ = is_prime 7;;
    let%test _ = is_prime 12 |> not;;
    let%test _ = is_prime 997;;
    (* let%test _ = is_prime 2305843009213693951;; *)
end

module Gcd = struct
    let rec gcd a b = let
        rem = a mod b
    in if rem = 0 then b else gcd b rem

    let%test _ = gcd 129512 9121312 = 8;;
    let%test _ = gcd 3 2 = 1;;
    let%test _ = gcd 6 3 = 3;;
    let%test _ = gcd 6 2 = 2;;
end

module CoPrime = struct
    open Gcd

    let co_prime a b = gcd a b |> ((=) 1)

    let%test _ = co_prime 3 2;;
end

module Totient = struct
    open Range
    open CoPrime

    let phi = function
        | 1 -> 1
        | n -> n - 1 |> range 1 |> List.filter (co_prime n) |> List.length

    let%test _ = phi 10 = 4;;
end

module Factors = struct
    let rec factors n = if n = 1 then [] else 
        let rec aux k = if k > n then k :: (factors (n / k)) else if n mod k = 0 then (k :: (factors (n / k))) else k + 1 |> aux
        in aux 2

    let%test _ = factors 315 = [3; 3; 5; 7];;
end

module Factors'= struct
    open Encode
    open Factors

    let factors' n = n |> factors |> encode |> List.map (fun (a, b) -> (b,a))
    
    (* let%test_unit _ = factors2 315 |> List.map (fun (a, b) -> (Int.to_string a) ^ "," ^ (Int.to_string b)) |> join ";" |> print_endline;; *)
    let%test _ = factors' 315  =[(3, 2); (5, 1); (7, 1)];;
end

module Totient2 = struct 
    open Factors'

    let phi' n = factors' n |> List.map (fun (p, m) -> (p - 1) * (Int.pow p (m - 1))) |> List.fold_left ( * ) 1

    (* let%test_unit _ = ipow 2 61 |> Int.to_string |> print_endline;; *)
    let%test _ = phi' 10 = 4;;
    let%test _ = phi' 13 = 12;;
end

(* module T1v2 = struct *)
(*     open Totient *)
(*     open Totient2 *)

(*     let%test_unit _ = phi 10 |> timeit |> print_endline;; *)
(*     let%test_unit _ = phi' 10 |> timeit |> print_endline;; *)
(* end *)

module Goldbach = struct
    open IsPrime

    let goldbach n = let rec aux k = if k < n && is_prime k then if is_prime (n - k) then (k, (n - k)) else (aux (k + 1)) else (aux (k + 1)) in aux 2

    let%test _ = goldbach 28 = (5, 23);;
    (* let%test _ = range 10 20 |> List.filter (fun a -> a |> is_prime |> not) |> List.map (fun v -> (v, goldbach v)) =  *)
    (*     [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13)); (20, (3, 17))];; *)
end

module Logic = struct
    open Range

    type bool_expr =
      | Var of string
      | Not of bool_expr
      | And of bool_expr * bool_expr
      | Or of bool_expr * bool_expr;;

    let only_bit n = Int.shift_left 1 n

    (* let bool_perm n = n |> ((+) 1) |> only_bit |> range' |> List.map (fun k -> range' n |> List.map (fun a -> Int.logand k (only_bit a)));; *)
    let bool_perm n = n |> ((+) 1) |> only_bit |> range' |> List.map (fun k -> range' n |> List.map (only_bit) |> List.map (Int.logand k) |> List.map ((!=) 0));;

    (* let%test_unit _ = bool_perm 2 |> List.map (List.map Bool.to_string) |> List.map (fun v -> "(" ^ (join ", " v) ^")") |> print_ls;; *)

    let zip a = List.map2 (fun a -> fun b -> (a, b)) a

    let snd (_, v) = v

    let table vars expr = let
        internal = List.length vars |> bool_perm |> List.map (zip vars)
    in let rec aux t = function
        | Var name -> List.find (fun (n, _) -> n = name) t |> snd
        | Not v -> aux t v |> not
        | And (a, b) -> (aux t a) && (aux t b)
        | Or (a, b) -> (aux t a) || (aux t b)
    in List.map (fun t -> (t, aux t expr)) internal

    (* let%test _ = List.rev (table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")))) = [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true); ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)];; *)
    (* let%test_unit _ = table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b"))) |> List.map (fun (ls, v) -> "[" ^ (List.map (fun(a,b) -> "(\""^a^"\","^(Bool.to_string b)^")") ls |> join "; ") ^ "], " ^ (Bool.to_string v)) |> print_ls;; *)
end

module Gray = struct
    let rec gray = function
        | 0 -> [""]
        | n -> let sub = n - 1 |> gray in List.append (List.map (fun v -> "0" ^ v) sub) (List.rev sub |> List.map (fun v -> "1" ^ v))

    let%test _ = gray 1 = ["0"; "1"];;
    let%test _ = gray 2 = ["00"; "01"; "11"; "10"];;
    let%test _ = gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"];;
end

module Huffman = struct
    type 'a tree = 
        | Leaf of int * 'a
        | Branch of ('a tree) * ('a tree);;

    let rec cost = function
        | Leaf (c, _) -> c
        | Branch (a, b) -> (cost a) + (cost b)

    let rec to_string = function
        | Leaf (_, v) -> "Leaf " ^ v
        | Branch (a, b) -> "Branch(" ^ (to_string a) ^ ", " ^ (to_string b) ^ ")";;


    let enter = List.map (fun (a, b) -> Leaf(b, a))
    let sort_tree v = List.sort (fun l -> fun r -> (cost l) - (cost r)) v;;
    let rec build_tree = function
        | []  -> raise ArbitraryFail
        | [ x ] -> x
        | a :: b :: tail -> (Branch (a, b) :: tail) |> sort_tree |> build_tree

    let rec calculate_prefix = function
        | Leaf (_, v) -> [v, ""]
        | Branch (a, b) -> List.append (calculate_prefix a |> List.map (fun (value, prefix) -> (value, "0" ^ prefix))) (calculate_prefix b |> List.map (fun (value, prefix) -> (value, "1" ^ prefix)));;

    let compute v = enter v |> sort_tree |> build_tree |> calculate_prefix;;

    let freqs = [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)];;

    (* let%test_unit _ = compute freqs |> List.map (fun (a,b) -> a ^ b) |> print_ls;; *)
    let%test _ = compute freqs = [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101"); ("d", "111")];;
end

module BinaryTree = struct
    type 'a binary_tree =
      | Empty
      | Node of 'a * 'a binary_tree * 'a binary_tree;;
end

module AllBalancedTrees = struct
    (* open BinaryTree *)

    (* let cbal_tree = function *)
    (*     | 0 -> [Empty] *)
    (*     | _ -> failwith "todo" *)

    (* let%test _ = cbal_tree 4 = [ *)
    (*     Node ('x', *)
    (*         Node ('x', Empty, Empty),  *)
    (*         Node ('x', *)
    (*             Node ('x', Empty, Empty), *)
    (*             Empty) *)
    (*         ); *)
    (*     Node ('x', *)
    (*         Node ('x', Empty, Empty), *)
    (*         Node ('x', *)
    (*             Empty, *)
    (*             Node ('x', Empty, Empty) *)
    (*         ) *)
    (*     ); *)
    (*     Node ('x', *)
    (*         Node ('x', *)
    (*             Node ('x', Empty, Empty), *)
    (*             Empty *)
    (*         ), *)
    (*         Node ('x', Empty, Empty) *)
    (*     ); *)
    (*     Node ('x', *)
    (*         Node ('x', *)
    (*             Empty, *)
    (*             Node ('x', Empty, Empty) *)
    (*         ), *)
    (*         Node ('x', Empty, Empty) *)
    (*     ) *)
    (* ];; *)
end

module Factr = struct
    let faci n = Array.init n (fun n -> n + 1) |> Array.fold_left ( * ) 1

    let%test_unit _ = faci 3 |> Int.to_string |> print_endline;;
end
