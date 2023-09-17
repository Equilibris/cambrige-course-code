module Utils = struct
    let rec rep v c = if c = 0 then [] else v::(c-1 |> rep v)

    let join sep = function
        | [] -> ""
        | h :: t -> List.fold_left (fun a -> fun b -> a ^ sep ^ b) h t

    let print_ls ls = ls |> join " ; " |> print_endline
end

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
    open Utils

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
    open Utils

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
    let rec combos n ls = if n = 0 then [[]] else if
        n = 1
    then List.map (fun a -> [a]) ls
    else if List.length ls = n then [ls]
    else match ls with 
        | [] -> [] (* TODO: no idea what to do here *)
        | h :: t -> List.append (List.map (fun v -> h :: v) (combos (n - 1) t)) (combos n t)

    let%test _ = combos 2 ["a"; "b"; "c"; "d"] = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]];;
end

module Disjoint = struct
    open Split

    let rec multipart ls = function
        | [] -> []
        | h :: t -> let (a, b) = (split ls h) in a :: (multipart b t)

    let%test _ = multipart [1 ; 2 ; 3 ; 4] [1 ; 2 ; 1] = [[1] ; [2 ; 3] ; [4]];;
end

