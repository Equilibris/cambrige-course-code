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
                | Many (count, value) -> let rec rep v c = if c = 0 then [] else v::(c-1 |> rep v) in rep value count
                | One value -> [value]
            )
        )
        |> (List.fold_left (fun a -> fun b -> List.concat [a ; b]) [])

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

module Replicate = struct
    let duplicate ls = ls |> (List.concat_map (fun v -> [ v ; v ]))

    let%test _ = duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"];;
    let%test _ = duplicate [] = [];;
end
