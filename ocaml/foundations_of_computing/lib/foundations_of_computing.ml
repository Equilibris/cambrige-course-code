(* This is a prelude, please search for Ex 1.1 for the first ex *)
module Rel = struct
    let (=.) a b = Float.abs (a -. b) |> (>) 10e-8

    let%test _ = 1. =. 2. |> not
    let%test _ = 1. =. 1.
end

module Dbg = struct
    let join sep = function
        | [] -> ""
        | h :: t -> List.fold_left (fun a b -> a ^ sep ^ b) h t

    let print_ls ls = ls |> join " ; " |> (fun v -> "[" ^ v ^ "]") |> print_endline
end

module Algebra = struct
    module type SemiGroup = sig
        type t

        val unit : t
        val op : t -> t -> t
    end

    module type Group = sig
        type t

        val unit : t
        val op : t -> t -> t
        val invert : t -> t
    end

    module GroupNarrowing (Concrete : Group) = struct
        module Concrete : SemiGroup with type t = Concrete.t = struct
            type t = Concrete.t

            let unit = Concrete.unit
            let op = Concrete.op
        end
    end

    let n_apply : type a. (module SemiGroup with type t = a) -> a -> int -> a =
        fun internal value exp -> 
            let module M = (val internal : SemiGroup with type t = a)
            in let rec aux value exp acc = 
                if exp = 0 then acc
                else aux
                    (M.op value value)
                    (exp / 2)
                    (if
                        exp mod 2 = 0
                     then acc
                     else M.op value acc)
            in aux value exp M.unit
end

module Fn = struct
    (* haskell (.) *)
    let (<<) f g x = f(g(x))
end

(*
 # Ex 1.1

 This 'solution' to the 2000-bug is not actually a solution but rather
 a delaying of its effects, the cost of implementing this will be to
 great for the short term gains it leads to.
 *)

(*
 # Ex 1.2
 *)

module Ex1_2 = struct
    type date = int

    let eq = (=)

    let lt a b = match (0 <= a && a < 50, 0 <= b && b < 50) with
        | (false, true) -> true
        | (true, false) -> false
        | _ -> a < b

    let gt a b =  (eq a b |> not) && (lt a b |> not)

    let add a b = (a + b) mod 100
    let sub a b = (a - b + 100) mod 100

    (* test_2023_gt_1999 *)
    let%test _ = gt 23 99
    let%test _ = lt 23 23 |> not
    let%test _ = 20 = add 80 40
    let%test _ = 90 = add 50 40
    let%test _ = 30 = add 20 10
end

(*
 # Ex 1.3

 if a then true else false

 in ocaml is equivalent to identity monomorphized to (a : bool)
 in some langs it may be useful to write such since they have notions
 of thruthy and falseyness, but thanks to ocaml's type system there is
 no reason to do it here.

 on the other hand

 if a then flase else true

 is equivalent to the build in function `not` and therefore since we
 have not we have no reason of this construct.
 *)

(*
 # Ex 1.4

 if this was a more advanced course i would hand execute Algorithm J/W,
 but since were in a foundational course it sufices to say that x must
 be able to be applied to *. which has a signature of (float -> float ->
 float) thereby the only (most general) type satasfying this is
 float. Following that we know that x is a float we can see how at a
 point we simply return x, and thereby returning a float.
 *)

(*
 # Ex 1.5
 *)

module Ex1_5 = struct
    let rec f x = function
        | 1 -> x
        | n -> x +. f x (n - 1)

    let%test_unit _ = (f 0.1 10000) -. 1000. |> Float.to_string |> print_endline
end

module Ex1_6 = struct 
    let rec gamma = function
        | 0 -> (1. +. (sqrt 5.)) /. 2.
        | n -> 1. /. ((gamma (n - 1)) -. 1.)

    let%test_unit _ = 
        let open Fn in
           Array.init 10 (
               Float.to_string
            << gamma
            << (( * ) 50)
           )
        |> Array.fold_left (fun a b -> a ^ " " ^ b) ":"
        |> print_endline
end

(*
 # Ex 2.1

 Theres a special and a general way of doing this, well do both
 *)

module Ex2_1 = struct
    open Algebra
    open Rel

    (* very approximate *)
    module FloatMul : Group with type t = float = struct
        type t = float

        let op = ( *. )
        let invert = (/.) 1.
        let unit = 1.
    end

    (* General and simplest way from a DX perspective *)
    let power = n_apply (module FloatMul)

    let%test _ = power 10. 3 =. 1000.

    (* Special but moduleless way *)
    let spower =
        let rec aux acc x = function
            | 0 -> acc
            | n -> aux ((if n mod 2 = 1 then x else 1.) *. acc) (x *. x) (n / 2)
        in aux 1.

    let%test _ = spower 10. 3 =. 1000.
end

(*
 # Ex 2.2

 TODO, the only hard part here is finding what value of n they selected for n log n and 1h
 *)

(*
 # Ex 2.3

 Define f(n) = O(g(n)) to be:
 $ ∃c > 0 ∃N ∀n > N (|f(n)| <= c g(n)) $

 Without that much experience of this kind of proof, here's at least my attempt

 First lets assume that

 ∀k ak >= 1

 I am unsure how to prove this without this assumption

 First lets prove that (O(g1(n) + ... + gk(n)) >= O(a1 g1(n) + ... + ak gk(n)))

     ∃c > 0 ∃N ∀n > N (a1 g1(n) + ... + ak gk(n) <= c (g1(n) + ... + gk(n)))

          pick c = a1 * ... * ak
     then pick N = any arbitrary value, in this case 1

     observe that

     ∀n > 1 (a1 g1(n) + ... + ak gk(n) <= c (g1(n) + ... + gk(n)) = c g1(n) + ... + c gk(n)) $

     this is true as ∀k (ak <= c) is true

 Next lets prove that (O(g1(n) + ... + gk(n)) <= O(a1 g1(n) + ... + ak gk(n)))

     ∃c > 0 ∃N ∀n > N (g1(n) + ... + gk(n) <= c (a1 g1(n) + ... + ak gk(n)))

          pick c = 1
     then pick N = any arbitrary value, in this case 1

     observe that

     ∀n > 1 (g1(n) + ... + gk(n) <= a1 g1(n) + ... + ak gk(n))

     this is true and follows of our assumption

 Using these two corollarys we can conclude that
 O(g1(n) + ... + gk(n)) = O(a1 g1(n) + ... + ak gk(n))

 *)

(*
 # Ex 2.4

 The solution is quite logically O(log n). This can be seen by ignoring
 the constant factor on the recursive call. A full proof can be provided
 by induction but i feel is a bit overkill for this task
 *)

(*
 # Ex 3.1
 *)

module Ex3_1 = struct
    (* Time Θ(n) Space Θ(n) *)
    let rec rsum = function
        | [] -> 0
        | hd :: tl -> hd + rsum tl

    (* Time Θ(n); Space Θ(1) *)
    let isum = let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (acc + hd) tl
    in aux 0

    let%test _ = rsum [1; 2; 3] = 6
    let%test _ = isum [1; 2; 3] = 6
end

(*
 # Ex 3.2

 Given a linked-list representation the best you can do will be Ω(n)

 Given a (dynamic-)array / vector representation, Ω(1)
 *)

module Ex3_2 = struct
    let rec last = function
        | []    -> failwith "Domain error"
        | [x]   -> x
        | _::tl -> last tl
end

module Ex3_3 = struct
    let even_pos ls = let rec aux flip value = match (flip, value) with
        | (_, []) -> []
        | (true,  hd::tl) -> hd::aux false tl
        | (false, _::tl) -> aux true tl
    in aux false ls 

    let%test _ = even_pos [1;2;3;4] = [2;4]
end

(*
 # Ex 3.4

 Well id is simply the trivial identity morphism and therefor by
 definition exsists for all types element in the category of types
 mapping every type to itself

 On the other hand, thanks to the halting problem and the fact that ocaml
 is (barely even) fp and not tfp each type can be represented as a set
 of all possible values, one of these values is always bottom. Since we
 know that bottom is an element in every type and that a non-halting
 function returns in a bottom we can conclude that loop accepts any
 valid type and outputs bottom, in other words any type (yet they dont
 have to be the same and thereby we get 'a -> 'b rather than 'a -> 'a).
 *)

module Ex3_5 = struct
    let rec tails = function
        | []    -> [[]]
        | hd::tl -> (hd::tl)::tails tl

    let%test _ = tails [1; 2; 3] = [[1; 2; 3]; [2; 3]; [3]; []]
end

(*
 # Ex 4.1

 The naive version of doing this will be O(n²), in my opinion this is
 unacceptable, on the other hand a marginally less naive solution has time
 O(s(n) + n) = O(s(n)) where s(n) is the performace of a given sorting
 function, assuming a cmp sort the best case we can get is O(n log n),
 following this the performace of the slightly less naive algorithm is
 O(n log n)
 *)

module Ex4_1 = struct
    let merge a b = let rec aux acc a b = match a, b with
        | [],       []       -> []
        | hd::tl,   []       -> aux (hd::acc) tl []
        | [],       hd::tl   -> aux (hd::acc) tl []
        | hda::tla, hdb::tlb -> if hda = hdb then aux (hda::acc) tla tlb else
                                if hda < hdb then aux (hda::acc) tla (hdb::tlb) else
                                                  aux (hdb::acc) (hda::tla) tlb
    in aux [] (List.fast_sort (-) a) (List.fast_sort (-) b)

    let%test _ = merge [4; 7; 1] [6; 4; 7] = [1; 4; 6; 7]
    (* let%test _ = List.fast_sort (-) [1; 2; 3] = [1; 2; 3] *)
end

