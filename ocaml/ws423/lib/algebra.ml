module type Monoid = sig
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
    module Concrete : Monoid with type t = Concrete.t = struct
        type t = Concrete.t

        let unit = Concrete.unit
        let op = Concrete.op
    end
end

module IntMul : Monoid with type t = int = struct
    type t = int

    let unit = 1
    let op = ( * )
end
module IntAdd : Group with type t = int = struct
    type t = int

    let unit = 0
    let op = ( + )
    let invert a = -a
end
module FloatMul : Group with type t = float = struct
    type t = float

    let unit = 1.
    let op = (+.)
    let invert = (/.) 1.
end

(* Lives in O(n) memory, optimize this *)
let rec n_apply : type a. (module Monoid with type t = a) -> a -> int -> a =
    fun internal value exp -> 
        let module M = (val internal : Monoid with type t = a)
        in if exp = 0 then
            M.unit
        else
            M.op
                (if exp mod 2 = 0 then M.unit else value)
                (n_apply
                    internal
                    (M.op value value)
                    (exp / 2))

let mul  = n_apply (module IntAdd)
let pow  = n_apply (module IntMul)
let powf = n_apply (module FloatMul)

let%test _ = mul 2 0 = 0
let%test _ = mul 2 1 = 2
let%test _ = mul 2 2 = 4
let%test _ = mul 2 3 = 6
let%test _ = mul 2 4 = 8
let%test _ = mul 2 5 = 10

let%test _ = pow 2 0 = 1
let%test _ = pow 2 1 = 2
let%test _ = pow 2 2 = 4
let%test _ = pow 2 3 = 8
let%test _ = pow 2 4 = 16
let%test _ = pow 2 5 = 32
let%test _ = pow 2 12 = 4096

type mat = int * int * int * int

let mat_mul (a, b,
             c, d) 
            (e, f,
             g, h) : mat = 
            (a * e + b * g, a * f + b * h,
             c * e + d * g, f * c + h * d)

let%test _ = mat_mul (1, 0, 0, 1) (1, 2, 3, 4) = (1, 2, 3, 4)
let%test _ = mat_mul (1, 2, 3, 4) (1, 0, 0, 1) = (1, 2, 3, 4)

module MatMul : Monoid = struct
    type t = mat

    let unit = (1, 0, 0, 1)

    let op = mat_mul
end

let mat_pow = n_apply (module MatMul)

