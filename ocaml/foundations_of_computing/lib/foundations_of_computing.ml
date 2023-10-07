let pi = 3.1415;;

module type Shape = sig
    type t
    
    val area: t -> float
end

module Cirle: Shape = struct
    type t = { radius: float }

    let area (Radius v) = pi *. v *. v
end

let%test_unit _ = let open Shape in (Cirle.Radius 10.0)
