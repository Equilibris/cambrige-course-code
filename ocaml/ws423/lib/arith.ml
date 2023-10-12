module type Add = sig
    type self
    type other

    type output

    val add: self -> other -> output
end

module IntAdd : Add = struct
    type self = int
    type other = int

    type output = int

    let add = (+)
end

