module Abr =
  struct
    type elt = int
    type abr = E | T of abr * elt * abr

    let empty = E

    let is_empty = function
      | E -> true
      | _ -> false

    let rec member e = function
      | E -> false
      | T(l, e', r) ->
        if e' < e then member e r
        else if e' > e then member e l
        else true

    let rec insert e = function
      | E -> T(E, e, E)
      | T(l, e', r) as a ->
        if e' < e then
          T(l, e', insert e r)
        else if e' > e then
          T(insert e l, e', r)
        else a
  end
