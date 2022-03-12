let id = fun x -> x

let aba = fun x -> fun y -> x

let comp f g x = x |> f |> g

let proj1 = fun (x, y) -> x

let proj2 = fun (x, y) -> y

let conj_com = fun (x, y) -> y, x

let top = ()

let all_imp_top = fun x -> ()

type empty = | (* botom *)

let absurd = fun (x:empty): 'a -> match x with _ -> .

let neg = fun (x:'a) : empty -> match x with _ -> Obj.magic ()

let contr =
  fun (f:'a -> 'b) ->
  fun (b:'b -> empty) ->
  fun (a:'b) : empty ->
  b (f a)

type ('a, 'b) coprod =
    Left of 'a
  | Right of 'b

let absurd : 'a -> 'b = fun x -> raise Not_found

let rec absurd = fun x -> absurd x

let fake : empty = absurd ()
