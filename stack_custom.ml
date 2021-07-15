module Stack =
  struct
    open Format

    type t = int
    type stack = Nil | Cons of int * t * stack

    exception Empty

    let empty = Nil

    let is_empty = function
      | Nil -> true
      | _ -> false

    let cons t = function
      | Nil -> Cons (1, t, Nil)
      | Cons (n, _, _) as c -> Cons (n+1, t, c)

    let head = function
      | Nil -> raise Empty
      | Cons (_, x, _) -> x

    let tail = function
      | Nil -> raise Empty
      | Cons (_, _, t) -> t

    let length = function
      | Nil -> 0
      | Cons (n, _, _) -> n

    let rec str fmt = function
      | Nil -> ()
      | Cons (_, x, Nil) -> fprintf fmt "%d" x
      | Cons (_, x, s) -> fprintf fmt "%d, %a" x str s

    let pprint q = print_string @@ asprintf "%a" str q

  end

let () =
  let q = ref Stack.empty in

  assert (Stack.is_empty !q);

  q := Stack.cons 3 !q;
  q := Stack.cons 2 !q;
  q := Stack.cons 1 !q;
  q := Stack.cons 0 !q;
  q := Stack.tail !q;

  assert (not @@ Stack.is_empty !q);

  Stack.pprint !q
