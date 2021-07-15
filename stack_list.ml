module StackList =
  struct
    open Format

    type t = int
    type stack = int * t list

    exception Empty

    let empty = 0, []

    let is_empty = function
      | 0, _ -> true
      | _       -> false

    let cons t = function
      | n, l -> n+1, t::l

    let head = function
      | _, []   -> raise Empty
      | _, x::l -> x

    let tail = function
      | _, []   -> raise Empty
      | n, x::l -> n-1, l

    let length = function
      | n, _ -> n

    let rec str fmt = function
      | _, l -> fprintf fmt "%a" str_list l
    and str_list fmt = function
      | []   -> ()
      | [e]  -> fprintf fmt "%d" e
      | e::k -> fprintf fmt "%d, %a" e str_list k

    let pprint q = print_string @@ asprintf "%a" str q

  end

let () =
  let q = ref StackList.empty in

  assert (StackList.is_empty !q);

  q := StackList.cons 3 !q;
  q := StackList.cons 2 !q;
  q := StackList.cons 1 !q;
  q := StackList.cons 0 !q;
  q := StackList.tail !q;

  assert (not @@ StackList.is_empty !q);

  StackList.pprint !q
