module Stack =
  struct
    open Format

    type t = int
    type stack = int * t list

    exception Empty

    let empty = 0, []

    let is_empty = function
      | 0, _ -> true
      | _       -> false

    let push t = function
      | n, l -> n+1, t::l

    let pop = function
      | _, []   -> raise Empty
      | n, x::l -> n, l

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
  let q = ref Stack.empty in

  assert (Stack.is_empty !q);

  q := Stack.push 3 !q;
  q := Stack.push 2 !q;
  q := Stack.push 1 !q;
  q := Stack.push 0 !q;
  q := Stack.pop !q;

  assert (not @@ Stack.is_empty !q);

  Stack.pprint !q
