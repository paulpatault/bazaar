module Queue =
  struct
    open Format

    type t = int
    type queue = int * t list * t list

    exception Empty

    let empty = (0, [], [])

    let is_empty = function
      | 0, _, _ -> true
      | _       -> false

    let push t = function
      | n, l1, l2 -> (n+1, t::l1, l2)

    let rec pop = function
      | n, [], []    -> raise Empty
      | n, l1, []    -> pop (n, [], List.rev l1)
      | n, l1, x::l2 -> (n-1, l1, l2), x

    let length = function
      | n, _, _ -> n

    let rec str fmt = function
      | _, [], [] -> ()
      | _, l,  [] -> fprintf fmt "%a" str_list l
      | _, [], l  -> fprintf fmt "%a" str_list (List.rev l)
      | _, l1, l2 -> fprintf fmt "%a, %a" str_list l1 str_list (List.rev l2)
    and str_list fmt = function
      | []   -> ()
      | [e]  -> fprintf fmt "%d" e
      | e::k -> fprintf fmt "%d, %a" e str_list k

    let pprint q = print_string @@ asprintf "%a" str q

  end

let () =
  let q = ref Queue.empty in

  assert (Queue.is_empty !q);

  q := Queue.push 10 !q;
  q := Queue.push 3 !q;
  q := Queue.push 2 !q;
  q := Queue.pop !q |> fst;
  q := Queue.push 1 !q;
  q := Queue.push 0 !q;

  assert (not @@ Queue.is_empty !q);

  Queue.pprint !q
