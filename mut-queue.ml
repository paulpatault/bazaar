module Queue : sig
  type cont = int
  type t = { mutable n : int; mutable left : cont list ; mutable right : cont list }
  val empty : unit -> t
  val is_empty : t -> bool
  val push : cont -> t -> unit
  val pop : t -> cont
  val pp : t -> unit

end = struct

    exception Empty

    type cont = int

    type t = { mutable n : int; mutable left : cont list ; mutable right : cont list }

    let empty () = { n = 0; left = []; right = [] }

    let is_empty {n;_} = n = 0

    let push t p =
      p.n <- p.n + 1;
      p.left <- t::p.left

    let rec pop t = match t.left, t.right with
      | [], []    -> raise Empty
      | l1, []    ->
          t.left <- [];
          t.right <- List.rev l1;
          pop t
      | l1, x::l2 ->
          t.n <- t.n-1;
          t.right <- l2;
          x

    let length t = match t.contents with
      | n, _, _ -> n

    let rec str fmt {left;right;_} = let open Format in
      left @ List.rev right |> fprintf fmt "%a" (pp_print_list pp_print_int)

    let pp q = Format.printf "%a" str q

  end

let () =
  let q = Queue.empty () in

  assert (Queue.is_empty q);

  Queue.push 10 q;
  Queue.push 3 q;
  Queue.push 2 q;
  Queue.push 1 q;
  Queue.push 0 q;

  assert (not @@ Queue.is_empty q);

  Queue.pp q
