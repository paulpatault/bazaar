let diag e =
  let dg = String.make 1 (Char.chr 34) in
  String.split_on_char (Char.chr 120) e
  |> String.concat (dg ^ e ^ dg)

let _ =
  "let diag e =
  let dg = String.make 1 (Char.chr 34) in
  String.split_on_char (Char.chr 120) e
  |> String.concat (dg ^ e ^ dg)

let _ =
  x
  |> diag |> print_endline"
  |> diag |> print_endline
