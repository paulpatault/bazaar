module String2 = struct
  type t = string * string option
  let compare (s1, _) (s2, _) = String.compare s1 s2
end

module Graph = struct

  module G = Map.Make(String)
  module S = Set.Make(String2)

  type set = S.t
  type graph = set G.t
  type node = string

  let find (x:node) (g:graph): set =
    match G.find_opt x g with
    | None -> S.empty
    | Some s -> s

  let degree (x:node) (g:graph): int =
    S.cardinal (find x g)

  let add_node (x:node) (g:graph): graph =
    if G.mem x g then g
    else G.add x S.empty g

  let update_node (x:node) (f:set -> set) (g:graph): graph =
    G.add x (f (find x g)) g

  let add_edge_undir ?(str:string option) (x:node) (y:node) (g:graph): graph =
    update_node x (S.add (y, str)) g
    |> update_node y (S.add (x, str))

  let add_edge_dir ?(str:string option) (x:node) (y:node) (g:graph): graph =
    update_node x (S.add (y, str)) g


  let remove_node (x:node) (g:graph): graph =
    S.fold (fun (y, lab) -> update_node y (S.remove (x, lab))) (G.find x g) g
    |> G.remove x

  let pp ?(filename="graph.dot") (g:graph) : unit =
    let out = open_out filename in
    Printf.fprintf out
"digraph name {
    rankdir=LR;
    node [shape = ellipse];\n";

    G.iter (fun node edges ->
        S.iter (function
            | n, Some lab ->
                  Printf.fprintf out "    %s -> %s [label = %s]\n" node n lab
            | n, None ->
                assert false
                  (* Printf.fprintf out "    %s -> %s\n" node n *)
        ) edges;
    ) g;

    Printf.fprintf out "}";
    close_out out
end


let out = open_out "sol.dot"

let tbl = ref []

let t1 (p1, p2, p3, p4) =
  if p2 > 0 && p4 > 0 then
    p1+1, p2, p3, p4-1
  else
    p1, p2, p3, p4

let t2 (p1, p2, p3, p4) =
  if p1 > 0 then
    p1-1, p2, p3+1, p4
  else
    p1, p2, p3, p4

let t3 (p1, p2, p3, p4) =
  if p2 > 0 then
    p1, p2-1, p3, p4+1
  else
    p1, p2, p3, p4

let t4 (p1, p2, p3, p4) =
  if p3 > 0 then
    p1, p2, p3, p4+1
  else
    p1, p2, p3, p4

let pp (p1, p2, p3, p4) =
  Printf.sprintf "\"⟨%d,%d,%d,%d⟩\"" p1 p2 p3 p4

let cond state =
  let a,b,c,d = state in
  a < 2 && b < 2 && c < 2 && d < 2
  && not @@ List.mem state !tbl

let g = ref Graph.G.empty

let rec steps state =
  if cond state then begin
    tbl := state :: !tbl;
    step t1 "t1" state;
    step t2 "t2" state;
    step t3 "t3" state;
    step t4 "t4" state
  end
and step t_i str pre =
  let post = t_i pre in
  if pre <> post then begin
      g := Graph.add_node (pp pre) !g;
      g := Graph.add_node (pp post) !g;
      g := Graph.add_edge_dir ~str (pp pre) (pp post) !g;
    Printf.fprintf out "    %s -> %s [label = %s]\n" (pp pre) (pp post) str;
    steps post
  end


let _ =
  Printf.fprintf out
"digraph name {
    rankdir=LR;
    node [shape = ellipse];\n";

  let init = 1,1,0,0 in
  steps init;
  Graph.pp !g;
  Printf.fprintf out "}";
  close_out out

