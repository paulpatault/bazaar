(* #load "unix.cma";; *)

module PetriNet = struct

  type place = string
  type flow_t = place list * place list * string
  type value = Int of int | Inf
  type marking = value array

  type net = {
    places: place list;
    flow: flow_t list;
    marking: marking
  }

  let pp (net:net) : string =
    let s = ref (Printf.sprintf "⟨") in
    Array.iteri (fun i e ->
      begin match e with
      | Int n -> s := !s ^ (string_of_int n)
      | Inf -> s := !s ^ "w"
      end;
      if i <> Array.length net.marking - 1 then
        s := !s ^ ", "
    ) net.marking;
    !s ^ "⟩"

  let ppm m : string =
    let s = ref (Printf.sprintf "⟨") in
    Array.iteri (fun i e ->
      begin match e with
      | Int n -> s := !s ^ (string_of_int n)
      | Inf -> s := !s ^ "w"
      end;
      if i <> Array.length m - 1 then
        s := !s ^ ", "
    ) m;
    !s ^ "⟩"

  let mk ?places ?flow ?marking net : net =
    let places      = Option.value ~default:net.places      places in
    let flow        = Option.value ~default:net.flow        flow in
    let marking     = Option.value ~default:net.marking     marking in
    {places;flow;marking}

  let fire net =
    let cond left arr =
      List.for_all (fun e ->
        match arr.(int_of_string e - 1) with
        | Int 0 -> false
        | _ -> true ) left
    in

    let fire_one left right mark =
      let mark = Array.copy mark in
      List.iter (fun e ->
        match mark.(int_of_string e - 1) with
        | Int x ->
            mark.(int_of_string e - 1) <- Int (x - 1)
        | _ -> ()
      ) left;
      List.iter (fun e ->
        match mark.(int_of_string e - 1) with
        | Int x ->
            mark.(int_of_string e - 1) <- Int (x + 1)
        | _ -> ()
      ) right;
      mark
    in

    List.fold_left (
      fun acc (left, right, name) ->
        if cond left net.marking then begin
          let marking = fire_one left right net.marking in
          let net = mk ~marking net in
          (net, name) :: acc
        end else acc
    ) [] net.flow;
end

open PetriNet

module S = Set.Make(struct
    type t = net
    let compare n1 n2 = Stdlib.compare n1.marking n2.marking
end)

let h = Hashtbl.create 15

let net =
  let places = ["1";"2";"3";"4"] in
  let t1 = ["2";"4"], ["2";"1"], "t1" in
  let t2 = ["1"], ["3"], "t2" in
  let t3 = ["2"], ["4"], "t3" in
  let t4 = ["3"], ["3";"4"], "t4" in
  let flow = [t1;t2;t3;t4] in
  let marking = [|Int 1; Int 1; Int 0; Int 0|] in
  { places; flow; marking; }

(* let net =
  let places = ["1";"2";"3";"4"] in
  let t1 = ["1";"3"], ["2";"1"], "t1" in
  let t2 = ["1"], ["3"], "t2" in
  let t3 = ["4"], ["2";"1"], "t3" in
  let t4 = ["2";"3"], ["4"], "t4" in
  let flow = [t1;t2;t3;t4] in
  let marking = [|Int 1; Int 0; Int 0; Int 1|] in
  { places; flow; marking; } *)


let rec fire_loop net mem =
  let cond net =
    Array.for_all (function
      | Int 0 | Int 1 -> true
      | _ -> false
    ) net.marking
  in

  if cond net then
    let nl = fire net in
    Hashtbl.add h net nl;
    let mem =
      List.fold_left (fun acc (e, _) ->
        let acc = S.add e acc in
        fire_loop e acc
    ) mem nl in
    mem
  else mem

(* let _ =
  let out = open_out "dm9.dot" in
  Printf.fprintf out
"digraph name {
    rankdir=LR;
    node [shape = ellipse];\n";

  let mem = S.singleton net in
  let _ = fire_loop net mem in
  Hashtbl.iter (fun k v ->
    let k = pp k in
    List.iter (fun (e, name) ->
      Printf.fprintf out "    \"%s\" -> \"%s\" [label = %s]\n" k (pp e) name;
    ) v
  ) h;
  Printf.fprintf out "}";
  close_out out *)

(* TODO : poitn fixe *)

let inf_val = function
    | Int x, Int y -> x <= y
    | Int x, Inf   -> true
    | Inf, Inf     -> true
    | _  -> false

let inf_arr m1 m2 =
    if m1 = m2 then false
    else Array.for_all2 (fun a b -> inf_val (a, b)) m1 m2

let coverability_graph net =
    let verti = ref [Array.copy net.marking] in
    let edges = ref [] in
    let v0 = Array.copy net.marking in
    let work = ref [Array.copy net.marking] in


    let has_path m1 m2 =
        List.exists (fun (m1', _, m2') ->
            (m1 = m1' && m2 = m2')
        ) !edges
    in

    let rec add_omegas m m' v =
        let saved = Array.copy m' in
        let m' = ref m' in
        List.iter (fun m'' ->
            if has_path m'' m && inf_arr m'' !m' then begin
                let arr = Array.copy m in
                let i = ref 0 in
                Array.iter2 (fun em' em'' ->
                    begin match em', em'' with
                    | Inf, Inf
                    | Inf, Int _ ->
                        arr.(!i) <- Inf
                    | Int x, Int y ->
                        if x = y then begin
                            arr.(!i) <- Int x
                        end else begin
                            arr.(!i) <- Inf
                        end
                    | _ -> assert false
                    end;
                    incr i;
                ) !m' m'';
                m' := arr;
            end
        ) v;
        if saved = !m' then saved
        else add_omegas (Array.copy m) (Array.copy !m') v
    in

    (* let i = ref 0 in *)
    while !work <> [] do begin
        (* Printf.printf "%5d ::" !i; incr i;
        List.iter (fun e -> Printf.printf "%s " (ppm e)) !work;
        print_newline ();
        (* Unix.sleepf 1.; *)
        flush_all (); *)
        match !work with
        | [] -> assert false
        | m::t ->
            work := t;
            let nl = fire (mk net ~marking:m) in
            List.iter (fun (net, tr) ->
                let m' = net.marking in
                let m' = add_omegas (Array.copy m) (Array.copy m') !verti in
                if not (List.mem m' !verti)
                then begin
                    verti := Array.copy m' :: !verti;
                    work := Array.copy m' :: !work;
                end;
                edges := (m, tr, m') :: !edges
            ) nl
    end done;
    !verti, !edges, v0


let replace x by_y arr =
    Array.map (fun (v1, m, v2) ->
        if v1 = x then
            if v2 = x then by_y, m, by_y
            else by_y, m, v2
        else if v2 = x then v1, m, by_y
        else v1, m, v2
    ) arr

let exists_plus_grand x arr =
    Array.fold_left (fun acc (v1, t, v2) ->
        if inf_arr x v1 then
            Some v1
        else if inf_arr x v2 then
            Some v2
        else acc
    ) None arr

let _ =
  let out = open_out "coverability_graph.dot" in
  let _v, e, _v0 = coverability_graph net in
  Printf.fprintf out
"digraph name {
    rankdir=LR;
    node [shape = ellipse];\n";

  (* let arr_edges = Array.of_list e in
  let cpy = ref (Array.copy arr_edges) in

  Array.iter (fun (v1, p, v2) ->
      match exists_plus_grand v1 arr_edges with
      | None -> ()
      | Some g -> cpy := (replace v1 g !cpy);
      match exists_plus_grand v2 arr_edges with
      | None -> ()
      | Some g -> cpy := (replace v2 g !cpy)
  ) arr_edges; *)

  List.iter (fun (m1, tr, m2) ->
      if List.mem m1 _v then begin
      if List.mem m2 _v then begin
          let m1 = ppm m1 in
          let m2 = ppm m2 in
          if String.contains m1 '2' then ()
          else if String.contains m1 '3' then ()
          else if String.contains m2 '2' then ()
          else if String.contains m2 '3' then ()
          else Printf.fprintf out "    \"%s\" -> \"%s\" [label = %s]\n" m1 m2 tr
      end end
  ) e;

  (* Array.iter (fun (m1, tr, m2) ->
      if List.mem m1 _v then begin
      if List.mem m2 _v then begin
          let m1 = ppm m1 in
          let m2 = ppm m2 in
          Printf.fprintf out "    \"%s\" -> \"%s\" [label = %s]\n" m1 m2 tr
      end end
  ) !cpy; *)

  Printf.fprintf out "}";
  close_out out
