(* V1 *)
type void = V of (void -> void)
let i (f:void -> void): void = V f
let r (v:void) : void -> void = match v with V f -> f
let delta = i (fun x:void -> r x x)
let omega = r delta delta

(* V2 *)
type t = F of (t -> t)
let app (F f ) x = f x
let delta = F ( fun x -> app x x )
let _ = app delta delta

(* V3 *)
let r = ref (fun _ -> 42)
let () = r := (fun () -> !r ())
let _ = !r ()
let rec x = 1 :: x and g = 0

(* > echo $? *)
let(_):unit=let(_::_')|_'=[(@)]in(match(_')with|[(@)]->4|[]->42|_->2)|>exit;;

(* plus courte fonction de 'a -> 'b *)
let f x=exit 0

(* cast louche *)
type t = [`A | `B]
type s = [`A]
let f x = (x : s :> t)

(* quine *)
let s = "let s = %S\nlet () = Format.printf %S s s@."
let () = Format.printf "let s = %S\nlet () = Format.printf %S s s@." s s

let _ =
(fun s -> Printf.printf "%s%S;;\n" s s) "(fun s -> Printf.printf \"%s%S;;\\n\" s s) ";;
