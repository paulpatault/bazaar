type void
= V of (void -> void)

let i (f:void -> void): void
= V f

let r (v:void) : void -> void
= match v with
    V f -> f

let delta
= i (fun x:void -> r x x)

(* ne termine pas *)
let omega
= r delta delta

(* plus courte fonction de 'a -> 'b *)
let f x=exit 0
let rec f x=f x

(* cast louche *)
type t = [`A | `B]
type s = [`A]

let f x = (x : s :> t)

(* quine *)
let s = "let s = %S\nlet () = Format.printf %S s s@."
let () = Format.printf "let s = %S\nlet () = Format.printf %S s s@." s s

let _ =
(fun s -> Printf.printf "%s%S;;\n" s s) "(fun s -> Printf.printf \"%s%S;;\\n\" s s) ";;

