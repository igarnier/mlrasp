open Rasp


let () = Lang.(
    (indices (input ()) +% (int 1))
    |> eval (String.to_seq "hello")
    |> Seq.iter (fun i ->
        Printf.printf "%d, %!" i)
  )

let () = print_newline ()

let () = Lang.(
    if_ ((indices (input ())) mod (int 2) =% (int 0)) (input ()) (char '-')
    |> eval (String.to_seq "hello")
    |> String.of_seq
    |> Printf.printf "%s%!"
  )

let () = print_newline ()

let () = Lang.(
    let expr = indices (input ()) in
    aggregate
      (select expr expr Lt)
      (to_float (expr +% (int 1)))
    |> eval (String.to_seq "hey")
    |> Seq.iter (fun f ->
        Printf.printf "%f, %!" f)
  )

let () = print_newline ()

(* Reverse *)

let () =
  let open Lang in
  let indices = indices (input ()) in
  let length = length (input ()) in
  aggregate
    (select indices (length -% indices -% int 1) Equal)
    (to_float indices)
  |> to_int
  |> eval (String.to_seq "dummy")
  |> fun seq ->
  Format.printf "%a@." (Format.pp_print_seq Format.pp_print_int) seq

let () = print_newline ()

(* Length *)

let () =
  let open Lang in
  let indices = indices (input ()) in
  (* Why it works:
     - indicator (indices = 0) == 1 0 0 0 ... 0 of length [L]
     - select_all is true everywhere
     - hence the outcome of aggregate for each row is (1 + 0 + 0 + ... + 0) / (number of true) = 1 / L
     - and trivially 1 / (1 / L) = L
  *)
  float 1. /%. aggregate (select_all ()) (indicator (indices =% (int 0)) |> to_float)
  |> eval (String.to_seq "hey")
  |> Seq.iter (fun f ->
      Printf.printf "len = %f, %!" f)

let () = print_newline ()

(* Histogram *)

(*let () =
  let open Lang in
  aggregate
    (select id id Char.equal)
    (float 1.)
  |> eval (String.to_seq "dummy")
  |> Seq.iter (fun f ->
      Printf.printf "%f, %!" f)
*)
