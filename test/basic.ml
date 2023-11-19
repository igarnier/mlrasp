open Rasp


let () = Lang.(
    (indices +% (int 1))
    |> eval (String.to_seq "hello")
    |> Seq.iter (fun i ->
        Printf.printf "%d, %!" i)
  )

let () = print_newline ()

let () = Lang.(
    tokens_if (indices mod 2 =% (int 0)) (char '-')
    |> eval (String.to_seq "hello")
    |> String.of_seq
    |> Printf.printf "%s%!"
  )

let () = print_newline ()

let () = Lang.(
    aggregate
      (select indices indices (<))
      (to_float (indices +% (int 1)))
    |> eval (String.to_seq "hey")
    |> Seq.iter (fun f ->
        Printf.printf "%f, %!" f)
  )

let () = print_newline ()

(* Reverse *)

let () =
  let open Lang in
  aggregate
    (select indices (length -% indices -% int 1) Int.equal)
    (to_float indices)
  |> to_int
  |> load
  |> eval (String.to_seq "dummy")
  |> String.of_seq
  |> Printf.printf "%s%!"

let () = print_newline ()

(* Length *)

let () =
  let open Lang in
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

let () =
  let open Lang in
  aggregate
    (select id id Char.equal)
    (float 1.)
  |> eval (String.to_seq "dummy")
  |> Seq.iter (fun f ->
      Printf.printf "%f, %!" f)
