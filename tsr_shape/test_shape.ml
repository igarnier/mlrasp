open Shape
module Fmt = Format

let%expect_test "pp" =
  let shape = create () in
  let shape' = tensor shape shape in
  let shape'' = tensor shape' (tensor shape (rank_one 1)) in
  Fmt.printf "create: %a@." pp shape ;
  Fmt.printf "tensor [| shape; shape |]: %a@." pp shape' ;
  Fmt.printf "tensor [| shape; shape |]: %a@." pp shape'' ;
  [%expect
    {|
    create: ?
    tensor [| shape; shape |]: [ ? ? ]
    tensor [| shape; shape |]: [ [ ? ? ] [ ? 1 ] ] |}]

let%expect_test "join_exn_idempotent" =
  let shape = create () in
  let shape' = tensor shape shape in
  let shape'' = tensor shape' (tensor shape (rank_one 1)) in
  join_exn shape shape ;
  join_exn shape' shape' ;
  join_exn shape'' shape'' ;
  Fmt.printf "create: %a@." pp shape ;
  Fmt.printf "tensor [| shape; shape |]: %a@." pp shape' ;
  Fmt.printf "tensor [| shape; shape |]: %a@." pp shape'' ;
  [%expect
    {|
    create: ?
    tensor [| shape; shape |]: [ ? ? ]
    tensor [| shape; shape |]: [ [ ? ? ] [ ? 1 ] ] |}]

let%expect_test "join_exn_example1" =
  let a = tensor (tensor (rank_one 1) (rank_one 2)) (rank_one 3) in
  let b = create () in
  let c = tensor a a in
  join_exn a b ;
  Fmt.printf "%a@." pp c ;
  [%expect {|
    [ [ [ 1 2 ] 3 ] [ [ 1 2 ] 3 ] ] |}]

let%expect_test "join_exn_example2" =
  let a = tensor (tensor (rank_one 1) (rank_one 2)) (rank_one 3) in
  let (b, c, d) = (create (), create (), create ()) in
  let e = tensor (tensor b c) d in
  join_exn a e ;
  Fmt.printf "%a@." pp b ;
  Fmt.printf "%a@." pp c ;
  Fmt.printf "%a@." pp d ;
  [%expect {|
    1
    2
    3 |}]

let%expect_test "join_exn_example2" =
  let x = create () in
  let a = tensor (tensor (rank_one 1) x) (tensor x x) in
  let (b, c, d) = (create (), rank_one 42, create ()) in
  let e = tensor (tensor b c) d in
  join_exn a e ;
  Fmt.printf "%a@." pp b ;
  Fmt.printf "%a@." pp c ;
  Fmt.printf "%a@." pp d ;
  [%expect {|
    1
    42
    [ 42 42 ] |}]

let%expect_test "iter_flat_rank_one" =
  let x = rank_one 4 in
  iter x (fun i -> Format.printf "%d@." i) ;
  [%expect {|
    0
    1
    2 |}]

let%expect_test "iter_flat_rank_two_singleton" =
  let x = tensor (rank_one 1) (rank_one 1) in
  iter x (fun (i, j) -> Format.printf "%d, %d" i j) ;
  [%expect {| 0, 0 |}]

let%expect_test "iter_flat_rank_two" =
  let x = tensor (rank_one 1) (rank_one 2) in
  iter x (fun (i, j) -> Format.printf "[%d;%d] "  i  j) ;
  [%expect {| [0;0] [0;1] |}]

let%expect_test "iter_nested_nontrivial" =
  let x =
    tensor (tensor (rank_one 2) (rank_one 2)) (tensor (rank_one 2) (rank_one 2))
  in
  iter x (fun ((i, j), (k, l)) ->
      Format.printf "[%d;%d];[%d;%d]; "  i  j  k  l) ;
  [%expect
    {| [0;0];[0;0]; [0;0];[0;1]; [0;0];[1;0]; [0;0];[1;1]; [0;1];[0;0]; [0;1];[0;1]; [0;1];[1;0]; [0;1];[1;1]; [1;0];[0;0]; [1;0];[0;1]; [1;0];[1;0]; [1;0];[1;1]; [1;1];[0;0]; [1;1];[0;1]; [1;1];[1;0]; [1;1];[1;1]; |}]
