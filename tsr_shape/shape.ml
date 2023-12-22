type 'a desc =
  | Dim : int -> int desc
  | Tensor : 'a t * 'b t -> ('a * 'b) desc
  | Unknown : 'a desc

and 'a t = { mutable uid : 'a Type.Id.t; desc : 'a desc Uf.t }

type (_, _) path =
  | Lcont : ('a * 'b, 'b) path
  | Rcont : ('a * 'b, 'a) path
  | L : ('a, 'x) path -> ('a * 'b, 'x * 'b) path
  | R : ('b, 'x) path -> ('a * 'b, 'a * 'x) path

type (_, _) slice =
  | Lset : 'a -> ('a * 'b, 'b) slice
  | Rset : 'b -> ('a * 'b, 'a) slice
  | L : ('a, 'b) slice -> ('a * 'c, 'b * 'c) slice
  | R : ('a, 'b) slice -> ('c * 'a, 'c * 'b) slice

type (_, _) iso =
  | LAssoc : (('a * 'b) * 'c, 'a * ('b * 'c)) iso
  | RAssoc : ('a * ('b * 'c), ('a * 'b) * 'c) iso
  | Transpose : ('a * 'b, 'b * 'a) iso
  | LMap : ('a, 'b) iso -> ('a * 'c, 'b * 'c) iso
  | RMap : ('a, 'b) iso -> ('c * 'a, 'c * 'b) iso
  | Circ : ('a, 'b) iso * ('b, 'c) iso -> ('a, 'c) iso

type (_, _) diagonal =
  | Diag_base : 'a t -> ('a, 'a) diagonal
  | Diag_cons : ('a, 'b) diagonal -> ('a, 'a * 'b) diagonal

type error =
  | Dimension_mismatch of int * int
  | Undefined
  | Shapes_not_equal : 'a t * 'b t -> error

exception Shape_error of error

let create () = { uid = Type.Id.make (); desc = Uf.create Unknown }

let rank_one dim = { uid = Type.Id.make (); desc = Uf.create (Dim dim) }

let scalar = rank_one 1

let tensor l r = { uid = Type.Id.make (); desc = Uf.create (Tensor (l, r)) }

let get_desc shape = Uf.get shape.desc

let left shape =
  match get_desc shape with Unknown -> None | Tensor (l, _r) -> Some l

let right shape =
  match get_desc shape with Unknown -> None | Tensor (_l, r) -> Some r

let destruct shape =
  match get_desc shape with
  | Unknown -> Error Undefined
  | Tensor (l, r) -> Ok (l, r)

let uid shape = shape.uid

let rec join : type a. a t -> a t -> (unit, error) result =
 fun shape1 shape2 ->
  Result.bind (Uf.join shape1.desc shape2.desc join_desc) @@ fun () ->
  let min_uid =
    if Type.Id.uid shape1.uid < Type.Id.uid shape2.uid then shape1.uid
    else shape2.uid
  in
  shape1.uid <- min_uid ;
  shape2.uid <- min_uid ;
  Ok ()

and join_desc : type a. a desc -> a desc -> (a desc, error) result =
 fun desc1 desc2 ->
  match (desc1, desc2) with
  | (Unknown, desc) -> Ok desc
  | (desc, Unknown) -> Ok desc
  | (Dim i1, Dim i2) ->
      if i1 <> i2 then Error (Dimension_mismatch (i1, i2)) else Ok desc1
  | (Tensor (l1, r1), Tensor (l2, r2)) ->
      Result.bind (join l1 l2) @@ fun () ->
      Result.bind (join r1 r2) @@ fun () -> Ok desc1

let join_exn shape1 shape2 =
  match join shape1 shape2 with Ok () -> () | Error e -> raise (Shape_error e)

let rec numel : type a. a t -> (int, error) result =
 fun shape ->
  match get_desc shape with
  | Unknown -> Error Undefined
  | Dim i -> Ok i
  | Tensor (l, r) ->
      Result.bind (numel l) @@ fun l ->
      Result.bind (numel r) @@ fun r -> Result.ok (l * r)

let rec contract : type a b. a t -> (a, b) path -> (b t, error) result =
 fun shape path ->
  match (get_desc shape, path) with
  | (Unknown, _) -> Error Undefined
  | (Dim _, _) -> .
  | (Tensor (l, r), path) -> (
      match path with
      | Lcont -> Ok r
      | Rcont -> Ok l
      | L p -> Result.bind (contract l p) @@ fun l' -> Result.ok (tensor l' r)
      | R p -> Result.bind (contract r p) @@ fun r' -> Result.ok (tensor l r'))

let equal_eq :
    type a b c d.
    (a, b) Type.eq option ->
    (c, d) Type.eq option ->
    (a * c, b * d) Type.eq option =
 fun left_eq right_eq ->
  match (left_eq, right_eq) with
  | (None, _) | (_, None) -> None
  | (Some Type.Equal, Some Type.Equal) -> Some Type.Equal

let rec assert_equal : type a b. a t -> b t -> ((a, b) Type.eq, error) result =
 fun shape1 shape2 ->
  match (get_desc shape1, get_desc shape2) with
  | (Unknown, _) | (_, Unknown) -> Error Undefined
  | (Dim i1, Dim i2) ->
      if i1 = i2 then (Ok Type.Equal : ((a, b) Type.eq, error) result)
      else Error (Shapes_not_equal (shape1, shape2))
  | (Tensor (l1, r1), Tensor (l2, r2)) -> (
      Result.bind (assert_equal l1 l2) @@ function
      | Type.Equal -> (
          Result.bind (assert_equal r1 r2) @@ function
          | Type.Equal -> Ok (Type.Equal : (a, b) Type.eq)))
  | _ -> Error (Shapes_not_equal (shape1, shape2))

let pp_opt :
    type a.
    (Format.formatter -> a -> unit) -> Format.formatter -> a option -> unit =
 fun pp fmtr opt ->
  match opt with None -> Format.fprintf fmtr "?" | Some elt -> pp fmtr elt

let rec pp : type a. Format.formatter -> a t -> unit =
 fun fmtr shape ->
  match get_desc shape with
  | Unknown -> Format.fprintf fmtr "?"
  | Dim i -> Format.fprintf fmtr "%d" i
  | Tensor (l, r) -> Format.fprintf fmtr "[ %a %a ]" pp l pp r

let rec pp_index : type a. ?shape:a t -> unit -> Format.formatter -> a -> unit =
 fun ?shape () fmtr index ->
  let desc_opt = Option.map get_desc shape in
  pp_opt (fun fmtr desc -> pp_index_desc desc fmtr index) fmtr desc_opt

and pp_index_desc : type a. a desc -> Format.formatter -> a -> unit =
 fun desc fmtr index ->
  match desc with
  | Unknown -> Format.fprintf fmtr "?"
  | Dim _ -> Format.fprintf fmtr "%d" index
  | Tensor (l, r) ->
      let (il, ir) = index in
      Format.fprintf
        fmtr
        "[ %a %a ]"
        (pp_index ~shape:l ())
        il
        (pp_index ~shape:r ())
        ir

let rec pp_path : type a b. Format.formatter -> (a, b) path -> unit =
 fun fmtr path ->
  match path with
  | Lcont -> Format.fprintf fmtr "lcont"
  | Rcont -> Format.fprintf fmtr "rcont"
  | L path -> Format.fprintf fmtr "L(%a)" pp_path path
  | R path -> Format.fprintf fmtr "R(%a)" pp_path path

let rec pp_slice :
    type a b. ?shape:a t -> unit -> Format.formatter -> (a, b) slice -> unit =
 fun ?shape () fmtr slice ->
  match slice with
  | Lset i ->
      let shape = Option.bind shape left in
      Format.fprintf fmtr "lset(%a)" (pp_index ?shape ()) i
  | Rset i ->
      let shape = Option.bind shape right in
      Format.fprintf fmtr "rset(%a)" (pp_index ?shape ()) i
  | L slice ->
      Format.fprintf
        fmtr
        "L(%a)"
        (pp_slice ?shape:(Option.bind shape left) ())
        slice
  | R slice ->
      Format.fprintf
        fmtr
        "R(%a)"
        (pp_slice ?shape:(Option.bind shape right) ())
        slice

let rec pp_iso : type a b. Format.formatter -> (a, b) iso -> unit =
 fun fmtr iso ->
  let open Format in
  match iso with
  | LAssoc -> fprintf fmtr "lassoc"
  | RAssoc -> fprintf fmtr "rassoc"
  | Transpose -> fprintf fmtr "transpose"
  | LMap iso -> fprintf fmtr "lmap(%a)" pp_iso iso
  | RMap iso -> fprintf fmtr "rmap(%a)" pp_iso iso
  | Circ (i1, i2) -> fprintf fmtr "(%a;%a)" pp_iso i1 pp_iso i2

let rec pp_diag :
    type base a. int -> Format.formatter -> (base, a) diagonal -> unit =
 fun depth fmtr diag ->
  match diag with
  | Diag_base shape -> Format.fprintf fmtr "%a^%d" pp shape depth
  | Diag_cons diag -> pp_diag (depth + 1) fmtr diag

let pp_error fmtr error =
  match error with
  | Dimension_mismatch (i, j) ->
      Format.fprintf fmtr "Shape.Dimension_mismatch (%d, %d)" i j
  | Undefined -> Format.fprintf fmtr "Shape.Undefined"
  | Shapes_not_equal (sh1, sh2) ->
      Format.fprintf fmtr "Shape.Shapes_not_equal (%a, %a)" pp sh1 pp sh2

let rec zero : type a. a t -> (a, error) result =
 fun (shape : a t) ->
  match get_desc shape with
  | Unknown -> Error Undefined
  | Dim _ -> Ok 0
  | Tensor (l, r) ->
      Result.bind (zero l) @@ fun zl ->
      Result.bind (zero r) @@ fun zr -> Ok (zl, zr)

let zero_exn shape =
  match zero shape with Ok z -> z | Error e -> raise (Shape_error e)

let rec of_slice : type a b. a t -> (a, b) slice -> (b t, error) result =
 fun shape slice ->
  match slice with
  | Lset _ ->
      let l = create () in
      let r = create () in
      Result.bind (join shape (tensor l r)) @@ fun () -> Ok (r : b t)
  | Rset _ ->
      let l = create () in
      let r = create () in
      Result.bind (join shape (tensor l r)) @@ fun () -> Ok l
  | L slice ->
      let l = create () in
      let r = create () in
      Result.bind (join shape (tensor l r)) @@ fun () ->
      Result.bind (of_slice l slice) @@ fun l -> Result.ok (tensor l r)
  | R slice ->
      let l = create () in
      let r = create () in
      Result.bind (join shape (tensor l r)) @@ fun () ->
      Result.bind (of_slice r slice) @@ fun r -> Result.ok (tensor l r)

module Iso = struct
  let lassoc = LAssoc

  let rassoc = RAssoc

  let tr = Transpose

  let lmap i = LMap i

  let rmap i = RMap i

  let ( |> ) i1 i2 = Circ (i1, i2)
end

let rec apply_iso : type a b. a t -> (a, b) iso -> (b t, error) result =
 fun shape iso ->
  match iso with
  | LAssoc ->
      let a = create () in
      let b = create () in
      let c = create () in
      Result.bind (join shape (tensor (tensor a b) c)) @@ fun () ->
      Ok (tensor a (tensor b c))
  | RAssoc ->
      let a = create () in
      let b = create () in
      let c = create () in
      Result.bind (join shape (tensor a (tensor b c))) @@ fun () ->
      Ok (tensor (tensor a b) c)
  | Transpose ->
      let l = create () in
      let r = create () in
      Result.bind (join shape (tensor l r)) @@ fun () -> Ok (tensor r l)
  | LMap iso ->
      let l = create () in
      let r = create () in
      Result.bind (join shape (tensor l r)) @@ fun () ->
      Result.bind (apply_iso l iso) @@ fun l -> Ok (tensor l r)
  | RMap iso ->
      let l = create () in
      let r = create () in
      Result.bind (join shape (tensor l r)) @@ fun () ->
      Result.bind (apply_iso r iso) @@ fun r -> Ok (tensor l r)
  | Circ (i1, i2) ->
      Result.bind (apply_iso shape i1) @@ fun shape -> apply_iso shape i2

let rec inverse : type a b. (a, b) iso -> (b, a) iso =
 fun iso ->
  match iso with
  | LAssoc -> RAssoc
  | RAssoc -> LAssoc
  | Transpose -> Transpose
  | LMap l -> LMap (inverse l)
  | RMap r -> RMap (inverse r)
  | Circ (i1, i2) -> Circ (inverse i2, inverse i1)

let rec shape_of_diag : type base a. (base, a) diagonal -> base t * a t =
 fun diag ->
  match diag with
  | Diag_base shape -> (shape, shape)
  | Diag_cons diag ->
      let (base, shape) = shape_of_diag diag in
      let shape = tensor base shape in
      (base, shape)

let rec iter : type a. a t -> (a -> unit) -> unit =
 fun shape f ->
  let desc = get_desc shape in
  match desc with
  | Unknown -> raise (Shape_error Undefined)
  | Dim dim ->
      for index = 0 to dim - 1 do
        f index
      done
  | Tensor (l, r) -> iter l (fun i -> iter r (fun j -> f (i, j)))

let rec is_ground : type a. a t -> bool =
 fun shape ->
  match get_desc shape with
  | Unknown -> false
  | Dim _ -> true
  | Tensor (l, r) -> is_ground l && is_ground r

let rec shape_index_eq : type a. a t -> a -> a -> bool option =
 fun s i1 i2 ->
  match get_desc s with
  | Unknown -> None
  | Dim _ -> Some (Int.equal i1 i2)
  | Tensor (l, r) -> (
      let (li1, ri1) = i1 in
      let (li2, ri2) = i2 in
      match (shape_index_eq l li1 li2, shape_index_eq r ri1 ri2) with
      | (None, _) | (_, None) -> None
      | (Some lexpr, Some rexpr) -> Some (lexpr && rexpr))

let shape_index_eq_exn shape i1 i2 =
  match shape_index_eq shape i1 i2 with
  | None -> failwith "shape_index_eq_exn: partial shape"
  | Some res -> res

let rec equal_path : type a b c d. (a, b) path -> (c, d) path -> bool =
 fun p1 p2 ->
  match (p1, p2) with
  | (Lcont, Lcont) -> true
  | (Rcont, Rcont) -> true
  | (L p1, L p2) -> equal_path p1 p2
  | (R p1, R p2) -> equal_path p1 p2
  | _ -> false

let rec equal_slice : type a b c. (a, b) slice -> (a, c) slice -> bool =
 fun sl1 sl2 ->
  match (sl1, sl2) with
  | (Lset i1, Lset i2) ->
      (* FIXME: polymorphic equality on indices *)
      i1 = i2
  | (Rset i1, Rset i2) -> i1 = i2
  | (L sl1, L sl2) -> equal_slice sl1 sl2
  | (R sl1, R sl2) -> equal_slice sl1 sl2
  | _ -> false

let rec equal_iso : type a b c d. (a, b) iso -> (c, d) iso -> bool =
 fun iso1 iso2 ->
  match (iso1, iso2) with
  | (LAssoc, LAssoc) -> true
  | (RAssoc, RAssoc) -> true
  | (Transpose, Transpose) -> true
  | (LMap i1, LMap i2) -> equal_iso i1 i2
  | (RMap i1, RMap i2) -> equal_iso i1 i2
  | (Circ (i1, j1), Circ (i2, j2)) -> equal_iso i1 i2 && equal_iso j1 j2
  | (LAssoc, _) -> false
  | (RAssoc, _) -> false
  | (Transpose, _) -> false
  | (LMap _, _) -> false
  | (RMap _, _) -> false
  | (Circ _, _) -> false

let rec equal_diag : type a b c d. (a, b) diagonal -> (c, d) diagonal -> bool =
 fun diag1 diag2 ->
  match (diag1, diag2) with
  | (Diag_base sh1, Diag_base sh2) -> (
      match Type.Id.provably_equal (uid sh1) (uid sh2) with
      | Some Type.Equal -> true
      | None -> false)
  | (Diag_cons diag1, Diag_cons diag2) -> equal_diag diag1 diag2
  | (Diag_base _, _) -> false
  | (Diag_cons _, _) -> false

let rec flatten : type a. a t -> (int list, error) result =
 fun shape ->
  match get_desc shape with
  | Unknown -> Error Undefined
  | Tensor (l, r) ->
      Result.bind (flatten l) @@ fun l ->
      Result.bind (flatten r) @@ fun r -> Result.ok (l @ r)
  | Dim i -> Result.ok [i]

let flatten shape = Result.map Array.of_list (flatten shape)

let rec flatten_index : type a. a t -> a -> (int list, error) result =
 fun shape index ->
  match (get_desc shape, index) with
  | (Unknown, _) -> Error Undefined
  | (Tensor (l, r), (li, ri)) ->
      Result.bind (flatten_index l li) @@ fun l ->
      Result.bind (flatten_index r ri) @@ fun r -> Ok (l @ r)
  | (Dim _, i) -> Ok [i]

let flatten_index shape index =
  Result.map Array.of_list (flatten_index shape index)
