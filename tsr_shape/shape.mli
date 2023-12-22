type 'a t

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

type error

val create : unit -> 'a t

val rank_one : int -> int t

val scalar : int t

val tensor : 'a t -> 'b t -> ('a * 'b) t

val left : ('a * 'b) t -> 'a t option

val right : ('a * 'b) t -> 'b t option

val destruct : ('a * 'b) t -> ('a t * 'b t, error) result

val uid : 'a t -> 'a Type.Id.t

val join : 'a t -> 'a t -> (unit, error) result

val join_exn : 'a t -> 'a t -> unit

val numel : 'a t -> (int, error) result

val contract : 'a t -> ('a, 'b) path -> ('b t, error) result

val equal_eq :
  ('a, 'b) Type.eq option ->
  ('c, 'd) Type.eq option ->
  ('a * 'c, 'b * 'd) Type.eq option

val assert_equal : 'a t -> 'b t -> (('a, 'b) Type.eq, error) result

val pp_opt :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit

val pp : Format.formatter -> 'a t -> unit

val pp_index : ?shape:'a t -> unit -> Format.formatter -> 'a -> unit

val pp_path : Format.formatter -> ('a, 'b) path -> unit

val pp_slice : ?shape:'a t -> unit -> Format.formatter -> ('a, 'b) slice -> unit

val pp_iso : Format.formatter -> ('a, 'b) iso -> unit

val pp_diag : int -> Format.formatter -> ('base, 'a) diagonal -> unit

val pp_error : Format.formatter -> error -> unit

val zero : 'a t -> ('a, error) result

val zero_exn : 'a t -> 'a

val of_slice : 'a t -> ('a, 'b) slice -> ('b t, error) result

module Iso : sig
  val lassoc : (('a * 'b) * 'c, 'a * ('b * 'c)) iso

  val rassoc : ('a * ('b * 'c), ('a * 'b) * 'c) iso

  val tr : ('a * 'b, 'b * 'a) iso

  val lmap : ('a, 'b) iso -> ('a * 'c, 'b * 'c) iso

  val rmap : ('a, 'b) iso -> ('c * 'a, 'c * 'b) iso

  val ( |> ) : ('a, 'b) iso -> ('b, 'c) iso -> ('a, 'c) iso
end

val apply_iso : 'a t -> ('a, 'b) iso -> ('b t, error) result

val inverse : ('a, 'b) iso -> ('b, 'a) iso

val shape_of_diag : ('base, 'a) diagonal -> 'base t * 'a t

val iter : 'a t -> ('a -> unit) -> unit

val is_ground : 'a t -> bool

val shape_index_eq : 'a t -> 'a -> 'a -> bool option

val shape_index_eq_exn : 'a t -> 'a -> 'a -> bool

val equal_path : ('a, 'b) path -> ('c, 'd) path -> bool

val equal_slice : ('a, 'b) slice -> ('a, 'c) slice -> bool

val equal_iso : ('a, 'b) iso -> ('c, 'd) iso -> bool

val equal_diag : ('a, 'b) diagonal -> ('c, 'd) diagonal -> bool

val flatten : 'a t -> (int array, error) result

val flatten_index : 'a t -> 'a -> (int array, error) result
