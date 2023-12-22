type 'a t

val create : 'a -> 'a t

val set : 'a t -> 'a -> unit

val get : 'a t -> 'a

val join : 'a t -> 'a t -> ('a -> 'a -> ('a, 'b) result) -> (unit, 'b) result

val join_heterogeneous :
  'a t ->
  'b t ->
  ('a -> 'b -> ('a * ('a, 'b) Type.eq, 'error) result) ->
  (('a, 'b) Type.eq, 'error) result
