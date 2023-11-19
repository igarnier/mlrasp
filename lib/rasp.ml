(* The semantics of a term of type [('a, 'b) Lang.t] is a sequence transformer ['a Seq.t -> 'b Seq.t] *)

module Lang : sig
  type ('a, 'b) t

  type ('a, 'b) selector

  val string : string -> (unit, char) t

  val id : ('a, 'a) t

  val ( mod ) : ('a, int) t -> int -> ('a, int) t

  val length : ('a, int) t

  val indices : ('a, int) t

  val to_float : ('a, int) t -> ('a, float) t

  val to_int : ('a, float) t -> ('a, int) t

  val indicator : ('a, bool) t -> ('a, int) t

  val char : char -> ('a, char) t

  val int : int -> ('a, int) t

  val float : float -> ('a, float) t

  val binop : ('b -> 'c -> 'd) -> ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t

  val ( =% ) : ('a, 'b) t -> ('a, 'b) t -> ('a, bool) t

  val ( +% ) : ('a, int) t -> ('a, int) t -> ('a, int) t

  val ( +%. ) : ('a, float) t -> ('a, float) t -> ('a, float) t

  val ( -% ) : ('a, int) t -> ('a, int) t -> ('a, int) t

  val ( -%. ) : ('a, float) t -> ('a, float) t -> ('a, float) t

  val ( *% ) : ('a, int) t -> ('a, int) t -> ('a, int) t

  val ( *%. ) : ('a, float) t -> ('a, float) t -> ('a, float) t

  val ( /% ) : ('a, int) t -> ('a, int) t -> ('a, int) t

  val ( /%. ) : ('a, float) t -> ('a, float) t -> ('a, float) t

  val load : ('a, int) t -> ('a, 'a) t

  val tokens_if : ('a, bool) t -> ('a, 'a) t -> ('a, 'a) t

  val select : ('a, 'b) t -> ('a, 'b) t -> ('b -> 'b -> bool) -> ('a, 'b) selector

  (* val select_or : ('a, 'b) selector -> ('a, 'b) selector -> ('a, 'b) selector *)

  val aggregate : ('a, 'b) selector -> ('a, float) t -> ('a, float) t

  val eval : 'a Seq.t -> ('a, 'b) t -> 'b Seq.t
end =
struct
  type ('a, 'b) t = 'a Seq.t -> 'b Seq.t

  let string s = fun _dummy ->
    String.to_seq s

  let id = Fun.id

  let ( mod ) tr modulo =
    fun seq ->
    Seq.map (fun x -> x mod modulo) (tr seq)

  let length seq =
    let len = Seq.length seq in
    Seq.init len (fun _ -> len)

  let char c = fun _seq -> Seq.forever (Fun.const c)

  let int i = fun _seq -> Seq.forever (Fun.const i)

  let float f = fun _seq -> Seq.forever (Fun.const f)

  let indices seq =
    Seq.mapi (fun i _ -> i) seq

  let to_float tr =
    fun seq -> Seq.map float_of_int (tr seq)

  let to_int tr =
    fun seq -> Seq.map int_of_float (tr seq)

  let indicator tr =
    fun seq -> Seq.map (fun b -> if b then 1 else 0) (tr seq)

  let binop f tr1 tr2 seq = Seq.map2 f (tr1 seq) (tr2 seq)

  let ( =% ) tr1 tr2 = binop (=) tr1 tr2

  let ( -% ) tr1 tr2 = binop (-) tr1 tr2

  let ( -%. ) tr1 tr2 = binop (-.) tr1 tr2

  let ( +% ) tr1 tr2 = binop (+) tr1 tr2

  let ( +%. ) tr1 tr2 = binop (+.) tr1 tr2

  let ( *% ) tr1 tr2 = binop ( * ) tr1 tr2

  let ( *%. ) tr1 tr2 = binop ( *. ) tr1 tr2

  let ( /% ) tr1 tr2 = binop ( / ) tr1 tr2

  let ( /%. ) tr1 tr2 = binop ( /. ) tr1 tr2

  let tokens_if condition alt = fun seq ->
    let cond = condition seq in
    let alt = alt seq in
    Seq.map2 (fun b (alt, original) ->
        if b then
          original
        else
          alt
      ) cond (Seq.zip alt seq)

  let load tr = fun seq ->
    let elts = Array.of_seq seq in
    let indices = tr seq in
    Seq.map (fun i ->
        elts.(i)
      ) indices
  
  type (_, _) selector = Selector : { lhs : ('a, 'b) t ;
                                      rhs : ('a, 'b) t ;
                                      op : 'b -> 'b -> bool } -> ('a, 'b) selector

  let select lhs rhs op = Selector { lhs ; rhs ; op }

  (* We could have non-square selectors but the current implementation is a bit too
     naive. We allow infinite sequences (see def. of [select_all]) and in order to ward
     off divergence, we truncate the rows to match the number of columns. *)
  let aggregate (Selector { lhs; rhs; op }) tr =
    fun seq ->
    (* Using a [zip] implicitly truncates the sequence of pairs to the length of the shortest argument. *)
    let cols = Seq.zip (tr seq) (lhs seq) |> Array.of_seq in
    (* let vec = tr seq |> Array.of_seq in *)
    (* let lhs = lhs seq |> Seq.take (Array.length vec) |> Array.of_seq in *)
    let f rv =
      (* Average over each row *)
      let acc = ref 0.0 in
      let selected = ref 0 in
      Array.iter (fun (vec_c, lhs_c) ->
        let b = op lhs_c rv in
        let v =
          if b then
            (incr selected ;
             vec_c)
          else
            0.0
        in
        acc := !acc +. v
      ) cols ;
      if !selected = 0 then
        0.0
      else
        !acc /. (float_of_int !selected)
    in
    rhs seq
    |> Seq.map f
    |> Seq.take (Array.length cols)

  let eval seq f = f seq
end

(* true everywhere *)
let select_all () = Lang.(select (int 1) (int 1) Int.equal)
