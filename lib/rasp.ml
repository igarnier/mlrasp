module Dim = struct
  type t = int option Tsr_shape.Uf.t

  let join ?(loc = "") (a : t) (b : t) =
    Tsr_shape.Uf.join a b (fun opt1 opt2 ->
        match (opt1, opt2) with
        | (None, opt) | (opt, None) -> Ok opt
        | (Some i1, Some i2) -> if i1 = i2 then Ok opt1 else Error ())
    |> function
    | Error () -> Format.kasprintf failwith "join_error: %s" loc
    | Ok _ -> ()

  let unknown () = Tsr_shape.Uf.create None

  let known i = Tsr_shape.Uf.create (Some i)

  let numel_exn ?(loc = "") shape =
    match Tsr_shape.Uf.get shape with
    | None -> Format.kasprintf failwith "numel_exn: shape undefined (%s)" loc
    | Some i -> i
end

module Lang = struct
  module S = Tsr_shape.Shape

  type 'ret desc =
    | Input : char desc
    | Indices : 'a t -> int desc
    | Length : 'a t -> int desc
    | To_float : int t -> float desc
    | To_int : float t -> int desc
    | Indicator : bool t -> int desc
    | Char : char -> char desc
    | Int : int -> int desc
    | Float : float -> float desc
    | Binop : ('a, 'b, 'c) binop * 'a t * 'b t -> 'c desc
    | Aggregate : selector * float t -> float desc
    | Cond : bool t * 'a t * 'a t -> 'a desc

  and ('a, 'b, 'c) binop =
    | Equal : ('a, 'a, bool) binop
    | Lt : ('a, 'a, bool) binop
    | IAdd : (int, int, int) binop
    | ISub : (int, int, int) binop
    | IMul : (int, int, int) binop
    | IDiv : (int, int, int) binop
    | FAdd : (float, float, float) binop
    | FSub : (float, float, float) binop
    | FMul : (float, float, float) binop
    | FDiv : (float, float, float) binop
    | Mod : (int, int, int) binop

  and selector =
    | Selector :
        { keys : 'a t; queries : 'b t; op : ('a, 'b, bool) binop }
        -> selector

  and 'ret t = { shape : Dim.t; desc : 'ret desc }

  let make shape desc = { shape; desc }

  let select keys queries op = Selector { keys; queries; op }

  let eval_binop : type a b c. (a, b, c) binop -> a -> b -> c =
   fun op x y ->
    match op with
    | Equal -> x = y
    | Lt -> x < y
    | IAdd -> x + y
    | ISub -> x - y
    | IMul -> x * y
    | IDiv -> x / y
    | FAdd -> x +. y
    | FSub -> x -. y
    | FMul -> x *. y
    | FDiv -> x /. y
    | Mod -> x mod y

  let rec apply_shape_constraints : type a. a t -> int -> unit =
   fun rasp numel ->
    match rasp.desc with
    | Input -> Dim.join rasp.shape (Dim.known numel)
    | Indices p ->
        Dim.join rasp.shape p.shape ;
        apply_shape_constraints p numel
    | Length p ->
        Dim.join rasp.shape p.shape ;
        apply_shape_constraints p numel
    | To_float p ->
        Dim.join rasp.shape p.shape ;
        apply_shape_constraints p numel
    | To_int p ->
        Dim.join rasp.shape p.shape ;
        apply_shape_constraints p numel
    | Indicator p ->
        Dim.join rasp.shape p.shape ;
        apply_shape_constraints p numel
    | Char _ -> ()
    | Int _ -> ()
    | Float _ -> ()
    | Binop (_, l, r) ->
        Dim.join l.shape r.shape ;
        Dim.join l.shape rasp.shape ;
        apply_shape_constraints l numel ;
        apply_shape_constraints r numel
    | Aggregate (selector, v) ->
        let (Selector { keys; queries; op = _ }) = selector in
        Dim.join v.shape keys.shape ;
        Dim.join rasp.shape queries.shape ;
        apply_shape_constraints keys numel ;
        apply_shape_constraints queries numel ;
        apply_shape_constraints v numel
    | Cond (cond, ift, iff) ->
        Dim.join cond.shape ift.shape ;
        Dim.join cond.shape iff.shape ;
        Dim.join rasp.shape ift.shape ;
        apply_shape_constraints cond numel ;
        apply_shape_constraints ift numel ;
        apply_shape_constraints iff numel

  let rec eval : type a. char Seq.t -> a t -> a Seq.t =
   fun input rasp ->
    match rasp.desc with
    | Input -> input
    | Indices p -> eval input p |> Seq.mapi (fun i _ -> i)
    | Length p ->
        let len = Seq.length (eval input p) in
        Seq.init len (fun _ -> len)
    | To_float p -> eval input p |> Seq.map float_of_int
    | To_int p -> eval input p |> Seq.map int_of_float
    | Indicator p -> eval input p |> Seq.map (fun b -> if b then 1 else 0)
    | Char c ->
        let numel = Dim.numel_exn ~loc:__LOC__ rasp.shape in
        Seq.init numel (fun _ -> c)
    | Int i ->
        let numel = Dim.numel_exn ~loc:__LOC__ rasp.shape in
        Seq.init numel (fun _ -> i)
    | Float f ->
        let numel = Dim.numel_exn ~loc:__LOC__ rasp.shape in
        Seq.init numel (fun _ -> f)
    | Binop (op, l, r) -> Seq.map2 (eval_binop op) (eval input l) (eval input r)
    | Aggregate (selector, v) ->
        let (Selector { keys; queries; op }) = selector in
        let keys = eval input keys |> Array.of_seq in
        let queries = eval input queries in
        let vec = eval input v |> Array.of_seq in
        Seq.map
          (fun qv ->
            let acc = ref 0.0 in
            let selected = ref 0 in
            Array.iter2
              (fun vec_c lhs_c ->
                let b = eval_binop op lhs_c qv in
                let v =
                  if b then (
                    incr selected ;
                    vec_c)
                  else 0.0
                in
                acc := !acc +. v)
              vec
              keys ;
            if !selected = 0 then 0.0 else !acc /. float_of_int !selected)
          queries
    | Cond (cond, ift, iff) ->
        let cond = eval input cond in
        let ift = eval input ift in
        let iff = eval input iff in
        Seq.zip (Seq.zip cond ift) iff
        |> Seq.map (fun ((c, t), f) -> if c then t else f)

  let eval input rasp =
    apply_shape_constraints rasp (Seq.length input) ;
    eval input rasp

  let input () = make (Dim.unknown ()) Input

  let indices p = make p.shape (Indices p)

  let length p = make p.shape (Length p)

  let to_float p = make p.shape (To_float p)

  let to_int p = make p.shape (To_int p)

  let indicator p = make p.shape (Indicator p)

  let char c = make (Dim.unknown ()) (Char c)

  let int i = make (Dim.unknown ()) (Int i)

  let float f = make (Dim.unknown ()) (Float f)

  let binop op l r = make l.shape (Binop (op, l, r))

  let ( =% ) tr1 tr2 = binop Equal tr1 tr2

  let ( -% ) tr1 tr2 = binop ISub tr1 tr2

  let ( -%. ) tr1 tr2 = binop FSub tr1 tr2

  let ( +% ) tr1 tr2 = binop IAdd tr1 tr2

  let ( +%. ) tr1 tr2 = binop FAdd tr1 tr2

  let ( *% ) tr1 tr2 = binop IMul tr1 tr2

  let ( *%. ) tr1 tr2 = binop FMul tr1 tr2

  let ( /% ) tr1 tr2 = binop IDiv tr1 tr2

  let ( /%. ) tr1 tr2 = binop FDiv tr1 tr2

  let ( mod ) tr1 tr2 = binop Mod tr1 tr2

  let if_ cond ift iff = make cond.shape (Cond (cond, ift, iff))

  let tokens_if condition alt seq =
    let cond = condition seq in
    let alt = alt seq in
    Seq.map2
      (fun b (alt, original) -> if b then original else alt)
      cond
      (Seq.zip alt seq)

  let aggregate selector v =
    let (Selector { keys = _; queries; op = _ }) = selector in
    let out_dim = queries.shape in
    Dim.join v.shape out_dim ;
    make out_dim (Aggregate (selector, v))

  (* A function allowing to load a given character from a string *)
end

(* true everywhere *)
let select_all () = Lang.(select (int 1) (int 1) Equal)
