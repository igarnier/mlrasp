(* Union-find *)

type 'a root = { mutable data : 'a; mutable rank : int }

type 'a node = Root of 'a root | Parent of 'a pointer

and 'a pointer = 'a node ref

type 'a t = 'a pointer

let create data = ref @@ Root { data; rank = 0 }

(* [get_root ptr] returns [(root, root_ptr)] where:
   - [root] is the root of type [root] that can be found by traversing
     the parent tree from [ptr] node.
   - [root_ptr] contains the [pointer] that points to that [root].
   This also flattens the whole branch from [ptr] to the root, attaching
   each traversed [pointer] to [root_ptr]. *)
let rec get_root ptr =
  match !ptr with
  | Root root -> (root, ptr)
  | Parent parent ->
      let (root, root_ptr) = get_root parent in
      ptr := Parent root_ptr ;
      (root, root_ptr)

let set (t : 'a t) (data : 'a) =
  let (root, _) = get_root t in
  root.data <- data

let get (t : 'a t) : 'a =
  let (root, _) = get_root t in
  root.data

let join (t1 : 'a t) (t2 : 'a t) f =
  let (root1, ptr1) = get_root t1 in
  let (root2, ptr2) = get_root t2 in
  (* If the roots are physically equal, it means [t1] and [t2] belong to the
     same equivalence class. If that is the case, we don't do anything. *)
  if root1 != root2 then
    let new_root =
      if root1.rank < root2.rank then (
        ptr1 := Parent ptr2 ;
        root2)
      else (
        ptr2 := Parent ptr1 ;
        if root1.rank = root2.rank then root1.rank <- root1.rank + 1 ;
        root1)
    in
    match f root1.data root2.data with
    | Ok data ->
        new_root.data <- data ;
        Ok ()
    | Error e -> Error e
  else Ok ()

let join_heterogeneous :
    type a b.
    a t ->
    b t ->
    (a -> b -> (a * (a, b) Type.eq, 'error) result) ->
    ((a, b) Type.eq, 'error) result =
 fun (t1 : a t) (t2 : b t) f ->
  let (root1, ptr1) = get_root t1 in
  let (root2, ptr2) = get_root t2 in
  (* If the roots are physically equal, it means [t1] and [t2] belong to the
     same equivalence class. If that is the case, we don't do anything. *)
  if root1 == Obj.magic root2 then Ok (Obj.magic Type.Equal)
  else
    match f root1.data root2.data with
    | Ok (data, Type.Equal)->
        let new_root =
          if root1.rank < root2.rank then (
            ptr1 := Parent ptr2 ;
            root2)
          else (
            ptr2 := Parent ptr1 ;
            if root1.rank = root2.rank then root1.rank <- root1.rank + 1 ;
            root1)
        in
        new_root.data <- data ;
        Ok Type.Equal
    | Error e -> Error e
