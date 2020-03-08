open! Base

type ('k, 'v, 'cmp, 'ph) t = ('k, 'v, 'cmp) Map.t
type ('k, 'v, 'cmp, 'ph) justified_map = ('k, 'v, 'cmp, 'ph) t

module Key = struct
  type ('k, 'ph) t = 'k

  let get = Fn.id
end

let[@cold] raise_key_unexpectedly_not_in_map (type k cmp) (map : (k, _, cmp) Map.t) key =
  let (module C : Comparator.S with type t = k and type comparator_witness = cmp) =
    Map.comparator_s map
  in
  raise_s
    (Sexp.message "Key unexpectedly not in map" [ "key", C.comparator.sexp_of_t key ])
;;

let[@cold] raise_key_not_found (type k cmp) (map : (k, _, cmp) Map.t) key =
  let (module C : Comparator.S with type t = k and type comparator_witness = cmp) =
    Map.comparator_s map
  in
  raise
    (Not_found_s
       (Sexp.message "Key not found in map" [ "key", C.comparator.sexp_of_t key ]))
;;

let to_map = Fn.id

module With_map = struct
  type ('k, 'v, 'cmp) t = T : ('k, 'v, 'cmp, 'ph) justified_map -> ('k, 'v, 'cmp) t
  [@@unboxed]
end

let with_map map ~f = f (With_map.T map)

module With_singleton = struct
  type ('k, 'v, 'cmp) t =
    | T : ('k, 'ph) Key.t * ('k, 'v, 'cmp, 'ph) justified_map -> ('k, 'v, 'cmp) t
end

let with_singleton comparator ~key ~data ~f =
  f (With_singleton.T (key, Map.singleton comparator key data))
;;

let mem t k =
  match Map.mem t k with
  | true -> Some k
  | false -> None
;;

let mem_exn t k =
  match mem t k with
  | Some key -> key
  | None -> raise_key_not_found t k
;;

let find t key =
  match Map.find_exn t key with
  | data -> data
  | exception _ -> raise_key_unexpectedly_not_in_map t key
;;

let ( .%{} ) = find
let keys = Map.keys
let closest_key = Map.closest_key

let update t key ~f =
  Map.update t key ~f:(function
    | None -> raise_key_unexpectedly_not_in_map t key
    | Some data -> f data)
;;

let updatei t key ~f =
  Map.update t key ~f:(function
    | None -> raise_key_unexpectedly_not_in_map t key
    | Some data -> f ~key ~data)
;;

let set = Map.set
let[@inline always] ( .%{}<- ) t key data = set t ~key ~data
let mapi = Map.mapi

(* TODO: This would be more efficient if [Base.Map.folding_mapi] existed. *)

let folding_mapi t ~init ~f =
  Map.fold
    t
    ~init:(init, Map.empty (Map.comparator_s t))
    ~f:(fun ~key ~data (accum, map) ->
      let accum, elt = f accum ~key ~data in
      accum, Map.add_exn map ~key ~data:elt)
;;

let merge t1 t2 ~f =
  Map.merge t1 t2 ~f:(fun ~key ->
    function
    | `Left _ -> raise_key_unexpectedly_not_in_map t2 key
    | `Right _ -> raise_key_unexpectedly_not_in_map t1 key
    | `Both (d1, d2) -> Some (f ~key d1 d2))
;;

module Inserting = struct
  type ('k, 'v, 'cmp, 'ph1) t =
    | T :
        { key : ('k, 'ph2) Key.t
        ; infer : ('k, 'ph1) Key.t -> ('k, 'ph2) Key.t
        ; map : ('k, 'v, 'cmp, 'ph2) justified_map
        }
        -> ('k, 'v, 'cmp, 'ph1) t
end

let inserting t ~key ~data ~f =
  f (Inserting.T { key; infer = Fn.id; map = Map.set t ~key ~data })
;;
