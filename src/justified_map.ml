open! Base

type ('k, 'v, 'cmp, 'ph) t = ('k, 'v, 'cmp) Map.t
type ('k, 'v, 'cmp, 'ph) unpacked = ('k, 'v, 'cmp, 'ph) t

module Packed = struct
  type ('k, 'v, 'cmp) t = T : ('k, 'v, 'cmp, 'ph) unpacked -> ('k, 'v, 'cmp) t
  [@@unboxed]
end

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

let to_map = Fn.id
let with_map map ~f = f (Packed.T map)

let mem t k =
  match Map.mem t k with
  | true -> Some k
  | false -> None
;;

let find t key =
  match Map.find_exn t key with
  | data -> data
  | exception _ -> raise_key_unexpectedly_not_in_map t key
;;

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
let mapi = Map.mapi
