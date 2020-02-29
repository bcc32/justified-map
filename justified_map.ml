open! Base

type ('k, 'v, 'cmp, 'ph) t = ('k, 'v, 'cmp) Map.t

module Handler = struct
  type nonrec ('k, 'v, 'cmp, 'a) t = { f : 'ph. ('k, 'v, 'cmp, 'ph) t -> 'a } [@@unboxed]
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
let with_map map ({ f } : _ Handler.t) = f map

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
let foldi t ~init ~f = Map.fold t ~init ~f:(fun ~key ~data accum -> f accum ~key ~data)
