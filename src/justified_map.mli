open! Base

(** {1 Map and Key types} *)

type ('k, 'v, 'cmp, 'ph) t = private ('k, 'v, 'cmp) Map.t
type ('k, 'v, 'cmp, 'ph) justified_map := ('k, 'v, 'cmp, 'ph) t

val to_map : ('k, 'v, 'cmp, 'ph) t -> ('k, 'v, 'cmp) Map.t

module Key : sig
  type ('k, 'ph) t = private 'k

  val get : ('k, _) t -> 'k
end

(** {1 Evaluation} *)

module With_map : sig
  type ('k, 'v, 'cmp) t = T : ('k, 'v, 'cmp, 'ph) justified_map -> ('k, 'v, 'cmp) t
  [@@unboxed]
end

val with_map : ('k, 'v, 'cmp) Map.t -> f:(('k, 'v, 'cmp) With_map.t -> 'a) -> 'a

module With_singleton : sig
  type ('k, 'v, 'cmp) t =
    | T : ('k, 'ph) Key.t * ('k, 'v, 'cmp, 'ph) justified_map -> ('k, 'v, 'cmp) t
end

val with_singleton
  :  ('k, 'cmp) Map.comparator
  -> key:'k
  -> data:'v
  -> f:(('k, 'v, 'cmp) With_singleton.t -> 'a)
  -> 'a

(** {1 Gathering evidence} *)

val mem : ('k, _, _, 'ph) t -> 'k -> ('k, 'ph) Key.t option
val mem_exn : ('k, _, _, 'ph) t -> 'k -> ('k, 'ph) Key.t
val keys : ('k, _, _, 'ph) t -> ('k, 'ph) Key.t list

val closest_key
  :  ('k, 'v, _, 'ph) t
  -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
  -> 'k
  -> (('k, 'ph) Key.t * 'v) option

(** {1 Safe lookup} *)

val find : ('k, 'v, _, 'ph) t -> ('k, 'ph) Key.t -> 'v
val ( .%{} ) : ('k, 'v, _, 'ph) t -> ('k, 'ph) Key.t -> 'v

(** {1 Preserving key sets} *)

(** {2 Localized updates} *)

val update
  :  ('k, 'v, 'cmp, 'ph) t
  -> ('k, 'ph) Key.t
  -> f:('v -> 'v)
  -> ('k, 'v, 'cmp, 'ph) t

val updatei
  :  ('k, 'v, 'cmp, 'ph) t
  -> ('k, 'ph) Key.t
  -> f:(key:('k, 'ph) Key.t -> data:'v -> 'v)
  -> ('k, 'v, 'cmp, 'ph) t

val set : ('k, 'v, 'cmp, 'ph) t -> key:('k, 'ph) Key.t -> data:'v -> ('k, 'v, 'cmp, 'ph) t
val ( .%{}<- ) : ('k, 'v, 'cmp, 'ph) t -> ('k, 'ph) Key.t -> 'v -> ('k, 'v, 'cmp, 'ph) t

(** {2 Mapping values} *)

val mapi
  :  ('k, 'v1, 'cmp, 'ph) t
  -> f:(key:('k, 'ph) Key.t -> data:'v1 -> 'v2)
  -> ('k, 'v2, 'cmp, 'ph) t

val folding_mapi
  :  ('k, 'v1, 'cmp, 'ph) t
  -> init:'accum
  -> f:('accum -> key:('k, 'ph) Key.t -> data:'v1 -> 'accum * 'v2)
  -> 'accum * ('k, 'v2, 'cmp, 'ph) t

(** {2 Zipping} *)

val merge
  :  ('k, 'v1, 'cmp, 'ph) t
  -> ('k, 'v2, 'cmp, 'ph) t
  -> f:(key:('k, 'ph) Key.t -> 'v1 -> 'v2 -> 'v)
  -> ('k, 'v, 'cmp, 'ph) t

(** {1 Enlarging key sets} *)

(** {2 Inserting new keys} *)

module Inserting : sig
  (* TODO: Consider the naming of these record fields. *)
  type ('k, 'v, 'cmp, 'ph1) t =
    | T :
        { key : ('k, 'ph2) Key.t
        ; infer : ('k, 'ph1) Key.t -> ('k, 'ph2) Key.t
        ; map : ('k, 'v, 'cmp, 'ph2) justified_map
        }
        -> ('k, 'v, 'cmp, 'ph1) t
end

(* TODO: Unlike the Haskell version, perhaps we do not need to write this in
   continuation-passing style. *)
val inserting
  :  ('k, 'v, 'cmp, 'ph) t
  -> key:'k
  -> data:'v
  -> f:(('k, 'v, 'cmp, 'ph) Inserting.t -> 'a)
  -> 'a

val inserting_with
  :  ('k, 'v, 'cmp, 'ph) t
  -> key:'k
  -> data:'v
  -> combine:(old_data:'v -> new_data:'v -> 'v)
  -> f:(('k, 'v, 'cmp, 'ph) Inserting.t -> 'a)
  -> 'a

(** {2 Unions} *)

module Unioning : sig
  type ('k, 'v, 'cmp, 'ph_l, 'ph_r) t =
    | T :
        { infer_left : ('k, 'ph_l) Key.t -> ('k, 'ph_union) Key.t
        ; infer_right : ('k, 'ph_r) Key.t -> ('k, 'ph_union) Key.t
        ; map : ('k, 'v, 'cmp, 'ph_union) justified_map
        }
        -> ('k, 'v, 'cmp, 'ph_l, 'ph_r) t
end

val unioning
  :  ('k, 'v, 'cmp, 'ph_l) t
  -> ('k, 'v, 'cmp, 'ph_r) t
  -> f:(('k, 'v, 'cmp, 'ph_l, 'ph_r) Unioning.t -> 'a)
  -> 'a
