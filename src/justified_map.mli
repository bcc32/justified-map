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
