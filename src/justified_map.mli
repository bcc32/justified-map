open! Base

type ('k, 'v, 'cmp, 'ph) t = private ('k, 'v, 'cmp) Map.t
type ('k, 'v, 'cmp, 'ph) unpacked := ('k, 'v, 'cmp, 'ph) t

module Packed : sig
  type ('k, 'v, 'cmp) t = T : ('k, 'v, 'cmp, 'ph) unpacked -> ('k, 'v, 'cmp) t
  [@@unboxed]
end

val to_map : ('k, 'v, 'cmp, 'ph) t -> ('k, 'v, 'cmp) Map.t

module Key : sig
  type ('k, 'ph) t

  val get : ('k, _) t -> 'k
end

val with_map : ('k, 'v, 'cmp) Map.t -> f:(('k, 'v, 'cmp) Packed.t -> 'a) -> 'a
val mem : ('k, _, _, 'ph) t -> 'k -> ('k, 'ph) Key.t option
val keys : ('k, _, _, 'ph) t -> ('k, 'ph) Key.t list

val closest_key
  :  ('k, 'v, _, 'ph) t
  -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
  -> 'k
  -> (('k, 'ph) Key.t * 'v) option

val find : ('k, 'v, _, 'ph) t -> ('k, 'ph) Key.t -> 'v

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

val mapi
  :  ('k, 'v1, 'cmp, 'ph) t
  -> f:(key:('k, 'ph) Key.t -> data:'v1 -> 'v2)
  -> ('k, 'v2, 'cmp, 'ph) t

val foldi
  :  ('k, 'v, 'cmp, 'ph) t
  -> init:'a
  -> f:('a -> key:('k, 'ph) Key.t -> data:'v -> 'a)
  -> 'a
