open! Base

type ('k, 'v, 'cmp, 'ph) t

val to_map : ('k, 'v, 'cmp, 'ph) t -> ('k, 'v, 'cmp) Map.t

module Handler : sig
  type nonrec ('k, 'v, 'cmp, 'a) t = { f : 'ph. ('k, 'v, 'cmp, 'ph) t -> 'a } [@@unboxed]
end

module Key : sig
  type ('k, 'ph) t

  val get : ('k, _) t -> 'k
end

val with_map : ('k, 'v, 'cmp) Map.t -> ('k, 'v, 'cmp, 'a) Handler.t -> 'a
val mem : ('k, _, _, 'ph) t -> 'k -> ('k, 'ph) Key.t option
val keys : ('k, _, _, 'ph) t -> ('k, 'ph) Key.t list
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
