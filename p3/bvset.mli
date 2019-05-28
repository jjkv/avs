type 'a bvset_key (* the key indicating which bits stand for what *)

(* Make the key for a bit vector set *)
val list_to_key : 'a list -> 'a bvset_key

(* Return a list of all members of the set. *)
val to_list : 'a bvset_key -> int -> 'a list

(* Return an empty set that ranges over the elements in the list. *)
val mkempty : 'a bvset_key -> int

(* Add an element to a set, or do nothing if elt already present.
   Raises an exception if elt not in range of set. *)
val insert : 'a bvset_key -> 'a -> int -> int

(* Remove an element to a set, or do nothing if elt not present.
   Raises an exception if elt not in range of set. *)
val remove : 'a bvset_key -> 'a -> int -> int

(* Return true iff element in set. Raises an exception if elt not in
   range of set. *)
val mem : 'a bvset_key -> 'a -> int -> bool

(* Set union. *)
val union : 'a bvset_key -> int -> int -> int

(* Set intersection. *)
val inter : 'a bvset_key -> int -> int -> int

(* diff a b returns a - b. *)
val diff : 'a bvset_key -> int -> int -> int

(* negate a returns 1 - a, where 1 is the set of all elements. Thus,
   negate (mkempty l) returns top. *)
val negate : 'a bvset_key -> int -> int

(* Return true iff sets are equal. *)
val equal : 'a bvset_key -> int -> int -> bool
