(* Put your solution to part 1 in this file *)

(*
  You can assume that 63 bits is enough for anyone.
*)

(* elt-to-int, int-to-elt 
   elem-to-index, reverse *)
type 'a bvset_key = ('a, int) Hashtbl.t * (int, 'a) Hashtbl.t (* don't change this type *)

(* private helper, gets bit x of int n *)
let bit_of x n = 1 land (n lsr x)

let list_to_key (l:'a list):'a bvset_key =
	let rec ac_key l' i (k1,k2) =
		(match l' with
			[] -> (k1, k2)
		  | (x::xs) -> 
		  	let _ = Hashtbl.replace k1 x i in
		  	let _ = Hashtbl.replace k2 i x in
		  	ac_key xs (i + 1) (k1, k2))
	in ac_key l 0 ((Hashtbl.create 63), (Hashtbl.create 63))

let to_list (k:'a bvset_key) (n:int):'a list = 
	let (_, k2) = k in 
	let mk_list k' v' a =
		(match (bit_of k' n) with 
			0 -> a
		  | 1 -> (v'::a)
		  | _ -> failwith "wtf is going on")
	in Hashtbl.fold mk_list k2 []

let mkempty (k:'a bvset_key):int = 0

let insert (k:'a bvset_key) (x:'a) (n:int):int =
	let (k1, _) = k in
	let i = Hashtbl.find k1 x 
	in n lor (1 lsl i)

let remove (k:'a bvset_key) (x:'a) (n:int):int =
	let (k1, _) = k in
	let i = Hashtbl.find k1 x in
	let rm = (0-1) lsr (63 - i) in
	let lm = ((0-1) lsr (i + 1)) lsl (i + 1) in
	let m = rm lxor lm
	in m land n

let mem (k:'a bvset_key) (x:'a) (n:int):bool =
	let (k1, _) = k in
	let i = Hashtbl.find k1 x in
	let r = (1 land (n lsr i)) 
	in (function 1 -> true | _ -> false) r

let union (k:'a bvset_key) (n1:int) (n2:int):int = n1 lor n2

let inter (k:'a bvset_key) (n1:int) (n2:int):int = n1 land n2

let diff (k:'a bvset_key) (n1:int) (n2:int):int =
	let i = inter k n1 n2 
	in n1 lxor i

let negate (k:'a bvset_key) (n:int):int =
	let (k1, _) = k in
	let l = Hashtbl.length k1 in
	let t = (1 lsl l) - 1 
	in t - n

let equal (k:'a bvset_key) (n1:int) (n2:int):bool =
	let (k1, _) = k in
	let l = 63 - (Hashtbl.length k1) in
	let n1' = n1 lsl l in
	let n2' = n2 lsl l 
	in n1' = n2'
