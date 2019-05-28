open Boolean

type bvec = bexpr list (* low order bit at head of list *)

let int_of_bvec v = 
	let ints = List.map (function
	| ETrue -> 1
	| EFalse -> 0
    | _ -> failwith "contract violation") v in
	let rec to_int_ac xs a = (match xs with
		[] -> a
	  | b::rest -> to_int_ac rest ((2 * a) + b))
in to_int_ac (List.rev ints) 0

let rec bvec_of_int i = 
	let rec to_bvec_ac n a = (match n with
	| 0 -> a
	| _ -> 
		let bit = (function | 1 -> ETrue 
						    | 0 -> EFalse 
						    | _ -> failwith "can't happen") 
		in to_bvec_ac (n / 2) ((bit (n mod 2))::a))
in if i < 0 then failwith "contract violation" else
   if i = 0 then [EFalse] else List.rev (to_bvec_ac i [])

let rec subst a v = 
	let rec assign s a' b = (match b with
		| (EVar x) -> if x <> s then (EVar x) else
					  (if a' then ETrue else EFalse)
		| (ENot x) -> ENot (assign s a' x)
		| (EAnd (x,y)) -> EAnd ((assign s a' x), (assign s a' y))
		| (EOr (x,y)) -> EOr ((assign s a' x), (assign s a' y))
		| (EForall (s', x)) -> 
			if s' <> s then EForall (s', (assign s a' x))
			else EForall (s', x)
		| (EExists (s', x)) ->
			if s' <> s then EExists (s', (assign s a' x))
			else EExists (s', x)
		| _ -> b) 
in (match a with
	[] -> v
  | (s, a')::rest -> subst rest (List.map (assign s a') v))

let zero v = List.fold_left (fun a e -> (EAnd(a, (ENot e)))) ETrue v

let bitand v1 v2 = 
	let rec zip = (function
		| (a::[], b::[]) -> [(a, b)]
		| (a::ax, b::bx) -> (a,b)::(zip (ax, bx))
		| (_, _) -> []) in
	let xs = zip (v1, v2) in
	let anded = List.map (fun (e) -> let (x,y) = e in (EAnd (x,y))) xs
in anded

let xor b1 b2 = EOr (EAnd (b1, ENot (b2)), EAnd (ENot (b1), b2))
let nxor b1 b2 = ENot (EOr (EAnd (b1, ENot (b2)), EAnd (ENot (b1), b2)))

let eq v1 v2 =
	let rec zip = (function
		| (a::[], b::[]) -> [(a, b)]
		| (a::ax, b::bx) -> (a,b)::(zip (ax, bx))
		| (_, _) -> []) in
	let xs = zip (v1, v2) in
	let nxor b = 
		let (b1,b2) = b in
		ENot (EOr (EAnd (b1, ENot (b2)), EAnd (ENot (b1), b2))) in
	let bitxor = List.map nxor xs in
	let iseq = List.fold_left (fun a e -> (EAnd(a, e))) ETrue bitxor
in iseq

let add v1 v2 =
	let xor b1 b2 = EOr (EAnd (b1, ENot (b2)), EAnd (ENot (b1), b2)) in
	let rec add_ac v1' v2' c = 
		(match (v1', v2') with
			| [], [] -> [c]
			| x::xs, y::ys -> 
				let d = xor c (xor x y) in
				let c' = EOr (EAnd (x, y), EAnd (c, EOr (x, y)))
				in d::(add_ac xs ys c')
			| _ -> failwith "contract violation")
	in add_ac v1 v2 EFalse
