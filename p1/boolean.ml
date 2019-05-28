type bexpr =
    EFalse
  | ETrue
  | EVar of string
  | EAnd of bexpr * bexpr
  | EOr of bexpr * bexpr
  | ENot of bexpr
  | EForall of string * bexpr
  | EExists of string * bexpr

type asst = (string * bool) list

let rec eval rho e =
	let rec ev f = (match f with
	    EFalse -> false
	  | ETrue -> true
	  |	EVar s -> List.assoc s rho
	  | ENot x -> not (ev x)
	  | EAnd (x,y) -> ev x && ev y
	  | EOr (x,y) -> ev x || ev y
	  | EForall (x, e') ->
	  	let rho't = (x, true)::rho in
	  	let rho'f = (x, false)::rho in
	  	(eval rho't e') && (eval rho'f e')
	  | EExists (x, e') ->
	  	let rho't = (x, true)::rho in
	  	let rho'f = (x, false)::rho in
	  	(eval rho't e') || (eval rho'f e'))
	in ev e

let rec free_vars e =
	let rec fv_ac l b a =
		(match l with
			EFalse -> a
		  | ETrue -> a
		  | EVar s -> if (List.mem s a) || (List.mem s b) then a
		  			  else s::a
		  | ENot f -> fv_ac f b a
		  | EAnd (x,y) -> fv_ac y b (fv_ac x b a)
		  | EOr (x,y) -> fv_ac (EAnd (x,y)) b a
		  | EForall (s, f) -> fv_ac f (s::b) a
		  | EExists (s, f) -> fv_ac (EForall (s, f)) b a)
	in fv_ac e [] []

let sat f = 
	let rec pow a n = (match n with
		| 0 -> 1
		| _ -> a * (pow a (n - 1))) in
	let fvs = (free_vars f) in
	let c = pow 2 (List.length fvs) - 1 in
	let all_true = List.map (fun (_) -> true) fvs in
	let rec gen_perm_from n l = (match l with 
		| [] -> []
		| b::bs -> 
			let b2b = (function
			| 0 -> true
			| 1 -> false
			| _ -> failwith "this can't happen") in 
			(b2b (n mod 2))::(gen_perm_from (n / 2) bs)) in
	let rec all_perms n = (match n with
		| 0 -> [gen_perm_from 0 all_true]
		| x -> (gen_perm_from x all_true)::(all_perms (n - 1))) in
	let rec zip = (function
		| (a::[], b::[]) -> [(a, b)]
		| (a::ax, b::bx) -> (a,b)::(zip (ax, bx))
		| (_, _) -> []) in
	let ap = all_perms c in
	let rec solve p = (match p with
		| [] -> None
		| (x::xs) -> 
			let r = (zip (fvs, x)) in
			if (eval r f) then Some r
			else solve xs)
in (match fvs with
	| [] -> if (eval [] f) then Some [] else None
	| _ -> solve ap)
