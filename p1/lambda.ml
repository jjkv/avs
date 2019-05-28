type lexpr =
  | Var of string
  | Lam of string * lexpr
  | App of lexpr * lexpr

let rec output_lexpr () = function
  | Var x -> Printf.sprintf "%s" x
  | Lam(x, e) -> Printf.sprintf "%s.%a" x output_lexpr e
  | App(e1, e2) -> Printf.sprintf "(%a) (%a)" output_lexpr e1 output_lexpr e2

let unparse = output_lexpr ()

let rec free_vars e =
	let rec fv_ac l b a =
		(match l with
			Var x ->
				if (List.mem x a) || (List.mem x b) then a
				else x::a
		  | Lam(x, e') -> fv_ac e' (x::b) a
		  | App(e', e'') -> fv_ac e'' b (fv_ac e' b a))
	in fv_ac e [] []

(* e2[x->e1] *)
let rec subst x e1 e2 =
	let freshvar s =
		let c = (String.concat "" s) in c in
	(match e2 with
		Var s -> if (s = x) then e1 else e2
  	| Lam (s, l) -> 
  		if (x = s) then Lam (s, l)
  		else if (not (List.mem s (free_vars e1))) then Lam (s, (subst x e1 l))
  			else let frv = freshvar(List.append (free_vars e1) (free_vars l)) in
  			Lam (frv, (subst x e1 (subst s (Var frv) l)))
  	| App (l1, l2) -> App ((subst x e1 l1), (subst x e1 l2)))

let rec beta e = (match e with
	| App (e1, e2) -> 
		(match (beta e1) with
			| Some e1' -> Some (App (e1', e2))
			| None -> (match (beta e2) with
				| Some e2' -> Some (App (e1, e2'))
				| None -> (match e1 with
					| Lam (x, e') -> Some (subst x e2 e')
					| _ -> None)))
	| _ -> None)

let rec normal_form e = 
	(match e with
		| Lam (s, e') -> Lam (s, normal_form e')
		| _ -> (match (beta e) with
			| Some e' -> normal_form e'
			| None -> e))

let lexpr_of_int n = 
	let rec reduce = (function
	| 0 -> Var "y"
	| x -> App (Var "f", reduce (x - 1)))
in Lam ("f", Lam ("y", reduce n))

let add e1 e2 = 
	let sum = Lam ("a", Lam ("b", App (App (e1, Var "a"), App (App (e2, Var "a"), Var "b"))))
in normal_form sum
