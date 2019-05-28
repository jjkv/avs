let pad v i = 
	let rec n_false = (function
		| 0 -> []
		| x -> (Boolean.EFalse)::(n_false (x - 1)))
in 
	(match (i > (List.length v)) with
		| true -> 
			let fs = n_false (i - (List.length v))
			in List.append v fs
		| _ -> v) 

let add_three v1 v2 v3 = 
	let add2 = Bvec.add v1 v2 in
	let v3' = pad v3 (List.length add2) in
	let sum = Bvec.add v3' add2
in sum

let is_digit = (function
	| d0::d1::d2::d3::[] -> 
		let zero_check = Boolean.EOr (d0, Boolean.EOr (d1, Boolean.EOr (d2, d3))) in
		let ten_elev_check = Boolean.EAnd (d1, d3) in
		let twelve_plus_check = Boolean.EAnd (d2, d3) in
		let over_nine_check = Boolean.EOr (ten_elev_check, twelve_plus_check)
	in Boolean.EAnd (Boolean.ENot (over_nine_check), zero_check)
	| _ -> failwith "contract violation")

let disjoint vs =
	let rec test_against x xs = 
		(match xs with
		  [] -> Boolean.EFalse
		| y::ys -> Boolean.EOr ((Bvec.eq x y), (test_against x ys))) 
	in let rec double_for_loop e l r = 
		(match l,r with
			| [],[] -> failwith "wtf"
			| [],_ -> 
				let res = Boolean.ENot (test_against e r) 
				in res
			| (x::xs),_ -> 
				let res = Boolean.ENot (test_against e (List.append l r))
				in Boolean.EAnd (res, (double_for_loop x xs (e::r))))
in (match vs with
	| [] -> failwith "contract violation"
	| x::xs -> double_for_loop x xs [])

let is_magic vl = (match (List.map (fun (e) -> pad e 4) vl) with
	| v1::v2::v3::v4::v5::v6::v7::v8::v9::[] -> 
		let is_disjoint = disjoint vl in
		let r1 = add_three v1 v2 v3 in
		let r2 = add_three v4 v5 v6 in
		let r3 = add_three v7 v8 v9 in
		let c1 = add_three v1 v4 v7 in
		let c2 = add_three v2 v5 v8 in
		let c3 = add_three v3 v6 v9 in
		let d_tl_br = add_three v1 v5 v9 in
		let d_tr_bl = add_three v3 v5 v7 in
		let rec all_digits = List.fold_left (fun a e -> Boolean.EAnd((is_digit (pad e 4)), a)) Boolean.ETrue vl in
		let all_eq = Boolean.EAnd(Bvec.eq r1 r2, Boolean.EAnd(Bvec.eq r1 r3, Boolean.EAnd(Bvec.eq r1 c1, 
			         Boolean.EAnd(Bvec.eq r1 c2, Boolean.EAnd(Bvec.eq r1 c3, Boolean.EAnd(Bvec.eq r1 d_tl_br, Bvec.eq r1 d_tr_bl)))))) in 
		let res = Boolean.EAnd (is_disjoint, Boolean.EAnd (all_digits, all_eq))
	in res
	| _ -> failwith "contract violation")
