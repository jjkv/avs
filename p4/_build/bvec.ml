open Boolean

type bvec = bexpr list (* low order bit at head of list *)

let int_of_bvec v =
  let i = ref 0 in (*accumulator*)
	let place = ref 0.0 in (*decimal place*)
	let _ = List.map (fun x ->
                    if x = ETrue (*bit is on*)
                    then i := !i + (truncate (2.0 ** !place)) (*add 2^place to the accumulator*)
                    else () ;  (*add 0 to the accumulator*)
                    place := !place +. 1.0) (*advance the place *)
                   v (*map over the bitvector*)
	in !i (*return dereferenced accumulator*)

let bvec_of_int i =
  match i with
  | 0 -> [EFalse] (*0 basecase*)
  | 1 -> [ETrue]  (*1 basecase*)
  | x -> let rec bvec_of_int' i' acc = (* can assume >= 2*)
           match (i' mod 2, i' > 1) with (* check if bit should be set and if we are done *)
           | 1,true    -> bvec_of_int' ((i' - 1) / 2) (List.append acc [ETrue])
           | 0,true    -> bvec_of_int' (i' / 2) (List.append acc [EFalse])
           | _          -> List.append acc [ETrue]
         in bvec_of_int' x []

let subst a v =
  let rec sub1 x b v' = (*sub x for b in v' *)
    match v' with
    | [] -> []
    | be :: rest ->
      let rec subIn be = (*sub x for b in bit be *)
          match be with
          | EVar y -> if x = y then (if b then ETrue else EFalse) else EVar y
          | EAnd (e1,e2)  -> EAnd (subIn e1, subIn e2)
          | EOr  (e1,e2)  -> EOr (subIn e1, subIn e2)
          | EForall (z,e) -> if (x = z) then EForall (z,e) else EForall (z,subIn e)
          | EExists (z,e) -> if (x = z) then EExists (z,e) else EExists (z,subIn e)
          | ENot e        -> ENot (subIn e)
          | ETrue         -> ETrue
          | EFalse        -> EFalse
      in  (subIn be) :: (sub1 x b rest) (*map over bitvector*)
  in List.fold_right (fun (x,b) acc -> sub1 x b acc) a v (*sub for each assignment*)

let zero v = (* zero if all bits are false *)
  let zero1 be = (*negate bit*)
    match be with
    | ETrue         -> EFalse
    | EFalse        -> ETrue
    | EVar x        -> ENot (EVar x)
    | EAnd (e1,e2)  -> EOr (ENot e1, ENot e2)
    | EOr (e1,e2)   -> EAnd (ENot e1, ENot e2)
    | ENot e        -> e
    | EForall (x,e) -> EExists (x, ENot e)
    | EExists (x,e) -> EForall (x, ENot e)
  in List.fold_right (fun be acc -> EAnd ((zero1 be),acc)) v ETrue (*map (and o negate) over bitvector *)

(*pairwise zip two lists. equiv to map2 *)
let rec zip xs ys =
  match (xs,ys) with
  | (x :: xs', y :: ys') -> (x,y) :: zip xs' ys'
  | _ -> []

let bitand v1 v2 = (*pairwise zip and and each pair*)
  List.fold_right ( fun (b1,b2) acc -> EAnd (b1,b2) :: acc) (zip v1 v2) []

let eq v1 v2 = (*vectors are eq if each bit is eq. 2 bits are eq if same for all assignments*)
  let xor e1 e2 = EAnd ((EOr (e1,e2)), (ENot (EAnd (e1,e2)))) in (*not xor = both same*)
  List.fold_right (fun (b1,b2) acc -> EAnd ((ENot (xor b1 b2)),acc)) (zip v1 v2) ETrue


let add v1 v2 =
  let xor e1 e2 = EAnd ((EOr (e1,e2)), (ENot (EAnd (e1,e2)))) in
  let rec add' (vs : (bexpr*bexpr) list) (carry : bexpr) : bvec =
    match vs with
    | (a,b) :: vs' -> (*bit is 1 if odd number of bits (a,b,carry) is one. recurse on rest*)
        (xor a (xor b carry)) :: (add' vs' (*carry' is 1 if (a+b+carry) > 1*)
                                 (EOr(EAnd(b,carry),
                                  EOr (EAnd (a,carry),EAnd (a,b)))))
    | [] -> [carry] (* add last carry bit to end*)
  in add' (zip v1 v2) EFalse
