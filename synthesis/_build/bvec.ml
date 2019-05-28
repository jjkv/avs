open Boolean

type bvec = bexpr list (* low order bit at head of list *)

let int_of_bvec v =
  let i = ref 0 in
  let place = ref 0.0 in
  let _ = List.map (fun x -> if x = ETrue then i := !i + (truncate (2.0 ** !place)) else () ; place := !place +. 1.0) v
  in !i

let bvec_of_int i =
  match i with
  | 0 -> [EFalse]
  | 1 -> [ETrue]
  | x -> let rec bvec_of_int' i' acc =
           match (i' mod 2, i' > 1) with
           | 1,true    -> bvec_of_int' ((i' - 1) / 2) (List.append acc [ETrue])
           | 0,true    -> bvec_of_int' (i' / 2) (List.append acc [EFalse])
           | _          -> List.append acc [ETrue]
         in bvec_of_int' x []

let subst a v =
  let rec sub1 x b v' =
    match v' with
    | [] -> []
    | be :: rest ->
      let rec subIn be =
          match be with
          | EVar y -> if x = y then (if b then ETrue else EFalse) else EVar y
          | EAnd (e1,e2)  -> EAnd (subIn e1, subIn e2)
          | EOr  (e1,e2)  -> EOr (subIn e1, subIn e2)
          | EForall (z,e) -> if (x = z) then EForall (z,e) else EForall (z,subIn e)
          | EExists (z,e) -> if (x = z) then EExists (z,e) else EExists (z,subIn e)
          | ENot e        -> ENot (subIn e)
          | ETrue         -> ETrue
          | EFalse        -> EFalse
      in  (subIn be) :: (sub1 x b rest)
  in List.fold_right (fun (x,b) acc -> sub1 x b acc) a v

let zero v =
  let zero1 be =
    match be with
    | ETrue         -> EFalse
    | EFalse        -> ETrue
    | EVar x        -> ENot (EVar x)
    | EAnd (e1,e2)  -> EOr (ENot e1, ENot e2)
    | EOr (e1,e2)   -> EAnd (ENot e1, ENot e2)
    | ENot e        -> e
    | EForall (x,e) -> EExists (x, ENot e)
    | EExists (x,e) -> EForall (x, ENot e)
  in List.fold_right (fun be acc -> EAnd ((zero1 be),acc)) v ETrue

let rec zip xs ys =
  match (xs,ys) with
  | (x :: xs', y :: ys') -> (x,y) :: zip xs' ys'
  | _ -> []

let bitand v1 v2 = List.fold_right ( fun (b1,b2) acc -> EAnd (b1,b2) :: acc) (zip v1 v2) []

let eq v1 v2 =
  let xor e1 e2 = EAnd ((EOr (e1,e2)), (ENot (EAnd (e1,e2)))) in
  List.fold_right (fun (b1,b2) acc -> EAnd ((ENot (xor b1 b2)),acc)) (zip v1 v2) ETrue


let add v1 v2 =
  let xor e1 e2 = EAnd ((EOr (e1,e2)), (ENot (EAnd (e1,e2)))) in
  let rec add' (vs : (bexpr*bexpr) list) (carry : bexpr) : bvec =
    match vs with
    | (a,b) :: vs' ->
        (xor a (xor b carry)) :: (add' vs' (EOr(EAnd(b,carry),EOr (EAnd (a,carry),EAnd (a,b)))))
    | [] -> [carry]
  in add' (zip v1 v2) EFalse

let lt bv1 bv2 =
  let zs = zip (List.rev bv1) (List.rev bv2) in
  List.fold_right (fun (x,y) z -> EOr(z,EAnd(y,ENot x))) zs EFalse
