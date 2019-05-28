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

let rec eval a e =
  let rec find x a2 = (*association list lookup*)
    match a2 with
    | (v,b) :: rest -> if x = v then b else find x rest
    | _             -> false
  in  match e with
      | ETrue        -> true
      | EFalse       -> false
      | EVar s       -> find s a
      | EAnd (e1,e2) -> (eval a e1) && (eval a e2)
      | EOr (e1,e2)  -> (eval a e1) || (eval a e2)
      | ENot e1      -> (eval a e1) == false
      | EForall (x,e1) -> (*true iff e1 is true when x is both true and false*)
          (eval ((x,true)  :: a) e1) &&
          (eval ((x,false) :: a) e1)
      | EExists (x,e1) -> (*true iff e1 is true when x is either true or false*)
          (eval ((x,true)  :: a) e1) ||
          (eval ((x,false) :: a) e1)

let rec free_vars e =
  let rec insert x xs = (* set-like insert for list implementation *)
      match xs with
      | []        -> [x]
      | (y :: ys) ->
        match compare x y with
        | (-1) -> x :: xs
        | (0)  -> xs
        | _    -> y :: (insert x ys)
  in
  let append xs ys = List.fold_right insert xs ys in (*set-like union*)
  match e with
      | ETrue          -> []
      | EFalse         -> []
      | EVar s         -> [s]
      | EAnd (e1,e2)   -> append (free_vars e1) (free_vars e2)
      | EOr (e1,e2)    -> append (free_vars e1) (free_vars e2)
      | ENot e1        -> free_vars e1
      | EForall (x,e1) -> List.filter (fun y -> compare x y != 0) (free_vars e1)
      | EExists (x,e1) -> List.filter (fun y -> compare x y != 0) (free_vars e1)

let sat e =
  (*lift a function over an option*)
  let omap (f : 'a -> 'b) (a : 'a option) : 'b option =
    match a with
    | Some elem -> Some (f elem)
    | None      -> None
  in
  (* permutes the assignment in a bit-vector like way (adds 1).
   * assumes we start with all false
   * if all trues returns None as we have exhausted all possible assignments.
   * advance ["x",true;"y",false;"z",false] = ["x",false;"y",true;"z",false]*)
  let rec advance (xs : (string*bool) list) : ((string*bool) list) option  =
     (match xs with
      | (y,false) :: ys -> Some ((y,true) :: ys) (* flip the bit *)
      | (y,true)  :: ys -> omap (List.cons (y,false)) (advance ys) (*reset low order bit and advance high order bit*)
      | []              -> None) in (*we have flipped every bit*)
  let rec search (assignment : (string * bool) list) : ((string * bool) list) option =
    let currentAssignment = assignment in
    if eval currentAssignment e (*found a satisfying assignment *)
    then Some currentAssignment
    else match advance currentAssignment with 
         | None -> None          (*couldn't be found *)
         | Some newAssignment -> (*keep searching*)
             search newAssignment;
  in search (List.map (fun x -> (x,false)) (free_vars e))
