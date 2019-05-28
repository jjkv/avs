(* Put your implementation in this file. *)

open Instr
open Boolean
open Bvec

let print_result o = function
  | `Full (v, b) -> Printf.fprintf o "Reg: %a; Path: %a\n" Disassembler.value v Disassembler.bexpr b
  | `Partial b -> Printf.fprintf o "Partial: %a\n" Disassembler.bexpr b
  | `Failure b -> Printf.fprintf o "Failure: %a\n" Disassembler.bexpr b

let rec print_results o = function
  | [] -> ()
  | [r] -> print_result o r
  | r::rs -> Printf.fprintf o "%a\n%a" print_result r print_results rs

type prog_ret = [ `Full of value * bexpr | `Partial of bexpr | `Failure of bexpr ]

type inst_ret = [ `Normal of config | `Failure of bexpr ]


let rec pad v i = (* same pad from magic.ml *)
  let rec cycle j =
    match j with
    | 0 -> []
    | _ -> EFalse :: cycle (j-1)
  in
  let l = List.length v in
  if i > l
  then List.append v (cycle (i - l))
  else v

let rec take i xs = (* List.take *)
 match (xs,i) with
 | (_,0) -> []
 | ([],_) -> []
 | (y::ys,i) -> y :: (take (i-1) ys)

(*promote bvec functions to pad to 3 bits-pre and then truncate to 3 bits-post*)
let padadd bv1 bv2 = take 3 @@ add (pad bv1 3) (pad bv2 3)

let padeq bv1 bv2 = take 3 @@ pad [eq (pad bv1 3) (pad bv2 3)] 3

(*added some new functions for my on sake, not necessary*)
let padbitor bv1 bv2 = take 3  @@ List.map2 (fun x y -> EOr (x,y))(pad bv1 3) (pad bv2 3)
let padlt bv1 bv2 = take 3 @@ pad [lt (pad bv1 3) (pad bv2 3)] 3

(* global namespace variable for symbolic variables *)
let a_bvec = [EVar "a_0_0"; EVar "a_0_1"; EVar "a_0_2"]
let b_bvec = [EVar "b_0_0"; EVar "b_0_1"; EVar "b_0_2"]
let c_bvec = [EVar "c_0_0"; EVar "c_0_1"; EVar "c_0_2"]
let h1_bvec = [EVar "h1_0_0"; EVar "h1_0_1"; EVar "h1_0_2"]
let h2_bvec = [EVar "h2_0_0"; EVar "h2_0_1"; EVar "h2_0_2"]
let h3_bvec = [EVar "h3_0_0"; EVar "h3_0_1"; EVar "h3_0_2"]

(*shrink boolean expressions*)
let rec normalize (b : bexpr) : bexpr =
  let rec normalize' b' =
    match b' with
    | EAnd (EFalse,_)  | EAnd (_,EFalse)  | ENot ETrue  | EFalse -> EFalse
    | EOr (ETrue,_)    | EOr (_,ETrue)    | ENot EFalse | ETrue  -> ETrue
    | EOr (EFalse, b)  | EOr (b, EFalse)  | EAnd (ETrue, b) | EAnd (b, ETrue)
        -> normalize' b
    | EAnd (b1, b2) ->
        let norm_b1     = normalize' b1
        in let norm_b2  = normalize' b2
        in if b1 = b2
        then b1
        else (match b1, b2 with
                 ENot b1', ENot b2' ->  ENot (EOr (normalize' b1', normalize' b2'))
               | ENot b1', _    -> if b1' = b2 then EFalse else EAnd(norm_b1, norm_b2)
               | _,     ENot b2'-> if b1 = b2' then EFalse else EAnd(norm_b1, norm_b2)
               | _ -> EAnd(norm_b1, norm_b2))
  | EOr (b1, b2) ->
        let norm_b1     = normalize' b1
        in let norm_b2  = normalize' b2
        in if b1 = b2
        then b1
        else (match b1, b2 with
                 ENot b1', ENot b2' -> ENot (EAnd (normalize' b1', normalize' b2'))
               | ENot b1', _    -> if b1' = b2 then ETrue else EOr(norm_b1, norm_b2)
               | _,     ENot b2'-> if b1 = b2' then ETrue else EOr(norm_b1, norm_b2)
               | _ -> EOr(norm_b1, norm_b2))
    | ENot (ENot a)   -> (normalize' a)
    | ENot a          -> ENot (normalize' a)
    | EForall (x,e)   -> EForall (x, normalize' e)
    | EExists (x,e)   -> EExists (x, normalize' e)
    | EVar x          -> EVar x
  in let new_bool = normalize' b
  in let rec satNormalize b =
          match (sat b, b) with
          | None,_     -> EFalse
          | Some [], _ -> ETrue
          | _, EAnd (b1,b2) -> (EAnd (satNormalize b1, satNormalize b2))
          | _, EOr  (b1,b2) -> (EOr  (satNormalize b1, satNormalize b2))
          | _, ENot (b1)    -> (ENot (satNormalize b1))
          | _, b1           -> b1
  in if new_bool = b then b else satNormalize (normalize new_bool)

(*shrink bvecs*)
let normalize_bvec = List.map normalize

(*single config reduction*)
let run_inst (p:prog) ((s, path):config):inst_ret =
  let ((f,pc,regs), ss) =
    match s with
    | [] -> failwith "stack should never be empty when calling run_inst"
    | x::xs  -> x,xs
  in
  match (Hashtbl.find p f).(pc) with
  | I_const (`L_Reg r,v) ->
    let v' = (match v with
                  | `L_Hole 1 -> `L_Sym h1_bvec
                  | `L_Hole 2 -> `L_Sym h2_bvec
                  | `L_Hole 3 -> `L_Sym h3_bvec
                  | _ -> v) in
    let _ = Hashtbl.replace regs r v' in
    `Normal ((f,pc+1,regs) :: ss,path)
  | I_mov (`L_Reg r1, `L_Reg r2) ->
    let _ = Hashtbl.replace regs r1 (Hashtbl.find regs r2) in
    `Normal ((f,pc+1,regs) :: ss,path)
  | I_add (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
    let v1 = Hashtbl.find regs r2 in
    let v2 = Hashtbl.find regs r3 in
    let v3 =  (*The interesting case*)
      match (v1,v2) with
      | (`L_Int i, `L_Int j) -> `L_Int (i + j)
      | (`L_Int i, `L_Sym sj) ->
        let si = bvec_of_int i in `L_Sym (normalize_bvec @@ padadd si sj)
      | (`L_Sym si, `L_Int j) ->
        let sj = bvec_of_int j in `L_Sym (normalize_bvec @@ padadd si sj)
      | (`L_Sym si, `L_Sym sj) -> `L_Sym (normalize_bvec @@ padadd si sj)
      | _ -> failwith "adding non-numbers"
    in
    let _ = Hashtbl.replace regs r1 v3 in
    `Normal ((f,pc+1,regs) :: ss, path)
  | I_sub (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
      let v1 = Hashtbl.find regs r2 in
      let v2 = Hashtbl.find regs r3 in
      let v3 =
        match (v1,v2) with
        | (`L_Int i, `L_Int j) -> `L_Int (i - j)
        | _ -> failwith "subtracting non-numbers"
      in
      let _ = Hashtbl.replace regs r1 v3 in
      `Normal ((f,pc+1,regs) :: ss, path)
  | I_mul (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
      let v1 = Hashtbl.find regs r2 in
      let v2 = Hashtbl.find regs r3 in
      let v3 =
        match (v1,v2) with
        | (`L_Int i, `L_Int j) -> `L_Int (i * j)
        | _ -> failwith "multiplying non-numbers"
      in
      let _ = Hashtbl.replace regs r1 v3 in
      `Normal ((f,pc+1,regs) :: ss, path)
  | I_div (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
      let v1 = Hashtbl.find regs r2 in
      let v2 = Hashtbl.find regs r3 in
      let v3 =
        match (v1,v2) with
        | (`L_Int i, `L_Int j) -> `L_Int (i / j)
        | _ -> failwith "dividing non-numbers"
      in
      let _ = Hashtbl.replace regs r1 v3 in
      `Normal ((f,pc+1,regs) :: ss, path)
  | I_eq (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
      let v1 = Hashtbl.find regs r2 in
      let v2 = Hashtbl.find regs r3 in
      let v3 =
        match (v1,v2) with
        | (`L_Sym si, `L_Sym sj) -> `L_Sym (normalize_bvec @@ padeq si sj)
        | (`L_Sym si, `L_Int j)  -> `L_Sym (normalize_bvec @@ padeq si @@ bvec_of_int j)
        | (`L_Int i, `L_Sym sj)  -> `L_Sym (normalize_bvec @@ padeq (bvec_of_int i) sj)
        | i,j -> if i = j then `L_Int 1 else `L_Int 0
      in
      let _ = Hashtbl.replace regs r1 v3 in
      `Normal ((f,pc+1,regs) :: ss, path)
  | I_lt (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
      let v1 = Hashtbl.find regs r2 in
      let v2 = Hashtbl.find regs r3 in
      let v3 =
        match (v1,v2) with
        | (`L_Sym si, `L_Sym sj) -> `L_Sym (normalize_bvec @@ padlt si sj)
        | (`L_Sym si, `L_Int j)  -> `L_Sym (normalize_bvec @@ padlt si (bvec_of_int j))
        | (`L_Int i, `L_Sym sj)  -> `L_Sym (normalize_bvec @@ padlt (bvec_of_int i) sj)
        | i,j -> if i = j then `L_Int 1 else `L_Int 0
      in
      let _ = Hashtbl.replace regs r1 v3 in
      `Normal ((f,pc+1,regs) :: ss, path)
  | I_leq (`L_Reg r1, `L_Reg r2, `L_Reg r3) ->
      let v1 = Hashtbl.find regs r2 in
      let v2 = Hashtbl.find regs r3 in
      let v3 =
        match (v1,v2) with
        | (`L_Sym si, `L_Sym sj) -> `L_Sym (normalize_bvec @@ padbitor (padlt si sj) (padeq si sj))
        | (`L_Sym si, `L_Int j)  -> `L_Sym (normalize_bvec @@ padbitor (padlt si (bvec_of_int j)) (padlt si (bvec_of_int j)))
        | (`L_Int i, `L_Sym sj)  -> `L_Sym (normalize_bvec @@ padbitor (padlt (bvec_of_int i) sj) (padlt (bvec_of_int i) sj))
        | i,j -> if i = j then `L_Int 1 else `L_Int 0
      in
      let _ = Hashtbl.replace regs r1 v3 in
      `Normal ((f,pc+1,regs) :: ss, path)
  | I_is_int (`L_Reg r1,`L_Reg r2) ->
    let v = Hashtbl.find regs r2 in
    let v' = match v with
             | `L_Int _ -> `L_Int 1
             | `L_Sym _ -> `L_Int 1
             | _ -> `L_Int 0
    in let _ = Hashtbl.replace regs r1 v' in
    `Normal ((f,pc+1,regs) :: ss, path)
  | I_is_str (`L_Reg r1,`L_Reg r2) ->
    let v = Hashtbl.find regs r2 in
    let v' = match v with
             | `L_Str _ -> `L_Int 1
             | _ -> `L_Int 0
    in let _ = Hashtbl.replace regs r1 v' in
    `Normal ((f,pc+1,regs) :: ss, path)
  | I_jmp i -> `Normal ((f,pc+i+1,regs)::ss,path)
  | I_rd_glob (`L_Reg r, `L_Id id) ->
    let _ = Printf.printf "id: %s\n" id; flush_all () in 
    let v = [id ^ "_0_0"; id ^ "_0_1"; id ^ "_0_2"] in 
    let _ = Hashtbl.replace regs r (`L_Sym (List.map (fun x -> EVar x) v))
    in `Normal ((f,pc+1,regs) :: ss, path)
  | I_if_zero _ -> failwith "if should have been caught in run_prog"
  | _ -> failwith "unimplemented"

type scheduler = config * config option * config list -> config list

let sch_bfs cs =
  match cs with
  | (c,None,cl) -> List.append cl [c]
  | (c,Some c', cl) -> List.append cl [c;c']

let sch_dfs cs =
  match cs with
  | (c,None,cl) -> c :: cl
  | (c,Some c', cl) -> c :: c' :: cl

  let run_prog (p:prog) (sch:scheduler) (max_steps:int):prog_ret list =
    let rec loop (steps : int) (configs : config list) : prog_ret list = (*recursive helper function *)
      match configs with
      | []        -> [] (*Then we're done!*)
      | (c :: cs) ->
          (match c with
          | ([],_) -> failwith "empty stack" (*error case shouldn't happen*)
          | ((f,pc,regs) :: ss, path) ->
            (*let _ = Printf.printf "step: %d\n" steps; flush_all () in *)
            if steps > max_steps (*counter*)
            then [`Partial path]
            else
              (match (Hashtbl.find p f).(pc) with
              | I_if_zero (`L_Reg r, i) ->
                  (match Hashtbl.find regs r with
                   | `L_Int 0 -> loop (steps + 1) (sch (((f,pc+1+i,regs) :: ss,normalize path), None, cs))
                   | `L_Sym x -> let regs1 = Hashtbl.copy regs in (*check the condition or not-condition and the path to see if we need to fork*)
                                 let regs2 = Hashtbl.copy regs in
                                 (match sat @@ normalize (EAnd (path, (zero x))), sat @@ normalize @@ (EAnd (path, ENot (zero x))) with
                                 | Some _, Some _ -> loop (steps + 1) (sch ((((f,pc+i+1,regs1) :: ss, normalize @@ EAnd (path, (zero x)))),(Some ((f,pc+1,regs2) :: ss, normalize @@ EAnd (path, ENot (zero x)))),cs))
                                 | Some _, None   -> loop (steps + 1) (sch (((((f,pc+i+1,regs1) :: ss, normalize @@ EAnd (path, (zero x)))), None, cs)))
                                 | None  , Some _ -> loop (steps + 1) (sch (((((f,pc+1,regs2) :: ss, normalize @@ EAnd (path, (ENot (zero x))))), None, cs)))
                                 | None  , None   -> loop (steps + 1) cs)
                   | _ ->        loop (steps + 1) (((f,pc+1,regs)   :: ss,normalize path) :: cs)
                   )
              | I_ret (`L_Reg r) ->
                let inv3 b:bvec = 
                    let xs = (match b with
                         | a::b::c::_ -> [a;b;c]
                         | a::b::[] -> [a;b;EFalse]
                         | a::[] -> [a;EFalse;EFalse]
                         | [] -> [EFalse;EFalse;EFalse])
                    in (List.map normalize xs) in
                let promote = (function (`L_Int x) -> `L_Sym (inv3 (Bvec.bvec_of_int x)) | y -> y) in
                if   f = "main" && ss = [] (*success!*)
                then (`Full ((promote (Hashtbl.find regs r)),normalize path) :: (loop (steps + 1) cs))
                else (match run_inst p c with
                     | `Normal c' -> loop (steps + 1) (sch (c',None,cs))
                     | `Failure path -> `Failure (normalize path) :: loop (steps + 1) cs)
              | _ -> (match run_inst p c with
                   | `Normal c' -> loop (steps + 1) (sch (c',None,cs))
                   | `Failure path -> `Failure (normalize path) :: loop (steps + 1) cs)))
    in loop 0 [([("main",0,Hashtbl.create 20)],ETrue)]
