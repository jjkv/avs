open Bvset
open Instr

(* ./main.byte -analysis ae _ *)

(**************************************************************************)

type kind_uop = U_is_int | U_is_str
type kind_binop = B_add | B_sub | B_mul | B_div | B_eq | B_lt | B_leq
type expr =
  | Uop of kind_uop * reg
  | Binop of kind_binop * reg * reg  (* op, src, src *)

let to_expr i = 
  (match i with
      I_add (_, r2, r3) -> Some (Binop (B_add, r2, r3))
    | I_sub (_, r2, r3) -> Some (Binop (B_sub, r2, r3))
    | I_mul (_, r2, r3) -> Some (Binop (B_mul, r2, r3))
    | I_div (_, r2, r3) -> Some (Binop (B_div, r2, r3))
    | I_eq (_, r2, r3) -> Some (Binop (B_eq, r2, r3))
    | I_lt (_, r2, r3) -> Some (Binop (B_lt, r2, r3))
    | I_leq (_, r2, r3) -> Some (Binop (B_leq, r2, r3))
    | I_is_int (_, r2) -> Some (Uop (U_is_int, r2))
    | I_is_str (_, r2) -> Some (Uop (U_is_str, r2))
    | _ -> None)

let output_kind_uop o = function
  | U_is_int -> Printf.fprintf o "is_int"
  | U_is_str -> Printf.fprintf o "is_str"

let output_kind_binop o = function
  | B_add -> Printf.fprintf o "+"
  | B_sub -> Printf.fprintf o "-"
  | B_mul -> Printf.fprintf o "*"
  | B_div -> Printf.fprintf o "/"
  | B_eq -> Printf.fprintf o "="
  | B_lt -> Printf.fprintf o "<"
  | B_leq -> Printf.fprintf o "<="

let output_expr o = function
  | Uop (u, r) -> Printf.fprintf o "%a%a" output_kind_uop u Disassembler.reg r
  | Binop (b, r1, r2) -> Printf.fprintf o "%a %a %a" Disassembler.reg r1 output_kind_binop b Disassembler.reg r2

type gen_kill = { gen : int; kill : int }

let exprs_of_prog (instrs:instr array):expr list =
    let exprs es i =
        (match (to_expr i) with 
            Some x -> if (List.mem x es) then es
                      else x::es
          | None -> es)
      in Array.fold_left exprs [] instrs

let gk_general instrs k opt =
    let exprs = exprs_of_prog instrs in
    let killed_reg = (function
            I_add (r, _, _) | I_sub (r, _, _) | I_mul (r, _, _)
          | I_div (r, _, _) | I_eq  (r, _, _) | I_lt  (r, _, _)
          | I_leq (r, _, _) | I_is_int (r, _) | I_is_str (r, _)
          | I_const (r, _)  | I_mov (r, _) 
              -> (Some r)
          | _ -> None) in
    let gen_kill_ary i = 
        let gs =  (match (to_expr i) with
                      Some op -> Bvset.insert k op (Bvset.mkempty k)
                    | None -> Bvset.mkempty k) in
        let ks = let r = killed_reg i in
            (match r with 
                None -> (Bvset.mkempty k)
              | Some x -> 
                  let mentions = (function 
                        Uop   (_, r')      -> (x = r')
                      | Binop (_, r', r'') -> (x = r') || (x = r'')) in
                  let killed = (List.filter mentions exprs)
                  in (List.fold_left (fun a b -> (Bvset.insert k b a)) 
                                     (Bvset.mkempty k) 
                                     killed))
        in (match opt with
              | `AE ->
                  let gs' = (Bvset.diff k gs ks) 
                  in { gen=gs'; kill=ks }
              | `VBE ->
                  let ks' = (Bvset.diff k ks gs)
                  in { gen=gs; kill=ks' }) 
    in (Array.map gen_kill_ary instrs)

let avail_exprs (instrs:instr array) (k:expr bvset_key):gen_kill array = 
    gk_general instrs k `AE

let very_busy_exprs (instrs:instr array) (k:expr bvset_key):gen_kill array = 
    gk_general instrs k `VBE

(**************************************************************************)

let succ (instrs : instr array) (i:int):int list =
    let l = Array.length instrs in
    let xs = (fun x -> if x >= l-1 then [] else [i+1]) i in
    (match (Array.get instrs i) with
            I_jmp x -> if l > (i + x + 1) then [(i + x + 1)]
                       else failwith "out of bound jump"
          | I_if_zero (_, 0) -> xs
          | I_if_zero (_, x) -> (i + x + 1)::xs
          | _ -> xs)

let pred (instrs : instr array) (i:int):int list =
    (match i with
          0 -> []
        | _ -> 
          let sucs = Array.mapi (fun x _ -> (x, (succ instrs x))) instrs in
          let collect a b =
              let (i', xs) = b in
              if (List.mem i xs) then (i'::a)
              else a
          in Array.fold_left collect [] sucs)

type dir_type =
  | D_Forward
  | D_Backward

type may_must_type =
  | K_May
  | K_Must

let dfa (instrs : instr array)
	(key : 'a bvset_key)
	(dir : dir_type)
	(may_must : may_must_type)
	(gen_kill : instr array -> 'a bvset_key -> gen_kill array)
	(entry_or_exit_facts : int)
	(top : int):int array =
    let gka = gen_kill instrs key in
    let wl = Array.to_list (Array.mapi (fun x _ -> x) instrs) in
    let init_oi = (function | K_Must -> (fun _ -> top) 
                            | K_May -> (fun _ -> (Bvset.mkempty key))) in
    let o_i = Array.map (init_oi may_must) instrs in
    let init_d = (function | D_Forward  -> (pred, succ)
                           | D_Backward -> (succ, pred)) in
    let d = init_d dir in
    let mjf_init = (function | K_Must -> Bvset.inter key
                             | K_May -> Bvset.union key) in
    let mjf = mjf_init may_must in
    let mjc_init = (function | K_Must -> Bvset.union key 
                             | K_May -> Bvset.inter key) in
    let mjc = mjc_init may_must in
    let rec run (wl, o_i, d) =
        let (above, below) = d in
        (match wl with 
            [] -> o_i
          | (s::ws) ->
              let above_s = above instrs s in 
              let in_s = (match above_s with
                              [] -> entry_or_exit_facts
                            | _  -> (List.fold_left 
                                    (fun a b -> 
                                      let o' = (Array.get o_i b) 
                                      in (mjf a o'))
                                    top 
                                    above_s)) in
              let {gen=gen_s; kill=kill_s} = Array.get gka s in
              let temp = mjc gen_s (Bvset.diff key in_s kill_s) in
              let out_s = Array.get o_i s in
              if (Bvset.equal key temp out_s) then run (ws, o_i, d)
              else let _ = Array.set o_i s temp in
                   let below_s = below instrs s in
                   let w' = List.append ws below_s in
                   run (w', o_i, d))
    in run (wl, o_i, d)