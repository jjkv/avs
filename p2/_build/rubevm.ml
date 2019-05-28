(* Put your implementation in this file. *)

open Instr

type prog_ret = [ `Reg of value | `Halt of value ]
       
let max_regs (p:prog) (f:string):int = 
    let found = Hashtbl.find p f in
    let int_max m l = List.fold_left max m l in
    let int_of_reg = (function | `L_Reg i -> i) in 
    let maxx m i = (match i with
        | I_const (r, _) -> max m (int_of_reg r)
        | I_mov (r1, r2) -> int_max m (List.map int_of_reg [r1;r2])
        | I_add (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_sub (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_mul (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_div (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_eq (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_lt (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_leq (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_is_int (r1, r2) -> int_max m (List.map int_of_reg [r1;r2])
        | I_is_str (r1, r2) -> int_max m (List.map int_of_reg [r1;r2])
        | I_is_tab (r1, r2) -> int_max m (List.map int_of_reg [r1;r2])
        | I_jmp _ -> m
        | I_if_zero (r, _) -> max m (int_of_reg r)
        | I_rd_glob (r, _) -> max m (int_of_reg r)
        | I_wr_glob (_, r) -> max m (int_of_reg r)
        | I_mk_tab r -> max m (int_of_reg r)
        | I_rd_tab (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_wr_tab (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_has_tab (r1, r2, r3) -> int_max m (List.map int_of_reg [r1;r2;r3])
        | I_call (r, _, _) -> max m (int_of_reg r)
        | I_ret r -> max m (int_of_reg r)
        | I_halt r -> max m (int_of_reg r))
in Array.fold_left maxx (~- 1) found

let current_inst (p:prog) ((h, s):config):instr = 
    (match s with
        [] -> failwith "empty stack"
      | ((cid, cpc, regs)::rest) -> 
            let f = Hashtbl.find p cid in
            let i = Array.get f cpc in i)

let rec run_inst (p:prog) ((h, s):config):config =
    let int_of_lint = (function | `L_Int i -> i | _ -> failwith "expected lint: not an int") in
    let rec fresh_l t:value = 
        let i_float = (Sys.time ()) *. 1000000.0 in
        let i_int = (int_of_float i_float) mod (max_int - 1)
        in if (Hashtbl.mem t (`L_Loc i_int)) then (fresh_l t)
           else (`L_Loc i_int)
    in (match s with
        | [] -> failwith "empty stack"
        | ((cid, cpc, regs)::rest) -> (match (current_inst p (h, s)) with
            | I_const ((`L_Reg r), v) ->
                let _ = Hashtbl.replace regs r v
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_mov ((`L_Reg r1), (`L_Reg r2)) ->
                let v = Hashtbl.find regs r2 in
                let _ = Hashtbl.replace regs r1 v
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_add ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) -> 
                let n1 = int_of_lint (Hashtbl.find regs r2) in
                let n2 = int_of_lint (Hashtbl.find regs r3) in
                let sum = n1 + n2 in
                let _ = Hashtbl.replace regs r1 (`L_Int sum)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_sub ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) -> 
                let n1 = int_of_lint (Hashtbl.find regs r2) in
                let n2 = int_of_lint (Hashtbl.find regs r3) in
                let diff = n1 - n2 in
                let _ = Hashtbl.replace regs r1 (`L_Int diff)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_mul ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) -> 
                let n1 = int_of_lint (Hashtbl.find regs r2) in
                let n2 = int_of_lint (Hashtbl.find regs r3) in
                let prod = n1 * n2 in
                let _ = Hashtbl.replace regs r1 (`L_Int prod)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_div ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) ->
                let n1 = int_of_lint (Hashtbl.find regs r2) in
                let n2 = int_of_lint (Hashtbl.find regs r3) in
                let quot = n1 / n2 in
                let _ = Hashtbl.replace regs r1 (`L_Int quot)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_eq ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) ->
                let v1 = Hashtbl.find regs r2 in
                let v2 = Hashtbl.find regs r3 in
                let res = ((function (v', v'') -> if (v' = v'') then 1 else 0) (v1, v2)) in
                let _ = Hashtbl.replace regs r1 (`L_Int res)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_lt ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) ->
                let n1 = int_of_lint (Hashtbl.find regs r2) in
                let n2 = int_of_lint (Hashtbl.find regs r3) in
                let res = ((function (n', n'') -> if (n' < n'') then 1 else 0) (n1, n2)) in
                let _ = Hashtbl.replace regs r1 (`L_Int res)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_leq ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) ->
                let n1 = int_of_lint (Hashtbl.find regs r2) in
                let n2 = int_of_lint (Hashtbl.find regs r3) in
                let res = ((function (n', n'') -> if (n' <= n'') then 1 else 0) (n1, n2)) in
                let _ = Hashtbl.replace regs r1 (`L_Int res)
                in (h, (cid, (1 + cpc), regs)::rest) 
            | I_is_int ((`L_Reg r1), (`L_Reg r2)) -> 
                let res = (function | `L_Int _ -> 1 | _ -> 0) (Hashtbl.find regs r2) in
                let _ = Hashtbl.replace regs r1 (`L_Int res)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_is_str ((`L_Reg r1), (`L_Reg r2)) ->
                let res = (function | `L_Str _ -> 1 | _ -> 0) (Hashtbl.find regs r2) in
                let _ = Hashtbl.replace regs r1 (`L_Int res)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_is_tab ((`L_Reg r1), (`L_Reg r2)) ->
                let res = (function | `L_Loc _ -> 1 | _ -> 0) (Hashtbl.find regs r2) in
                let _ = Hashtbl.replace regs r1 (`L_Int res)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_jmp n -> (h, (cid, (1 + cpc + n), regs)::rest)
            | I_if_zero ((`L_Reg r), n) ->
                let get_jump = (function | `L_Int x -> if (x = 0) then n else 0 | _ -> failwith "not an int") in
                let jump = get_jump (Hashtbl.find regs r)
                in (h, (cid, (1 + cpc + jump), regs)::rest)
            | I_rd_glob ((`L_Reg r), (`L_Id i)) -> 
                let v = Hashtbl.find h (`L_Id i) in
                let _ = Hashtbl.replace regs r v
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_wr_glob ((`L_Id i), (`L_Reg r)) ->
                let v = Hashtbl.find regs r in
                let _ = Hashtbl.replace h (`L_Id i) v
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_mk_tab (`L_Reg r) ->
                let l = (fresh_l h) in
                let new_t = (`L_Tab (Hashtbl.create 1738)) in
                let _ = Hashtbl.replace regs r l in
                let _ = Hashtbl.replace h l new_t
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_rd_tab ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) ->
                let l = Hashtbl.find regs r2 in
                let v = Hashtbl.find regs r3 in
                let t = (function | `L_Tab x -> x | _ -> failwith "not a table") (Hashtbl.find h l) in
                let v' = Hashtbl.find t v in
                let _ = Hashtbl.replace regs r1 v'
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_wr_tab ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) ->
                let l = Hashtbl.find regs r1 in
                let v = Hashtbl.find regs r2 in
                let v' = Hashtbl.find regs r3 in
                let t = (function `L_Tab x -> x | _ -> failwith "not a table") (Hashtbl.find h l) in
                let _ = Hashtbl.replace t v v' in
                let _ = Hashtbl.replace h l (`L_Tab t)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_has_tab ((`L_Reg r1), (`L_Reg r2), (`L_Reg r3)) ->
                let l = Hashtbl.find regs r2 in
                let v = Hashtbl.find regs r3 in
                let t = Hashtbl.find h l in
                let res = (function `L_Tab x -> if (Hashtbl.mem x v) then 1 else 0 | _ -> failwith "not a table") t in
                let _ = Hashtbl.replace regs r1 (`L_Int res)
                in (h, (cid, (1 + cpc), regs)::rest)
            | I_call ((`L_Reg r), n1, n2) ->
                let nid = (function `L_Id x -> x | _ -> failwith "not an id") (Hashtbl.find regs r) in
                let iterate return_val = 
                    let _ = assert ((n2 - n1) = 2) in
                    let lt = (Hashtbl.find regs n1) in
                    let t = (function `L_Tab x -> x | _ -> failwith "not a table") (Hashtbl.find h lt) in
                    let f = (Hashtbl.find regs (n1 + 1)) in
                    let x = (Hashtbl.find regs n2) in
                    let mk_f k v a =
                        let ik  = (I_const ((`L_Reg 5), k)) in
                        let iv = (I_const ((`L_Reg 6), v)) in
                        let ix = (I_const ((`L_Reg 7), x)) in
                        let ii = (I_const ((`L_Reg 8), f)) in
                        let ic = (I_call ((`L_Reg 8), 5, 7)) in
                        let ia = [ik;iv;ix;ii;ic]
                        in List.append a ia in
                    let ni  = Hashtbl.fold mk_f t [] in
                    let last = [(I_const ((`L_Reg 0), (`L_Int 0))); I_ret (`L_Reg 0)] in
                    let instr_array = Array.of_list (List.append ni last) in
                    let rec new_name () =
                        let l = (fresh_l h) in
                        let nme = "iter function "^((function | `L_Loc x -> (string_of_int x) | _ -> failwith "wtf") l) in
                        if (Hashtbl.mem p nme) then new_name ()
                        else nme in
                    let new_fname = new_name () in
                    let mr1 = ((max_regs p cid) + 1) in
                    let _ = Hashtbl.replace p new_fname instr_array in
                    let eregs = Hashtbl.create 1738 in
                    (h, (new_fname, 0, eregs)::(cid, cpc, regs)::rest) in
                let intercept nid = 
                    if (Hashtbl.mem p nid) then (false, (`L_Int 0)) else
                    (match nid with
                        | "print_string" -> 
                            let s = (assert (n1 = n2)); (Hashtbl.find regs n1) in
                            let s' = (function `L_Str x -> x | _ -> failwith "not string for print_string") s in
                            let _ =  Printf.printf "%s" s' in (true, (`L_Str s'))  
                        | "print_int" -> 
                            let n = (assert (n1 = n2)); (Hashtbl.find regs n1) in
                            let n' = (function `L_Int x -> x | _ -> failwith "not int for print_int") n in
                            let _ = Printf.printf "%d" n' in (true, (`L_Int n'))
                        | "to_s" -> 
                            let arg = (assert (n1 = n2)); (Hashtbl.find regs n1) in
                            let convert a = (match a with
                                | (`L_Int x) -> `L_Str (string_of_int x)
                                | (`L_Str _) -> a 
                                | (`L_Id x) -> `L_Str ("Function<"^x^">")
                                | _ -> failwith "cannot convert table or location to string")
                            in (true, (convert arg))
                        | "to_i" ->
                            let arg = (assert (n1 = n2)); (Hashtbl.find regs n1) in
                            let convert a = (match a with
                                | (`L_Int _) -> a
                                | (`L_Str x) -> `L_Int (int_of_string x)
                                | (`L_Id x) -> `L_Int (int_of_string x)
                                | _ -> failwith "cannot convert value to int")
                            in (true, (convert arg))
                        | "concat" -> 
                            let s1 = (Hashtbl.find regs n1) in
                            let s2 = (Hashtbl.find regs n2)
                            in (match (s1, s2) with 
                                    ((`L_Str x), (`L_Str y)) -> (true, (`L_Str (x ^ y)))
                                  | _ -> failwith "can't concat non strings")
                        | "length" ->
                            let s = (assert (n1 = n2)); (Hashtbl.find regs n1) in
                            (match s with
                                | `L_Str x -> (true, `L_Int (String.length x))
                                | _ -> failwith "not a string for length")
                        | "size" ->
                            let l = (assert (n1 = n2)); (Hashtbl.find regs n1) in
                            let t = Hashtbl.find h l in
                            (match t with
                                | `L_Tab x -> (true, `L_Int (Hashtbl.length x))
                                | _ -> failwith "not a table for size")
                        | _ -> (false, (`L_Int 0))) in
                let (res, ret) = (intercept nid) in
                if res then 
                    let _ = Hashtbl.replace regs n1 ret in (h, (cid, (1 + cpc), regs)::rest)
                else if (nid = "iter") then iterate n1
                    else
                    let r2 = (Hashtbl.create 1738) in
                    let diff = n2 - n1 in
                    let rec mk_bindings i t =
                        let v' = Hashtbl.find regs (n1 + i) in 
                        let _ = Hashtbl.replace t i v' in
                        if (i = diff) then t
                        else mk_bindings (i + 1) t in
                    let r2' = mk_bindings 0 r2 in
                    let s' = (nid, 0, r2')::(cid, cpc, regs)::rest 
                    in (h, s')
            | I_ret (`L_Reg r) ->
                let (id, pc, r1), (id2, pc2, r2), s' = (function (x::y::r) -> x, y, r | _ -> failwith "small stack") s in
                let v = Hashtbl.find r1 r in
                let f = Hashtbl.find p id2 in
                let i = Array.get f pc2 in
                let n1 = (function (I_call (_, n1, _)) -> n1 | _ -> failwith "not call") i in
                let _ = Hashtbl.replace r2 n1 v
                in (h, (id2, (pc2 + 1), r2)::s') 
            | I_halt (`L_Reg r) -> 
                if (Hashtbl.mem regs r) then (h, [])
                else failwith "bad halt" ))

let run_prog (p:prog):prog_ret = 
    let empty_h = Hashtbl.create 1738 in
    let empty_r = Hashtbl.create 1738 in
    let i_config = empty_h, [("main", 0, empty_r)] in
    let rec execute p' c' =
        let n_config = run_inst p' c' in
        let (h', s') = n_config in
        let (i, pc, r) = List.hd s' in
        let rs = List.tl s' in
        let f = Hashtbl.find p' i in
        let i' = Array.get f pc in
        (match (rs, i') with 
          | [], (I_ret (`L_Reg r')) -> 
            if not (i = "main") then execute p' n_config
            else let res = Hashtbl.find r r' in (`Reg res)
          | _, (I_halt (`L_Reg r')) ->
            let res = Hashtbl.find r r' in (`Halt res)
          | _, _ -> execute p' n_config)
    in execute p i_config
