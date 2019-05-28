(* Put your implementation in this file. *)

open Instr

type prog_ret = [ `Reg of value | `Halt of value ]

let trace_find a b = (* debugging find *)
  try Hashtbl.find a b
  with e ->
    let msg = Printexc.to_string e
    and stack = Printexc.get_backtrace () in
    Printf.eprintf "there was an error: %s%s\n" msg stack;
    raise e

(*small API for manipulating the heap*)
let heap_find h k =
  match (h,k) with
  | ((vh,_), (`L_Id id)) -> trace_find vh (`L_Id id)
  | ((_,th), (`L_Loc l)) -> trace_find th (`L_Loc l)

let heap_find_val h (k : id) : value =
  let (vh,_) = h in trace_find vh k

let heap_find_tbl h (k : loc) : table =
  let (_,th) = h in trace_find th k

let heap_replace h k v =
  match (h,k,v) with
  | ((_,th),(`L_Loc _),(`L_Tbl _)) -> Hashtbl.replace th k v
  | ((vh,_),_,_) -> Hashtbl.replace vh k v

let heap_replace_val h k v =
  let (vh,_) = h in Hashtbl.replace vh k v

let heap_replace_tbl h k v =
  let (_,th) = h in Hashtbl.replace th k v

let _ = Printexc.record_backtrace true

let max_regs (p:prog) (f:string):int =
  let instrs = trace_find p f in (*lookup f in the program*)
  let maxOf (i : instr) (m : int) = (*local max with accumulator m*)
    match i with
    | I_const (`L_Reg i,_) -> max i m
    | I_mov (`L_Reg i, `L_Reg j) -> max i (max j m)
    | I_add (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_sub (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_mul (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_div (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_eq  (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_lt  (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_leq (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_is_int (`L_Reg i, `L_Reg j) -> max i (max j m)
    | I_is_str (`L_Reg i, `L_Reg j) -> max i (max j m)
    | I_is_tab (`L_Reg i, `L_Reg j) -> max i (max j m)
    | I_jmp _ -> m
    | I_if_zero (`L_Reg i,_) -> max i m
    | I_rd_glob (`L_Reg i,_) -> max i m
    | I_wr_glob (_,`L_Reg i) -> max i m
    | I_mk_tab (`L_Reg i)    -> max i m
    | I_rd_tab (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_wr_tab (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_has_tab (`L_Reg i, `L_Reg j, `L_Reg k) -> max i (max j (max k m))
    | I_call (`L_Reg i, _, _) -> max i m
    | I_ret (`L_Reg i) -> max i m
    | I_halt (`L_Reg i) -> max i m
  in Array.fold_right maxOf instrs 0 (*apply max repeatedly for each instr*)

let current_inst (p:prog) ((h, s):config):instr =
  match s with
  | (f,pc,_) :: s' -> (trace_find p f).(pc)
  | []             -> failwith "current_inst: No program loaded"



let rec run_inst (p:prog) ((h, s):config):config =
match s with
| [] -> failwith "run_inst: empty stack"
| (f,pc,regs) :: s' ->
  let rec run_inst' (instr : instr) ((h,s) : config) : config = (*recursive helper function for use with iter*)
    (match instr with
    | I_const (`L_Reg i,v) ->
      let _ = Hashtbl.replace regs i v
      in (h,(f,pc+1,regs)::s')
    | I_mov (`L_Reg i, `L_Reg j) ->
      let v = trace_find regs j in
      let _ = Hashtbl.replace regs i v in
      (h,(f,pc+1,regs)::s')
    | I_add (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let v1 = trace_find regs j in
      let v2 = trace_find regs k in
      let v = match (v1,v2) with (* runtime type checking *)
             | (`L_Int n,`L_Int m) -> n + m
             | _ -> failwith "tried to + two non-ints"
      in
      let _ = Hashtbl.replace regs i (`L_Int v) in
      (h,(f,pc+1,regs) :: s')
    | I_sub (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let v1 = trace_find regs j in
      let v2 = trace_find regs k in
      let v = match (v1,v2) with (* runtime type checking *)
             | (`L_Int n,`L_Int m) -> n - m
             | _ -> failwith "tried to - two non-ints"
      in
      let _ = Hashtbl.replace regs i (`L_Int v) in
      (h,(f,pc+1,regs) :: s')
    | I_mul (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let v1 = trace_find regs j in
      let v2 = trace_find regs k in
      let v = match (v1,v2) with (* runtime type checking *)
             | (`L_Int n,`L_Int m) -> n * m
             | _ -> failwith "tried to * two non-ints"
      in
      let _ = Hashtbl.replace regs i (`L_Int v) in
      (h,(f,pc+1,regs) :: s')
    | I_div (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let v1 = trace_find regs j in
      let v2 = trace_find regs k in
      let v = match (v1,v2) with (* runtime type checking *)
             | (`L_Int n,`L_Int 0) -> failwith "tried to divide by 0"
             | (`L_Int n,`L_Int m) -> n / m
             | _ -> failwith "tried to / two non-ints"
      in
      let _ = Hashtbl.replace regs i (`L_Int v) in
      (h,(f,pc+1,regs) :: s')
    | I_eq  (`L_Reg i, `L_Reg j, `L_Reg k) -> (*value eq is ocaml = *)
      let v1 = trace_find regs j in
      let v2 = trace_find regs k in
      let e  = if v1 = v2 then `L_Int 1 else `L_Int 0 in
      let _ = Hashtbl.replace regs i e in
      (h,(f,pc+1,regs) :: s')
    | I_lt  (`L_Reg i, `L_Reg j, `L_Reg k) -> (*ocaml < is poliymorphic for some reason*)
      let v1 = trace_find regs j in           (*seriously, compare strings. it's fun!*)
      let v2 = trace_find regs k in
      let lt  = if v1 < v2 then `L_Int 1 else `L_Int 0 in
      let _ = Hashtbl.replace regs i lt in
      (h,(f,pc+1,regs) :: s')
    | I_leq (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let v1 = trace_find regs j in
      let v2 = trace_find regs k in
      let lte  = if v1 <= v2 then `L_Int 1 else `L_Int 0 in
      let _ = Hashtbl.replace regs i lte in
      (h,(f,pc+1,regs) :: s')
    | I_is_int (`L_Reg i, `L_Reg j) ->
      let v = match trace_find regs j with (*constructor type checking*)
              | `L_Int _ -> `L_Int 1
              | _        -> `L_Int 0
      in
      let _ = Hashtbl.replace regs i v in
      (h,(f,pc+1,regs) :: s')

    | I_is_str (`L_Reg i, `L_Reg j) ->
      let v = match trace_find regs j with (*constructor type checking *)
              | `L_Str _ -> `L_Int 1
              | _        -> `L_Int 0
      in
      let _ = Hashtbl.replace regs i v in
      (h,(f,pc+1,regs) :: s')
    | I_is_tab (`L_Reg i, `L_Reg j) ->
      let v = match trace_find regs j with(*constructor type checking *)
              | `L_Loc _ -> `L_Int 1
              | _        -> `L_Int 0
      in
      let _ = Hashtbl.replace regs i v in
      (h,(f,pc+1,regs) :: s')
    | I_jmp offset -> (h,(f,pc+offset+1,regs) :: s') (*let pc lookup throw an error if out of bounds*)
    | I_if_zero (`L_Reg i,offset) ->
      (match trace_find regs i with
      | `L_Int 0 -> (h,(f,pc+offset+1,regs) :: s')
      | _        -> (h,(f,pc+1,regs) :: s'))
    | I_rd_glob (`L_Reg r,`L_Id id) ->
      let _ = Hashtbl.replace regs r (heap_find_val h (`L_Id id)) in
      (h,(f,pc+1,regs) :: s')
    | I_wr_glob (`L_Id i,`L_Reg r) ->
      let _ = heap_replace_val h (`L_Id i) (trace_find regs r) in
      (h,(f,pc+1,regs) :: s')
    | I_mk_tab (`L_Reg i)    ->
      let (_,th) = h in (*locations are numbers, so just find the biggest one in the heap +1 for fresh loc*)
      let max_loc = Hashtbl.fold (fun a _ c -> match a with `L_Loc a' -> if a' > c then a' else c | _ -> c) th 0 in
      let _ = heap_replace_tbl h (`L_Loc (max_loc + 1)) (`L_Tbl (Hashtbl.create 0)) in
      let _ = Hashtbl.replace regs i (`L_Loc (max_loc + 1)) in
      (h,(f,pc+1,regs) :: s')
    | I_rd_tab (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let l = match trace_find regs j with `L_Loc l -> `L_Loc l | _ -> failwith "rd_tab not called on a table" in
      let (`L_Tbl tbl) = heap_find_tbl h l in
      let key = trace_find regs k in
      let v = trace_find tbl key in
      let _ = Hashtbl.replace regs i v in
      (h,(f,pc+1,regs) :: s')
    | I_wr_tab (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let l = match trace_find regs i with `L_Loc l -> `L_Loc l | _ -> failwith "wr_tab not called on a table" in
      let (`L_Tbl tbl) = heap_find_tbl h l in
      let key = trace_find regs j in
      let v = trace_find regs k in
      let _ = Hashtbl.replace tbl key v in
      (h,(f,pc+1,regs) :: s')
    | I_has_tab (`L_Reg i, `L_Reg j, `L_Reg k) ->
      let l = match trace_find regs j with `L_Loc l -> `L_Loc l | _ -> failwith "has_tab not called on a table" in
      let (`L_Tbl tbl) = heap_find_tbl h l in
      let key = trace_find regs k in
      let v = match Hashtbl.find_opt tbl key with
              | Some _ -> 1
              | None   -> 0
      in
      let _ = Hashtbl.replace regs i (`L_Int v) in
      (h,(f,pc+1,regs) :: s')
    | I_call (`L_Reg i, first, last) -> (*ok here is the fun one *)
      (match trace_find regs i with (*are we trying to call a function?*)
      | `L_Id id -> (*yes? yay!*)
         (match Hashtbl.find_opt p id with (*is this function defined by the program?*)
          | Some _ -> (*yes? ok then! *)
             let regs' = Hashtbl.create (Hashtbl.length regs) in
             let _ = Hashtbl.fold (fun key value _ -> if key >= first && key <= last then Hashtbl.replace regs' (key - first) value else ()) regs () in
             (h,(id,0,regs') :: (f,pc,regs) :: s') (*push onto the call stack *)
          | None -> (* no? then maybe it is an FFI CALL? *)
             (match id with (*is it an ffi call? *)
              | "print_string" ->
                let _ = if first = last then () else failwith "print_int needs one arg" in
                 let str = (match trace_find regs first with
                            | `L_Str str -> str
                            | _         -> failwith "print_string on non-string") in
                 let _ = print_string str in
                 (h,(f,pc+1,regs) :: s')
              | "print_int" ->
                 let _ = if first = last then () else failwith "print_int needs one arg" in
                 let n = (match trace_find regs first with
                          | `L_Int n -> n
                          | _        -> failwith "print_int on non-int") in
                 let _ = print_int n in
                 (h, (f,pc+1,regs) :: s')
              | "to_s" ->
                 let _ = if first = last then () else failwith "to_s needs one arg" in
                 let str = (match trace_find regs first with
                           | `L_Int n -> string_of_int n
                           | `L_Str str -> str
                           | `L_Id id -> "Function<" ^ id ^ ">"
                           | _ -> failwith "to_s on non int, str, or id") in
                 let _ = Hashtbl.replace regs first (`L_Str str) in
                 (h,(f,pc+1,regs) :: s')
              | "to_i" ->
                let _ = if first = last then () else failwith "to_i needs one arg" in
                let n = (match trace_find regs first with
                         | `L_Int n -> n
                         | `L_Str str -> int_of_string str
                         | _ -> failwith "to_i on non string or int") in
                let _ = Hashtbl.replace regs first (`L_Int n) in
                (h,(f,pc+1,regs) :: s')
              | "concat" ->
                let _ = if last - first = 1 then () else failwith "concat needs two args" in
                let str = (match trace_find regs first, trace_find regs (first+1) with
                           | `L_Str str1, `L_Str str2 -> str1 ^ str2
                           | _ -> failwith "concat of two non-strings") in
                let _ = Hashtbl.replace regs first (`L_Str str) in
                (h,(f,pc+1,regs) :: s')
              | "length" ->
                let _ = if first = last then () else failwith "length needs one arg" in
                let n = (match trace_find regs first with
                        | `L_Str str -> String.length str
                        | _ -> failwith "length of non-string") in
                let _ = Hashtbl.replace regs first (`L_Int n) in
                (h,(f,pc+1,regs) :: s')
              | "size" ->
                let _ = if first = last then () else failwith "size needs one arg" in
                let l = match trace_find regs first with `L_Loc l -> `L_Loc l | _ -> failwith "size called on non-table" in
                let (`L_Tbl tbl) = heap_find_tbl h l in
                let n = Hashtbl.length tbl in
                let _ = Hashtbl.replace regs first (`L_Int n) in
                (h,(f,pc+1,regs) :: s')
              | "iter"  -> (* oh boy! *)
                let _ = if (last - first) = 2 then () else failwith "iter needs 3 args" in
                let l = match trace_find regs first with `L_Loc l -> `L_Loc l | _ -> failwith "size called on non-table" in
                (* prep the args *)
                let (`L_Tbl tbl) = heap_find_tbl h l in
                let old_regs = Hashtbl.copy regs in
                let some_fun = (match trace_find regs (first+1) with
                         | `L_Id f -> f
                         | _ -> failwith "iter called on non-function arg") in
                let x = trace_find regs (first+2) in
                let runKV k v _ = (*for some key and value *)
                  let rec runWhile c = (*simulate run_inst for side-effects until we bottom out *)
                    (match c with
                     | (_,[]) -> () (*bottom of call stack; done!*)
                     | _  -> runWhile (run_inst p c)) in
                    let _ = Hashtbl.replace regs 0 k in
                    let _ = Hashtbl.replace regs 1 v in
                    let _ = Hashtbl.replace regs 2 x in
                  runWhile (h,[some_fun,0,regs]) in (*simulate on a stack with the function arg as a simulated "main"*)
                let _ = Hashtbl.fold runKV tbl () in (*simulate for each k,v pair in the table*)
                let _ = Hashtbl.replace old_regs first (`L_Int 0) in (*return 0*)
                (h, (f,pc+1,old_regs) :: s')
              | _ -> failwith ("called a function that doesn't exist " ^ id)))
      | _ -> failwith "calling a non function")
    | I_ret (`L_Reg i) ->
      (match s' with
      | ((f',pc',regs') :: s'') ->
        let first = (match (trace_find p f').(pc') with
                    | (I_call (`L_Reg _, first, _))  -> first (*put return value in register*)
                    | _ -> failwith "returning to not a call stmt") in
        let _ = Hashtbl.replace regs' first (trace_find regs i) in
        (h,(f',pc' + 1, regs') :: s'')
      | [] -> (h,[])) (*design decision: bottom out allowed for simulated calls from iter*)
                      (*user programs cannot initialize a stack that bottoms out otherwise*)
    | I_halt (`L_Reg i) -> (h,s)) in
    run_inst' (trace_find p f).(pc) (h,s)

let run_prog (p:prog):prog_ret =
  let rec run_next (config : config) : [`Reg of value | `Halt of value] =
    match config with
    | (_,["main",pc,regs]) -> (*if in main *)
      (match (trace_find p "main").(pc) with
      | I_ret (`L_Reg r) -> `Reg (trace_find regs r) (*and we are done, finish *)
      | I_halt (`L_Reg r) -> `Halt (trace_find regs r) (*and we halt, finish *)
      | _       -> run_next (run_inst p config)) (*otherwise advance *)
    | (_,(f,pc,regs) :: _) -> (*if we are not in main *)
      (match (trace_find p f).(pc) with
      | I_halt (`L_Reg r) -> `Halt (trace_find regs r) (* and we halt, finish *)
      | _        -> run_next (run_inst p config)) (*otherwise advance *)
    | _ -> run_next (run_inst p config) (*shouldn't ever get an empty config*)
  in run_next ((Hashtbl.create 0, Hashtbl.create 0),["main",0,Hashtbl.create 0])
