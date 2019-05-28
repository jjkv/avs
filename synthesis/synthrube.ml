(* john kerns avs project 5 *)
open Instr
open Boolean
open Bvec
open Symrube

let print_verif o = function
  | `Eq -> Printf.fprintf o "equivalent"
  | `Neq (a, b, c) -> Printf.fprintf o "not equivalent, a=%d, b=%d, c=%d" a b c

let print_synth o = function
  | `Unsat -> Printf.fprintf o "unsat"
  | `Sat (h1, h2, h3) -> Printf.fprintf o "solution, h1=%d, h2=%d, h3=%d" h1 h2 h3

type verif_ret = [ `Eq | `Neq of int * int * int ]

type synth_ret = [ `Unsat | `Sat of int * int * int ]

(* infix compose, this is from stack overflow *)
let (<<) f g x = f(g(x))

let int_to_bvec = (fun n -> Symrube.inv3 @@ Bvec.bvec_of_int n)

(* returns a bexpr that is true when h == m, forall a, b, c *)
let equate_rets (h:fn) (m:fn) =
    (* inserts f into a prog (string * fn) hashtable for run_prog *)
    let mk_prog (name:string) (f:fn) =
        let h = Hashtbl.create 1 in
        let _ = Hashtbl.add h name f
        in  h in
    (* combines list of prog returns into single bvec*)
    let ret_to_bvec = 
        let calzone a r = 
            (match (a, r) with 
                 | ([a0;a1;a2], `Full (`L_Sym [b0;b1;b2], (p:bexpr))) ->
                    (* each bit is disjunction of (path /\ return bit) *)
                   [EOr (a0, EAnd (b0, p));EOr (a1, EAnd (b1, p));EOr (a2, EAnd (b2, p))] 
                 | _ -> failwith "not full return from main")
        in List.fold_left calzone [EFalse;EFalse;EFalse] in
    let h_prog = mk_prog "main" h in (* call them both main for symrube *)
    let m_prog = mk_prog "main" m in
    (* symbolic execution with "unlimited" steps *)
    let h_ret  = Symrube.run_prog h_prog Symrube.sch_bfs max_int in
    let m_ret  = Symrube.run_prog m_prog Symrube.sch_bfs max_int
    (* set the prog returns to equal each other *)
    in  Bvec.eq (ret_to_bvec h_ret) (ret_to_bvec m_ret)

(* converts bvecs x,y,z into integers based on assignment a *)
let conv3 (a:(string * bool) list) (x:bvec) (y:bvec) (z:bvec) =
    let conv =
        let assign     = List.map (Boolean.eval a) in
        let cast_bools = List.map (function false -> EFalse | true -> ETrue)
        in  Bvec.int_of_bvec << cast_bools << assign
    in (conv x, conv y, conv z) 

let verif (h:fn) (m:fn):verif_ret = 
    (match Boolean.sat (ENot (equate_rets h m)) with
         | None   -> `Eq 
         | Some a -> `Neq (conv3 a Symrube.a_bvec Symrube.b_bvec Symrube.c_bvec))

let synth (h:fn) (m:fn):synth_ret =
    (* memory represents the *) 
    let rec synthesize (a:int) (b:int) (c:int) (memory:bexpr) =
        (* substitute concrete values of a,b,c into program *) 
        let replace_symvars (a:int) (b:int) (c:int) =
            (function | I_rd_glob(r, `L_Id "a") -> I_const(r, `L_Sym (int_to_bvec a))
                      | I_rd_glob(r, `L_Id "b") -> I_const(r, `L_Sym (int_to_bvec b))
                      | I_rd_glob(r, `L_Id "c") -> I_const(r, `L_Sym (int_to_bvec c))
                      | i -> i) in
        (* substitute hole values into fn array *)
        let plug_holes (h1:int) (h2:int) (h3:int) = 
            (function | I_const  (r, `L_Hole 1) -> I_const(r, `L_Sym (int_to_bvec h1))
                      | I_const  (r, `L_Hole 2) -> I_const(r, `L_Sym (int_to_bvec h2))
                      | I_const  (r, `L_Hole 3) -> I_const(r, `L_Sym (int_to_bvec h3))
                      | i -> i) in
        (* overwrite array using f *)
        let write_fn_with f a = Array.map f (Array.copy a) in
        let repl   = (replace_symvars a b c) in
        let h', m' = write_fn_with repl h, write_fn_with repl m in
        let test   = EAnd(memory, (equate_rets h' m'))
        in (match (Boolean.sat test) with
                | None      -> `Unsat
                | Some asst ->
                    let h1, h2, h3 = conv3 asst Symrube.h1_bvec Symrube.h2_bvec Symrube.h3_bvec in
                    let m'         = write_fn_with (plug_holes h1 h2 h3) m
                    in (match (verif h m') with
                            | `Eq -> `Sat (h1, h2, h3)
                            | `Neq (a', b', c') -> synthesize a' b' c' test))
    (* begin synthesis on initial values a,b,c=0 *)
    in synthesize 0 0 0 ETrue