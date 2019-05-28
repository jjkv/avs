(* Do not change this file! *)

open Dfa

let dump_res k f r =
  let rec output_set o = function
    | [] -> ()
    | [x] -> f o x
    | x::xs -> Printf.fprintf o "%a, %a" f x output_set xs
  in
    Array.iteri (fun i s -> Printf.printf "%d: {%a}\n" i output_set (Bvset.to_list k s)) r

let main () =
  let input_file = ref "" in
  let analysis = ref "" in
  Arg.parse ["-analysis", Arg.Symbol (["rd"; "ae"; "lv"; "vbe"], (fun s -> analysis := s)), "\tSpecify dataflow analysis"]
    (fun s -> input_file := s)  "Usage: ./main.byte [opts] input_file";
  if !input_file <> "" then
    let chan = open_in (!input_file) in
    let lexbuf = Lexing.from_channel chan in
    let p =
      try Array.of_list (Parser.main Lexer.token lexbuf)
      with _ -> Printf.printf "Parse error!\n"; exit 0 in
    let _ = close_in chan in
    match (!analysis) with
    | "" -> Disassembler.dis_instrs stdout p
    | "ae" ->
       let exprs = exprs_of_prog p in
       let k = Bvset.list_to_key exprs in
       let r = dfa p
		   k
		   D_Forward
		   K_Must
		   avail_exprs
		   0
		   (0 - 1) in
       Printf.printf "Available expressions\n";
       dump_res k output_expr r
    | "vbe" ->
       let exprs = exprs_of_prog p in
       let k = Bvset.list_to_key exprs in
       let r = dfa p
		   k
		   D_Backward
		   K_Must
		   very_busy_exprs
		   0
		   (0 - 1) in
	Printf.printf "Very busy expressions\n";
	dump_res k output_expr r
    | _ -> failwith "Can't happen"
;;

main ()
