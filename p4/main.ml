(* Do not change this file! *)



let main () =
  let input_file = ref "" in
  let max_steps = ref 0 in
  let sch = ref Symrube.sch_bfs in
  Arg.parse ["-steps", Arg.Int (fun n -> max_steps := n), "\tMax symbolic execution steps";
	    "-sch", Arg.Symbol (["bfs"; "dfs"], (fun s -> if s = "bfs" then sch := Symrube.sch_bfs else sch := Symrube.sch_dfs)), "\tScheduler"]
    (fun s -> input_file := s)  "Usage: ./main.byte [opts] input_file";
  if !input_file <> "" then
    let chan = open_in (!input_file) in
    let lexbuf = Lexing.from_channel chan in
    let p =
      try Parser.main Lexer.token lexbuf
      with _ -> Printf.printf "Parse error!\n"; exit 0 in
    let p' = Hashtbl.create 17 in
    let _ = List.iter (fun (f, is) -> Hashtbl.add p' f (Array.of_list is)) p in
    let _ = close_in chan in
    if !max_steps = 0 then
       Disassembler.disassemble p'
    else
      let results = Symrube.run_prog p' (!sch) (!max_steps) in
      let _ = Symrube.print_results stdout results in
      Printf.printf "\n"
  else
    Printf.printf "Usage: ./main.byte input_file\n"

;;

main ()
