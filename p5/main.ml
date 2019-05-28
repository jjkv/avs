(* Do not change this file! *)

open Disassembler

type mode =
  | Verif
  | Synth

let main () =
  let input_file = ref "" in
  let mode = ref Verif in
  Arg.parse ["-m", Arg.Symbol (["verif"; "synth"], (fun s -> if s = "verif" then mode := Verif else mode := Synth)), "\tVerification or Synthesis"]
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
    let h = Array.of_list (List.assoc "harness" p) in
    let m = Array.of_list (List.assoc "main" p) in
    match !mode with
    | Verif ->
       let res = Synthrube.verif h m in
       Synthrube.print_verif stdout res
    | Synth ->
       let res = Synthrube.synth h m in
       Synthrube.print_synth stdout res
  else
    Printf.printf "Usage: ./main.byte input_file\n"

;;

main ()
