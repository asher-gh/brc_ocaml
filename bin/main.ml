let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s filename\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let t = Sys.time () in
    let stations = Brc_ocaml.process_file filename in
    let exec_time = Sys.time () -. t in
    Printf.printf "Execution time: %fs\n" exec_time;
    Brc_ocaml.print_results stations
