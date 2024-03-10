type station = {
  name : string;
  mutable total_temp : float;
  mutable min_temp : float;
  mutable max_temp : float;
  mutable count : int;
}

let stations = Hashtbl.create 10000

let update_station_data name temp =
  let data =
    if Hashtbl.mem stations name then Hashtbl.find stations name
    else { name; min_temp = 0.; max_temp = 0.; total_temp = 0.; count = 0 }
  in
  data.min_temp <- min data.min_temp temp;
  data.max_temp <- max data.max_temp temp;
  data.total_temp <- data.total_temp +. temp;
  data.count <- data.count + 1;
  Hashtbl.replace stations name data

let parse_line line =
  match String.split_on_char ';' line with
  | [ name; temp_str ] ->
      let temp = float_of_string temp_str in
      update_station_data name temp
  | _ -> ()

let process_file filename =
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      parse_line line
    done
  with End_of_file -> close_in ic

let _print_results () =
  Hashtbl.iter
    (fun _ data ->
      let avg_temp = data.total_temp /. float_of_int data.count in
      Printf.printf "Station: %s, Min: %.2f, Max: %.2f, Avg: %2f\n" data.name
        data.min_temp data.max_temp avg_temp)
    stations

let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s filename\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let t = Sys.time () in
    process_file filename;
    let exec_time = Sys.time () -. t in
    (* print_results (); *)
    Printf.printf "Execution time: %fs\n" exec_time
