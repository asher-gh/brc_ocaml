type temp = int * int

let add_temp (a1, b1) (a2, b2) : temp = (a1 + a2, b1 + b2)

let min_temp (a1, b1) (a2, b2) =
  if a1 < a2 then (a1, b1)
  else if a1 > a2 then (a2, b2)
  else if b1 < b2 then (a1, b1)
  else (a2, b2)

let max_temp (a1, b1) (a2, b2) =
  if a1 > a2 then (a1, b1)
  else if a1 < a2 then (a2, b2)
  else if b1 > b2 then (a1, b1)
  else (a2, b2)

let temp_to_float (whole, fraction) =
  float_of_int whole +. (float_of_int fraction /. 100.0)

type station = {
  name : string;
  mutable total_temp : temp;
  mutable min_temp : temp;
  mutable max_temp : temp;
  mutable count : int;
}

(* NOTE: measurement values will have exactly one fractional digit. *)

let stations = Hashtbl.create 10000

let update_station_data name temperature =
  let data =
    if Hashtbl.mem stations name then Hashtbl.find stations name
    else
      {
        name;
        min_temp = (0, 0);
        max_temp = (0, 0);
        total_temp = (0, 0);
        count = 0;
      }
  in
  data.min_temp <- min_temp data.min_temp temperature;
  data.max_temp <- max_temp data.max_temp temperature;
  data.total_temp <- add_temp data.total_temp temperature;
  data.count <- data.count + 1;
  Hashtbl.replace stations name data

let parse_line line =
  match String.split_on_char ';' line with
  | [ name; temp_str ] ->
      let parts = String.split_on_char '.' temp_str in
      let temp =
        match parts with
        | [ left; right ] -> (int_of_string left, int_of_string right)
        | [ whole ] -> (int_of_string whole, 0)
        | _ -> failwith "Invaild temp format"
      in
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
      let avg_temp = temp_to_float data.total_temp /. float_of_int data.count in
      Printf.printf "Station: %s, Min: %.2f, Max: %.2f, Avg: %2f\n" data.name
        (temp_to_float data.min_temp)
        (temp_to_float data.max_temp)
        avg_temp)
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
