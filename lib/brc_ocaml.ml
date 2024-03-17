type temp = int * int

let add_temp (a1, b1) (a2, b2) : temp =
  let new_frac = b1 + b2 in
  let carry, new_frac_corrected =
    if new_frac >= 10 then (1, new_frac - 10) else (0, new_frac)
  in
  (a1 + a2 + carry, new_frac_corrected)

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
  float_of_int whole +. (float_of_int fraction /. 10.0)

type station = {
  mutable total_temp : temp;
  mutable min_temp : temp;
  mutable max_temp : temp;
  mutable count : int;
}

(* NOTE: measurement values will have exactly one fractional digit. *)

let update_station_data stations name temperature =
  let data =
    if Hashtbl.mem stations name then Hashtbl.find stations name
    else
      {
        min_temp = temperature;
        max_temp = temperature;
        total_temp = temperature;
        count = 1;
      }
  in
  (* Update only if station already existed *)
  if Hashtbl.mem stations name then (
    data.min_temp <- min_temp data.min_temp temperature;
    data.max_temp <- max_temp data.max_temp temperature;
    data.total_temp <- add_temp data.total_temp temperature;
    data.count <- data.count + 1);
  Hashtbl.replace stations name data

let parse_line stations line =
  match String.split_on_char ';' line with
  | [ name; temp_str ] ->
      let parts = String.split_on_char '.' temp_str in
      let temp =
        match parts with
        | [ left; right ] -> (int_of_string left, int_of_string right)
        | [ whole ] -> (int_of_string whole, 0)
        | _ -> failwith "Invaild temp format"
      in
      update_station_data stations name temp
  | _ -> ()

let process_file filename =
  let stations = Hashtbl.create 10000 in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      parse_line stations line
    done;
    stations
  with
  | End_of_file ->
      close_in_noerr ic;
      stations
  | e ->
      close_in_noerr ic;
      raise e

let print_results stations =
  Hashtbl.iter
    (fun name data ->
      let avg_temp = temp_to_float data.total_temp /. float_of_int data.count in
      Printf.printf "Station: %s, Min: %.2f, Max: %.2f, Avg: %2f\n" name
        (temp_to_float data.min_temp)
        (temp_to_float data.max_temp)
        avg_temp)
    stations
