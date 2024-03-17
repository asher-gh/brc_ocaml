open Alcotest
open Brc_ocaml

(* Test for process_file function *)
let test_process_file file_path () =
  let expected = Hashtbl.create 10 in
  (* Fill this with expected data, making sure to reflect what you think the file's data should produce. *)
  Hashtbl.add expected "Dili"
    { total_temp = (52, 1); min_temp = (14, 5); max_temp = (37, 6); count = 2 };

  let result = Brc_ocaml.process_file file_path in
  (* Function to compare two station records for equality, this is necessary for Alcotest to properly compare hash table values. *)
  let compare_stations s1 s2 =
    s1.total_temp = s2.total_temp
    && s1.min_temp = s2.min_temp && s1.max_temp = s2.max_temp
    && s1.count = s2.count
  in

  (* Alcotest testable for the station type *)
  let station_testable =
    Alcotest.testable
      (fun ppf station ->
        Format.fprintf ppf
          "Total: (%d,%d), Min: (%d,%d), Max: (%d,%d), Count: %d"
          (fst station.total_temp) (snd station.total_temp)
          (fst station.min_temp) (snd station.min_temp) (fst station.max_temp)
          (snd station.max_temp) station.count)
      compare_stations
  in

  (* Now actually compare the expected hash table to the result *)
  Hashtbl.iter
    (fun name expected_station ->
      let actual_station = Hashtbl.find result name in
      check station_testable
        (Printf.sprintf "Station: %s" name)
        expected_station actual_station)
    expected

let suite =
  [
    ( "process_file function",
      `Quick,
      test_process_file "../data/measurements_test.csv" );
  ]

let () = Alcotest.run "brc_ocaml" [ ("process_file", suite) ]

(* let () = Alcotest.run "Tests" [ ("Greeting", suite) ] *)

(* let suite = *)
(*   [ *)
(*     ("can greet Tom", `Quick, test_hello_with_name "Tom"); *)
(*     ("can greet John", `Quick, test_hello_with_name "John"); *)
(*   ] *)

(* let () = Alcotest.run "Dummy" [ ("Greeting", suite) ] *)
