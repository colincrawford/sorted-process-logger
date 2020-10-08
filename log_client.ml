open Core

type process =
  { id : string
  ; start_time : int
  }

type t =
  { running_processes : (string, process) Hashtbl.t
  ; finished_processes : (int, process, Int.comparator_witness) Map.t ref
  }

let create () =
  let
    running_processes = Hashtbl.create (module String) and
    finished_processes = ref (Map.empty (module Int))
  in
  { running_processes; finished_processes }

let start log_client pid time =
  let process = { id=pid; start_time=time } in
  let result = Hashtbl.add log_client.running_processes ~key:pid ~data:process in
  match result with | _ -> ()

exception PID_not_found

let stop log_client pid =
  let end_process p =
    let () = Hashtbl.remove log_client.running_processes pid in
    let m = Map.add_exn !(log_client.finished_processes)
              ~key:p.start_time
              ~data:p
    in
    log_client.finished_processes := m
  in
  let process = Hashtbl.find log_client.running_processes pid in
  match process with
  | None -> raise PID_not_found
  | Some p -> end_process p

let poll log_client =
  let remove p =
    let m = Map.remove !(log_client.finished_processes) p.start_time in
    log_client.finished_processes := m
  in
  let process = Map.min_elt !(log_client.finished_processes) in
  match process with
  | None -> None
  | Some k_and_v -> begin
      let p = Tuple2.get2 k_and_v in
      remove p;
      Some p.id
    end

let%expect_test _ =
  let print_result = function
    | None -> print_endline "None"
    | Some s -> print_endline s
  and client = create () in
  start client "1" 12; start client "2" 8; start client "3" 7;
  print_result (poll client);
  stop client "2"; stop client "1"; stop client "3";
  for _=0 to 2 do
    print_result (poll client)
  done;
  [%expect {|
    None
    3
    2
    1
  |}]
