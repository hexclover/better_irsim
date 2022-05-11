open Ir

let line_stream_of_channel channel =
  Seq.of_dispenser @@ fun _ ->
  try Some (input_line channel) with End_of_file -> None

let parse_stmt i line =
  match stmt_of_string line with
  | Ok stmt -> stmt
  | Error e ->
      Printf.eprintf "Parsing error: At line %d: %s\n" (i + 1) e;
      exit 1

let () =
  let maybe_open_in = function "-" -> stdin | s -> open_in s in
  let ir_file, input_file =
    match Array.length Sys.argv with
    | 2 -> (maybe_open_in Sys.argv.(1), stdin)
    | 3 -> (maybe_open_in Sys.argv.(1), maybe_open_in Sys.argv.(2))
    | _ ->
        Printf.printf "Usage: %s ir-file [input-file]\n" Sys.argv.(0);
        exit 1
  in
  let ir_stream = Seq.mapi parse_stmt @@ line_stream_of_channel ir_file in
  let input_stream =
    Seq.map Int32.of_string @@ line_stream_of_channel input_file
  in
  match compile ir_stream with
  | Ok ({ codetbl; symtbl } as p) -> (
      Printf.printf "Code\n--------\n";
      pretty_stmt_array stdout codetbl;
      Printf.printf "\nSymbol Table\n--------\n";
      pretty_symtbl stdout symtbl;
      Printf.printf "\nExecution:\n--------\n";
      flush stdout;
      let m =
        new machine
          ~mem_words:(Int32.mul 64l @@ Int32.mul 1024l 1024l)
          ~input:input_stream
          ~output:(fun v ->
            Printf.printf "%ld\n" v;
            flush stdout)
      in
      m#load p;
      let start_time = Sys.time () in
      match m#run () with
      | Ok ret_code ->
          let end_time = Sys.time () in
          if ret_code = 0l then
            Printf.printf "Exited successfully with code %ld\n" ret_code
          else Printf.printf "Exited with non-zero code %ld\n" ret_code;
          Printf.printf "Executed %d instructions in %f seconds.\n"
            m#executed_count
          @@ (end_time -. start_time)
      | Error e ->
          Printf.printf "Error during execution: %s\n" @@ show_emulator_error e)
  | Error e ->
      Printf.eprintf "Error during compilation: %s\n" e;
      exit 1
