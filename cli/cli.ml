open Better_irsim.Ir

let program_name = Sys.argv.(0)
let verbose = ref false
let division = ref None
let ir_file, input_file = (ref "-", ref "-")
let usage_msg = program_name ^ " [-verbose] [ir-file] [input-file]"
let maybe_open_in = function "-" -> stdin | s -> open_in s

let () =
  let open Arg in
  let set_division s =
    match String.lowercase_ascii s with
    | "trunc" -> division := Some Trunc
    | "rounddown" -> division := Some RoundDown
    | _ -> raise @@ Bad ("Unknown division rounding behavior: " ^ s)
  in
  let anon_cnt = ref 0 in
  let anon_fun s =
    (match !anon_cnt with
    | 0 -> ir_file := s
    | 1 -> input_file := s
    | _ -> raise @@ Bad ("Unrecognized positional argument: " ^ s));
    anon_cnt := !anon_cnt + 1
  in
  let speclist =
    [
      ("-verbose", Set verbose, "Print program and execution information");
      ( "-division",
        String set_division,
        "Set integer division behavior: <rounddown|trunc> (default is \
         rounddown)" );
      ( "-",
        Unit (fun () -> anon_fun "-"),
        "When specifying a file, stands for STDIN" );
    ]
  in
  parse speclist anon_fun usage_msg

let ir_file, input_file = (maybe_open_in !ir_file, maybe_open_in !input_file)

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
  let ir_stream = Seq.mapi parse_stmt @@ line_stream_of_channel ir_file in
  let input_stream =
    Seq.map Int32.of_string @@ line_stream_of_channel input_file
  in
  match compile ir_stream with
  | Ok ({ codetbl; symtbl } as p) -> (
      if !verbose then (
        Printf.printf "Code\n--------\n";
        pretty_stmt_array stdout codetbl;
        Printf.printf "\nSymbol Table\n--------\n";
        pretty_symtbl stdout symtbl;
        Printf.printf "\nExecution:\n--------\n";
        flush stdout);
      let m =
        new machine
          ~mem_words:(Int32.mul 64l @@ Int32.mul 1024l 1024l)
          ~input:input_stream
          ~output:(fun v ->
            Printf.printf "%ld\n" v;
            flush stdout)
      in
      Option.iter (fun x -> m#set_division_flavor x) !division;
      m#load p;
      let start_time = Sys.time () in
      match m#run () with
      | Ok ret_code ->
          let end_time = Sys.time () in
          if !verbose then (
            if ret_code = 0l then
              Printf.printf "Exited successfully with code %ld\n" ret_code
            else Printf.printf "Exited with non-zero code %ld\n" ret_code;
            Printf.printf "Executed %d instructions in %f seconds.\n"
              m#executed_count
            @@ (end_time -. start_time));
          exit @@ Int32.to_int ret_code
      | Error e ->
          Printf.printf "Error during execution: %s\n" @@ show_emulator_error e)
  | Error e ->
      Printf.eprintf "Error during compilation: %s\n" e;
      exit 1
