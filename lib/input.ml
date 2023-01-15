let from_stdin () =
  let rec append_from_stdin acc =
    try
      let line = read_line () in
      append_from_stdin (line :: acc)
    with
    | End_of_file -> acc
  in
  let lines = List.rev (append_from_stdin []) in
  String.concat "\n" lines
