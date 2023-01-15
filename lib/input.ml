let from_stdin () =
  let rec append acc =
    try
      let line = read_line () in
      append (line :: acc)
    with
    | End_of_file -> acc
  in
  let lines = List.rev (append []) in
  String.concat "\n" lines
