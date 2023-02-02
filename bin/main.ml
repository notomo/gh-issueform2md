let handle_error =
  let is_test_mode = ref false in
  let opts = [ ("-test-mode", Arg.Set is_test_mode, "For internal use") ] in

  let () = Arg.parse opts (fun _ -> ()) "" in

  let output, code =
    if !is_test_mode then (print_string, 0) else (prerr_string, 1)
  in
  fun message ->
    message |> output;
    exit code

let () =
  let open Issueform2md in
  ()
  |> Input.from_stdin
  |> Str2yaml.convert
  |> (function
       | Ok x -> x
       | Error e -> e |> handle_error)
  |> Yaml2form.convert
  |> (function
       | Ok x -> x
       | Error e -> e |> Yaml2form.parse_info_to_str |> handle_error)
  |> Form2md.convert
  |> print_string
