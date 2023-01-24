let convert str =
  str |> Yaml.of_string |> function
  | Ok value -> Ok value
  | Error (`Msg x) -> Error ("invalid yaml: " ^ x ^ "\n")
