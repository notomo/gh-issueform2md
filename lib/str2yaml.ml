let convert str =
  str |> Yaml.of_string |> function
  | Ok value -> value
  | Error (`Msg x) -> failwith ("parsing yaml: " ^ x)
