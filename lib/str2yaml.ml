let convert str =
  str |> Yaml.of_string |> function
  | Ok value -> value
  | Error _ -> failwith "parse yaml"
