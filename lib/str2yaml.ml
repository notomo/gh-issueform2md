let convert str =
  Yaml.of_string str |> function
  | Ok value -> value
  | Error _ -> failwith "parse yaml"
