let to_string = function
  | Some (`String x) -> x
  | Some x ->
      failwith ("must be string type, but actual: " ^ Yaml.to_string_exn x)
  | None -> failwith "must be string type, but actual: None"

let to_string_option = function
  | Some (`String x) -> Some x
  | None -> None
  | Some x ->
      failwith
        ("must be string type or undefined, but actual: " ^ Yaml.to_string_exn x)

let to_list = function
  | Some (`A xs) -> xs
  | None -> []
  | Some x -> failwith ("must be list type, but actual: " ^ Yaml.to_string_exn x)

let to_string_list x = x |> to_list |> List.map (fun x -> Some x |> to_string)

let to_bool = function
  | Some (`Bool x) -> x
  | None -> false
  | Some x -> failwith ("must be bool type, but actual: " ^ Yaml.to_string_exn x)

let to_values = function
  | None -> []
  | Some (`O values) -> values
  | Some x ->
      failwith ("must be object type but actual: " ^ Yaml.to_string_exn x)
