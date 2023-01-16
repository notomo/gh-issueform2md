let to_string = function
  | Some (`String x) -> x
  | _ -> failwith "unexpected string type"

let to_string_option = function
  | Some (`String x) -> Some x
  | Some _ -> failwith "unexpected string option type"
  | None -> None

let to_string_list = function
  | Some (`A xs) ->
      List.map
        (function
          | `String s -> s
          | _ -> failwith "unexpected type")
        xs
  | Some _ -> failwith "unexpected type"
  | None -> []

let to_bool = function
  | Some (`Bool x) -> x
  | _ -> false

let to_values = function
  | None -> []
  | Some (`O values) -> values
  | Some x -> failwith ("unexpected type: " ^ Yaml.to_string_exn x)
