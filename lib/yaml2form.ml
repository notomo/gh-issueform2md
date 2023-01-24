type parse_info = {
  field : string;
  message : string option;
  children : parse_info list;
}

let ( let* ) v k =
  match v with
  | Ok x -> k x
  | Error e -> Error e

let rec extract key (values, info) =
  match values with
  | [] -> (None, { info with field = key; message = None })
  | (k, v) :: xs ->
      if k = key then (Some v, { info with field = key; message = None })
      else extract key (xs, info)

let must_extract key (values, info) =
  extract key (values, info) |> function
  | None, info -> Error { info with message = Some "must not be undefined" }
  | Some x, info -> Ok (x, info)

let yaml_to_str x =
  x |> function
  | `Null -> "null"
  | v -> v |> Yaml.to_string_exn |> String.trim

let to_values (v, info) =
  v |> function
  | None -> Ok ([], { info with message = None })
  | Some (`O xs) -> Ok (xs, { info with message = None })
  | Some x ->
      Error
        {
          info with
          message = Some ("must be object type but actual: " ^ yaml_to_str x);
        }

let to_must_values (v, info) =
  v |> function
  | `O xs -> Ok (xs, { info with message = None })
  | x ->
      Error
        {
          info with
          message = Some ("must be object type but actual: " ^ yaml_to_str x);
        }

let to_string (v, info) =
  v |> function
  | `String x -> Ok x
  | x ->
      Error
        {
          info with
          message = Some ("must be string type but actual: " ^ yaml_to_str x);
        }

let to_string_option (v, info) =
  v |> function
  | Some (`String x) -> Ok (Some x)
  | Some x ->
      Error
        {
          info with
          message = Some ("must be string type but actual: " ^ yaml_to_str x);
        }
  | None -> Ok None

let to_bool (v, info) =
  v |> function
  | Some (`Bool x) -> Ok x
  | None -> Ok false
  | Some x ->
      Error
        {
          info with
          message = Some ("must be bool type but actual: " ^ yaml_to_str x);
        }

let option_to_list (v, info) =
  v |> function
  | Some (`A xs) -> Ok xs
  | None -> Ok []
  | Some x ->
      Error
        {
          info with
          message = Some ("must be list type but actual: " ^ yaml_to_str x);
        }

let to_list (v, info) =
  v |> function
  | `A xs -> Ok xs
  | x ->
      Error
        {
          info with
          message = Some ("must be list type but actual: " ^ yaml_to_str x);
        }

let to_string_list (v, info) =
  let* values = (v, info) |> option_to_list in
  let strs =
    values
    |> Listutil.enumerate (fun (i, x) ->
           (x, { field = Int.to_string i; message = None; children = [] })
           |> to_string)
  in
  let errs = strs |> Listutil.all_errors in
  match errs with
  | [] -> Ok (strs |> List.map Result.get_ok)
  | _ -> Error { info with children = errs }

let to_err = function
  | Ok _ -> None
  | Error x -> Some x

let to_validations (values, info) =
  let* v = (values, info) |> extract "validations" |> to_values in
  let required = v |> extract "required" |> to_bool in
  match required with
  | Ok x -> Ok ({ required = x } : Form.validations)
  | Error err ->
      Error { field = "validations"; message = None; children = [ err ] }

let to_markdown (values, info) =
  let* attributes =
    let* attrs =
      let* x = (values, info) |> must_extract "attributes" in
      x |> to_must_values
    in
    let* value =
      let* x = attrs |> must_extract "value" in
      x |> to_string
    in
    Ok ({ value } : Form.Markdown.attributes)
  in
  Ok (Form.ElementMarkdown { attributes })

let to_textarea (values, info) =
  let attributes =
    let* attrs =
      let* x = (values, info) |> must_extract "attributes" in
      x |> to_must_values
    in
    let label =
      let* x = attrs |> must_extract "label" in
      x |> to_string
    in
    let description = attrs |> extract "description" |> to_string_option in
    let placeholder = attrs |> extract "placeholder" |> to_string_option in
    let value = attrs |> extract "value" |> to_string_option in
    let render = attrs |> extract "render" |> to_string_option in
    let id = attrs |> extract "id" |> to_string_option in
    let errs =
      [
        label |> to_err;
        description |> to_err;
        placeholder |> to_err;
        value |> to_err;
        render |> to_err;
        id |> to_err;
      ]
      |> Listutil.all_somes
    in
    match errs with
    | [] ->
        Ok
          ({
             label = Result.get_ok label;
             description = Result.get_ok description;
             placeholder = Result.get_ok placeholder;
             value = Result.get_ok value;
             render = Result.get_ok render;
             id = Result.get_ok id;
           }
            : Form.Textarea.attributes)
    | _ -> Error { field = "attributes"; message = None; children = errs }
  in
  let validations = (values, info) |> to_validations in
  let errs =
    [ attributes |> to_err; validations |> to_err ] |> Listutil.all_somes
  in
  match errs with
  | [] ->
      Ok
        (Form.ElementTextarea
           {
             attributes = Result.get_ok attributes;
             validations = Result.get_ok validations;
           })
  | _ -> Error { info with children = errs }

let to_input (values, info) =
  let attributes =
    let* attrs =
      let* x = (values, info) |> must_extract "attributes" in
      x |> to_must_values
    in
    let label =
      let* x = attrs |> must_extract "label" in
      x |> to_string
    in
    let description = attrs |> extract "description" |> to_string_option in
    let placeholder = attrs |> extract "placeholder" |> to_string_option in
    let value = attrs |> extract "value" |> to_string_option in
    let render = attrs |> extract "render" |> to_string_option in
    let id = attrs |> extract "id" |> to_string_option in
    let errs =
      [
        label |> to_err;
        description |> to_err;
        placeholder |> to_err;
        value |> to_err;
        render |> to_err;
        id |> to_err;
      ]
      |> Listutil.all_somes
    in
    match errs with
    | [] ->
        Ok
          ({
             label = Result.get_ok label;
             description = Result.get_ok description;
             placeholder = Result.get_ok placeholder;
             value = Result.get_ok value;
             id = Result.get_ok id;
           }
            : Form.Input.attributes)
    | _ -> Error { field = "attributes"; message = None; children = errs }
  in
  let validations = (values, info) |> to_validations in
  let errs =
    [ attributes |> to_err; validations |> to_err ] |> Listutil.all_somes
  in
  match errs with
  | [] ->
      Ok
        (Form.ElementInput
           {
             attributes = Result.get_ok attributes;
             validations = Result.get_ok validations;
           })
  | _ -> Error { info with children = errs }

let to_dropdown (values, info) =
  let attributes =
    let* attrs =
      let* x = (values, info) |> must_extract "attributes" in
      x |> to_must_values
    in
    let label =
      let* x = attrs |> must_extract "label" in
      x |> to_string
    in
    let description = attrs |> extract "description" |> to_string_option in
    let multiple = attrs |> extract "multiple" |> to_bool in
    let options =
      let x = attrs |> extract "options" in
      x |> to_string_list
    in
    let id = attrs |> extract "id" |> to_string_option in
    let errs =
      [
        label |> to_err;
        description |> to_err;
        multiple |> to_err;
        options |> to_err;
        id |> to_err;
      ]
      |> Listutil.all_somes
    in
    match errs with
    | [] ->
        Ok
          ({
             label = Result.get_ok label;
             description = Result.get_ok description;
             multiple = Result.get_ok multiple;
             options = Result.get_ok options;
             id = Result.get_ok id;
           }
            : Form.Dropdown.attributes)
    | _ -> Error { field = "attributes"; message = None; children = errs }
  in
  let validations = (values, info) |> to_validations in
  let errs =
    [ attributes |> to_err; validations |> to_err ] |> Listutil.all_somes
  in
  match errs with
  | [] ->
      Ok
        (Form.ElementDropdown
           {
             attributes = Result.get_ok attributes;
             validations = Result.get_ok validations;
           })
  | _ -> Error { info with children = errs }

let to_checkboxes_options (v, info) =
  let* xs = (v, info) |> to_list in
  let opts =
    xs
    |> Listutil.enumerate (fun (i, x) ->
           let* values =
             (x, { field = Int.to_string i; message = None; children = [] })
             |> to_must_values
           in
           let label =
             let* x = values |> must_extract "label" in
             x |> to_string
           in
           match label with
           | Ok x -> Ok ({ label = x } : Form.Checkboxes.option_)
           | Error err ->
               Error
                 { field = Int.to_string i; message = None; children = [ err ] })
  in
  let errs = opts |> Listutil.all_errors in
  match errs with
  | [] -> Ok (opts |> List.map Result.get_ok)
  | _ -> Error { info with children = errs }

let to_checkboxes (values, info) =
  let attributes =
    let* attrs =
      let* x = (values, info) |> must_extract "attributes" in
      x |> to_must_values
    in
    let label =
      let* x = attrs |> must_extract "label" in
      x |> to_string
    in
    let description = attrs |> extract "description" |> to_string_option in
    let options =
      let* x = attrs |> must_extract "options" in
      x |> to_checkboxes_options
    in
    let id = attrs |> extract "id" |> to_string_option in
    let errs =
      [
        label |> to_err; description |> to_err; options |> to_err; id |> to_err;
      ]
      |> Listutil.all_somes
    in
    match errs with
    | [] ->
        Ok
          ({
             label = Result.get_ok label;
             description = Result.get_ok description;
             options = Result.get_ok options;
             id = Result.get_ok id;
           }
            : Form.Checkboxes.attributes)
    | _ -> Error { field = "attributes"; message = None; children = errs }
  in
  let validations = (values, info) |> to_validations in
  let errs =
    [ attributes |> to_err; validations |> to_err ] |> Listutil.all_somes
  in
  match errs with
  | [] ->
      Ok
        (Form.ElementCheckboxes
           {
             attributes = Result.get_ok attributes;
             validations = Result.get_ok validations;
           })
  | _ -> Error { info with children = errs }

let to_body_element (values, info) =
  let* typ =
    let* x = (values, info) |> must_extract "type" in
    x |> to_string
  in
  let info_with_type =
    { info with message = Some ("This element type: " ^ typ) }
  in
  match typ with
  | "markdown" -> (values, info_with_type) |> to_markdown
  | "textarea" -> (values, info_with_type) |> to_textarea
  | "input" -> (values, info_with_type) |> to_input
  | "dropdown" -> (values, info_with_type) |> to_dropdown
  | "checkboxes" -> (values, info_with_type) |> to_checkboxes
  | x ->
      let err =
        {
          field = "type";
          message = Some ("unexpected type: " ^ x);
          children = [];
        }
      in
      Error { info with children = [ err ] }

let to_body (v, info) =
  let* values = (v, info) |> to_list in
  let elements =
    values
    |> Listutil.enumerate (fun (i, x) ->
           let* xs =
             (x, { field = Int.to_string i; message = None; children = [] })
             |> to_must_values
           in
           xs |> to_body_element)
  in
  let errs = elements |> Listutil.all_errors in
  match errs with
  | [] -> Ok (elements |> List.map Result.get_ok)
  | _ -> Error { info with children = errs }

let parse_info_to_str x =
  let rec aux indent v =
    let lines =
      (if v.field = "" then [] else [ "- " ^ v.field ])
      @
      match v.message with
      | Some x -> [ "  " ^ x ]
      | None -> []
    in
    let spaces = Listutil.repeat " " indent |> String.concat "" in
    let indented = lines |> List.map (fun x -> spaces ^ x) in
    indented @ (v.children |> List.map (aux (indent + 2)) |> List.flatten)
  in
  "The following validation errors occured:\n"
  ^ (aux 0 x |> String.concat "\n")
  ^ "\n"

let convert (v : Yaml.value) =
  let info = { field = "(yaml root)"; message = None; children = [] } in
  let* values = (v, info) |> to_must_values in
  let name =
    let* x = values |> must_extract "name" in
    x |> to_string
  in
  let description =
    let* x = values |> must_extract "description" in
    x |> to_string
  in
  let title = values |> extract "title" |> to_string_option in
  let labels =
    let x = values |> extract "labels" in
    x |> to_string_list
  in
  let assignees =
    let x = values |> extract "assignees" in
    x |> to_string_list
  in
  let body =
    let* x = values |> must_extract "body" in
    x |> to_body
  in
  let errs =
    [
      name |> to_err;
      description |> to_err;
      title |> to_err;
      labels |> to_err;
      assignees |> to_err;
      body |> to_err;
    ]
    |> Listutil.all_somes
  in
  match errs with
  | [] ->
      Ok
        ({
           name = Result.get_ok name;
           description = Result.get_ok description;
           title = Result.get_ok title;
           labels = Result.get_ok labels;
           assignees = Result.get_ok assignees;
           body = Result.get_ok body;
         }
          : Form.t)
  | _ -> Error { info with children = errs }
