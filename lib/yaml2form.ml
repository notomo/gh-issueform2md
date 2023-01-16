let extract = List.assoc_opt
let to_string = Yamlutil.to_string
let to_string_option = Yamlutil.to_string_option
let to_string_list = Yamlutil.to_string_list
let to_bool = Yamlutil.to_bool
let to_values = Yamlutil.to_values

let to_markdown values =
  let attributes = values |> extract "attributes" |> to_values in
  Form.ElementMarkdown
    { attributes = { value = attributes |> extract "value" |> to_string } }

let to_textarea values =
  let attributes = values |> extract "attributes" |> to_values in
  let validations = values |> extract "validations" |> to_values in
  Form.ElementTextarea
    {
      attributes =
        {
          label = attributes |> extract "label" |> to_string;
          description = attributes |> extract "description" |> to_string_option;
          placeholder = attributes |> extract "placeholder" |> to_string_option;
          value = attributes |> extract "value" |> to_string_option;
          render = attributes |> extract "render" |> to_string_option;
          id = attributes |> extract "id" |> to_string_option;
        };
      validations = { required = validations |> extract "required" |> to_bool };
    }

let to_input values =
  let attributes = values |> extract "attributes" |> to_values in
  let validations = values |> extract "validations" |> to_values in
  Form.ElementInput
    {
      attributes =
        {
          label = attributes |> extract "label" |> to_string;
          description = attributes |> extract "description" |> to_string_option;
          placeholder = attributes |> extract "placeholder" |> to_string_option;
          value = attributes |> extract "value" |> to_string_option;
          id = attributes |> extract "id" |> to_string_option;
        };
      validations = { required = validations |> extract "required" |> to_bool };
    }

let to_dropdown values =
  let attributes = values |> extract "attributes" |> to_values in
  let validations = values |> extract "validations" |> to_values in
  Form.ElementDropdown
    {
      attributes =
        {
          label = attributes |> extract "label" |> to_string;
          description = attributes |> extract "description" |> to_string_option;
          multiple = attributes |> extract "multiple" |> to_bool;
          options = attributes |> extract "options" |> to_string_list;
          id = attributes |> extract "id" |> to_string_option;
        };
      validations = { required = validations |> extract "required" |> to_bool };
    }

let to_checkboxes_options = function
  | Some (`A xs) ->
      List.map
        (fun x ->
          match x with
          | `O values -> (
              values |> extract "label" |> function
              | Some (`String x) -> ({ label = x } : Form.Checkboxes.option_)
              | Some x ->
                  failwith
                    ("unexpected checkbox option: " ^ Yaml.to_string_exn x)
              | None -> failwith "unexpected checkbox option")
          | _ -> failwith "")
        xs
  | None -> []
  | _ -> failwith "unexpected checkbox options"

let to_checkboxes values =
  let attributes = values |> extract "attributes" |> to_values in
  let validations = values |> extract "validations" |> to_values in
  Form.ElementCheckboxes
    {
      attributes =
        {
          label = attributes |> extract "label" |> to_string;
          description = attributes |> extract "description" |> to_string_option;
          options = attributes |> extract "options" |> to_checkboxes_options;
          id = attributes |> extract "id" |> to_string_option;
        };
      validations = { required = validations |> extract "required" |> to_bool };
    }

let to_body_element values =
  values |> extract "type" |> function
  | Some (`String "markdown") -> to_markdown values
  | Some (`String "textarea") -> to_textarea values
  | Some (`String "input") -> to_input values
  | Some (`String "dropdown") -> to_dropdown values
  | Some (`String "checkboxes") -> to_checkboxes values
  | _ -> failwith "unexpected element type"

let convert =
  let add_field (form : Form.t) (k, v) =
    match (k, v) with
    | "name", `String x -> { form with name = x }
    | "description", `String x -> { form with description = x }
    | "title", `String x -> { form with title = Some x }
    | "labels", `A xs ->
        { form with labels = xs |> List.map Option.some |> List.map to_string }
    | "assignees", `A xs ->
        {
          form with
          assignees = xs |> List.map Option.some |> List.map to_string;
        }
    | "body", `A xs ->
        {
          form with
          body =
            xs
            |> List.map Option.some
            |> List.map (fun x -> x |> to_values |> to_body_element);
        }
    | _, _ -> failwith "unexpected property"
  in

  function
  | `O xs ->
      let form : Form.t =
        {
          name = "";
          description = "";
          title = None;
          labels = [];
          assignees = [];
          body = [];
        }
      in
      xs |> List.fold_left add_field form
  | _ -> failwith "invalid form"