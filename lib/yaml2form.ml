let extract = List.assoc_opt
let to_string = Yamlutil.to_string
let to_list = Yamlutil.to_list
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

let to_checkboxes_options values =
  values
  |> to_list
  |> List.map (fun x ->
         Some x |> to_values |> extract "label" |> to_string
         |> fun x : Form.Checkboxes.option_ -> { label = x })

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
  | Some (`String x) -> failwith ("unexpected body[n].type: " ^ x)
  | _ -> failwith "unexpected body[n].type"

let convert (v : Yaml.value) =
  let values = Some v |> to_values in
  ({
     name = values |> extract "name" |> to_string;
     description = values |> extract "description" |> to_string;
     title = values |> extract "title" |> to_string_option;
     labels = values |> extract "labels" |> to_string_list;
     assignees = values |> extract "assignees" |> to_string_list;
     body =
       values
       |> extract "body"
       |> to_list
       |> List.map (fun x -> Some x |> to_values |> to_body_element);
   }
    : Form.t)
