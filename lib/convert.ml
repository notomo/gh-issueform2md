let from_string_to_yaml str =
  let parsed = Yaml.of_string str in
  match parsed with
  | Ok content -> content
  | Error _ -> failwith "parse yaml"

let rec extract keys (values : (string * Yaml.value) list) =
  match (keys, values) with
  | [], (_, v) :: _ -> Some v
  | _, [] -> None
  | key :: rest_keys, (k, v) :: xs ->
      if key = k then
        match v with
        | `O xs -> extract rest_keys xs
        | _ -> Some v
      else extract (key :: rest_keys) xs

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

let to_checkboxes_options = function
  | Some (`A xs) ->
      List.map
        (fun x ->
          match x with
          | `O values -> (
              let label = extract [ "label" ] values in
              match label with
              | Some (`String x) -> ({ label = x } : Form.Checkboxes.option_)
              | Some x ->
                  failwith
                    ("unexpected checkbox option: " ^ Yaml.to_string_exn x)
              | None -> failwith "unexpected checkbox option")
          | _ -> failwith "")
        xs
  | None -> []
  | _ -> failwith "unexpected checkbox options"

let to_bool = function
  | Some (`Bool x) -> x
  | _ -> false

let yaml_value_to_markdown_element values =
  Form.ElementMarkdown
    {
      attributes =
        { value = to_string (extract [ "attributes"; "value" ] values) };
    }

let yaml_value_to_textarea_element values =
  Form.ElementTextarea
    {
      attributes =
        {
          label = to_string (extract [ "attributes"; "label" ] values);
          description =
            to_string_option (extract [ "attributes"; "description" ] values);
          placeholder =
            to_string_option (extract [ "attributes"; "placeholder" ] values);
          value = to_string_option (extract [ "attributes"; "value" ] values);
          render = to_string_option (extract [ "attributes"; "render" ] values);
          id = to_string_option (extract [ "attributes"; "id" ] values);
        };
      validations =
        { required = to_bool (extract [ "validations"; "required" ] values) };
    }

let yaml_value_to_input_element values =
  Form.ElementInput
    {
      attributes =
        {
          label = to_string (extract [ "attributes"; "label" ] values);
          description =
            to_string_option (extract [ "attributes"; "label" ] values);
          placeholder =
            to_string_option (extract [ "attributes"; "placeholder" ] values);
          value = to_string_option (extract [ "attributes"; "value" ] values);
          id = to_string_option (extract [ "attributes"; "id" ] values);
        };
      validations =
        { required = to_bool (extract [ "validations"; "required" ] values) };
    }

let yaml_value_to_dropdown_element values =
  Form.ElementDropdown
    {
      attributes =
        {
          label = to_string (extract [ "attributes"; "label" ] values);
          description =
            to_string_option (extract [ "attributes"; "label" ] values);
          multiple = to_bool (extract [ "attributes"; "multiple" ] values);
          options = to_string_list (extract [ "attributes"; "options" ] values);
          id = to_string_option (extract [ "attributes"; "id" ] values);
        };
      validations =
        { required = to_bool (extract [ "validations"; "required" ] values) };
    }

let yaml_value_to_checkboxes_element values =
  Form.ElementCheckboxes
    {
      attributes =
        {
          label = to_string (extract [ "attributes"; "label" ] values);
          description =
            to_string_option (extract [ "attributes"; "description" ] values);
          options =
            to_checkboxes_options (extract [ "attributes"; "options" ] values);
          id = to_string_option (extract [ "attributes"; "id" ] values);
        };
      validations =
        { required = to_bool (extract [ "validations"; "required" ] values) };
    }

let join_form (a : Form.t) (b : Form.t) =
  let joined : Form.t =
    {
      name = (if b.name <> "" then b.name else a.name);
      description =
        (if b.description <> "" then b.description else a.description);
      title = (if b.title <> None then b.title else a.title);
      labels = (if List.length b.labels <> 0 then b.labels else a.labels);
      assignees =
        (if List.length b.assignees <> 0 then b.assignees else a.assignees);
      body = (if List.length b.body <> 0 then b.body else a.body);
    }
  in
  joined

let from_yaml_to_issue_form content =
  let add_form_field (form : Form.t) (k, v) =
    match (k, v) with
    | "name", `String x -> { form with name = x }
    | "description", `String x -> { form with description = x }
    | "title", `String x -> { form with title = Some x }
    | "labels", `A xs ->
        {
          form with
          labels =
            List.map
              (function
                | `String s -> s
                | _ -> failwith "unexpected label type")
              xs;
        }
    | "assignees", `A xs ->
        {
          form with
          assignees =
            List.map
              (function
                | `String s -> s
                | _ -> failwith "unexpected assignee type")
              xs;
        }
    | "body", `A xs ->
        {
          form with
          body =
            List.map
              (fun x ->
                match x with
                | `O values -> (
                    let typ = extract [ "type" ] values in
                    match typ with
                    | Some (`String "markdown") ->
                        yaml_value_to_markdown_element values
                    | Some (`String "textarea") ->
                        yaml_value_to_textarea_element values
                    | Some (`String "input") ->
                        yaml_value_to_input_element values
                    | Some (`String "dropdown") ->
                        yaml_value_to_dropdown_element values
                    | Some (`String "checkboxes") ->
                        yaml_value_to_checkboxes_element values
                    | _ -> failwith "unexpected element type")
                | _ -> failwith "unexpected")
              xs;
        }
    | _, _ -> failwith "unexpected property"
  in
  let rec fold_forms form = function
    | [] -> form
    | x :: xs -> fold_forms (join_form form x) xs
  in
  match content with
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
      fold_forms form (List.map (add_form_field form) xs)
  | _ -> failwith "invalid form"
