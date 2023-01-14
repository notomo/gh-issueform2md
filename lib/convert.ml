let from_str_to_yaml str = Yaml.of_string str

type element_markdown_attributes = { value : string }
type element_markdown = { attributes : element_markdown_attributes }
type element_validations = { required : bool }

type element_textarea_attributes = {
  label : string;
  description : string option;
  placeholder : string option;
  value : string option;
  render : string option;
  id : string option;
}

type element_textarea = {
  attributes : element_textarea_attributes;
  validations : element_validations;
}

type element_input_attributes = {
  label : string;
  description : string option;
  placeholder : string option;
  value : string option;
  id : string option;
}

type element_input = {
  attributes : element_input_attributes;
  validations : element_validations;
}

type element_dropdown_attributes = {
  label : string;
  description : string option;
  multiple : bool;
  options : string list;
  id : string option;
}

type element_dropdown = {
  attributes : element_dropdown_attributes;
  validations : element_validations;
}

type element_checkboxes_option = { label : string }

type element_checkboxes_attributes = {
  label : string;
  description : string option;
  options : element_checkboxes_option list;
  id : string option;
}

type element_checkboxes = {
  attributes : element_checkboxes_attributes;
  validations : element_validations;
}

type element =
  | Markdown of element_markdown
  | Textarea of element_textarea
  | Input of element_input
  | Dropdown of element_dropdown
  | Checkboxes of element_checkboxes

type issue_form = {
  name : string;
  description : string;
  title : string option;
  labels : string list;
  assignees : string list;
  body : element list;
}

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
              | Some (`String x) -> { label = x }
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
  Markdown
    {
      attributes =
        { value = to_string (extract [ "attributes"; "value" ] values) };
    }

let yaml_value_to_textarea_element values =
  Textarea
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
  Input
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
  Dropdown
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
  Checkboxes
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

let join_form a b =
  {
    name = (if b.name <> "" then b.name else a.name);
    description = (if b.description <> "" then b.description else a.description);
    title = (if b.title <> None then b.title else a.title);
    labels = (if List.length b.labels <> 0 then b.labels else a.labels);
    assignees =
      (if List.length b.assignees <> 0 then b.assignees else a.assignees);
    body = (if List.length b.body <> 0 then b.body else a.body);
  }

let from_yaml_to_issue_form content =
  let add_form_field form (k, v) =
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
      let form =
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

let from_issue_form_to_markdown_format form =
  String.concat ""
    [
      {|---
|};
      "name: ";
      form.name;
      {|
|};
      "about: ";
      form.description;
      {|
|};
      (match form.title with
      | Some x -> "title: " ^ x
      | _ -> "");
      (if List.length form.labels <> 0 then
       "labels: " ^ String.concat ", " form.labels ^ {|
|}
      else "");
      (if List.length form.assignees <> 0 then
       "assignees: " ^ String.concat ", " form.assignees ^ {|
|}
      else "");
      {|
---

|};
      String.concat ""
        (List.map
           (function
             | Markdown x -> x.attributes.value ^ {|
|}
             | Textarea x ->
                 "### " ^ x.attributes.label ^ {|
|}
                 ^ (match x.attributes.description with
                   | Some x -> {|<!--
|} ^ x ^ {|-->
|}
                   | None -> "")
                 ^ (match x.attributes.value with
                   | Some x -> x
                   | None -> "")
                 ^ {|
|}
             | Input x -> "### " ^ x.attributes.label ^ {|: 

|}
             | Dropdown x ->
                 "### " ^ x.attributes.label ^ {|

|}
                 ^ String.concat {|
|}
                     (List.map (fun o -> "- [ ] " ^ o) x.attributes.options)
                 ^ {|

|}
             | Checkboxes x ->
                 "### " ^ x.attributes.label ^ {|

|}
                 ^ String.concat {|
|}
                     (List.map
                        (fun (o : element_checkboxes_option) ->
                          "- [ ] " ^ o.label)
                        x.attributes.options)
                 ^ {|
|})
           form.body);
    ]
