let to_string = function
  | Some x -> x
  | None -> ""

let prefixed prefix = function
  | [] -> ""
  | xs -> prefix ^ String.concat ", " xs ^ "\n"

let comment_out = function
  | Some x -> Some ("<!--\n" ^ x ^ "\n-->\n")
  | None -> None

let to_checkbox str = "- [ ] " ^ str
let h3 str = "### " ^ str

let required_mark = function
  | false -> ""
  | true -> " (\\*)"

let to_example = function
  | Some x -> Some ("example: " ^ x)
  | None -> None

let convert (form : Form.t) =
  [
    "---\n";
    "name: " ^ form.name ^ "\n";
    "about: " ^ form.description ^ "\n";
    prefixed "title: " (form.title |> Option.to_list);
    prefixed "labels: " form.labels;
    prefixed "assignees: " form.assignees;
    "\n---\n\n";
    form.body
    |> List.map (function
         | Form.ElementMarkdown x -> x.attributes.value ^ "\n"
         | Form.ElementTextarea x ->
             h3 x.attributes.label
             ^ required_mark x.validations.required
             ^ "\n\n"
             ^ (x.attributes.description |> comment_out |> to_string)
             ^ (x.attributes.placeholder
               |> to_example
               |> comment_out
               |> to_string)
             ^ (x.attributes.value |> to_string)
             ^ "\n"
         | Form.ElementInput x ->
             h3 x.attributes.label
             ^ required_mark x.validations.required
             ^ ": \n\n"
             ^ (x.attributes.description |> comment_out |> to_string)
             ^ (x.attributes.placeholder
               |> to_example
               |> comment_out
               |> to_string)
             ^ "\n"
         | Form.ElementDropdown x ->
             h3 x.attributes.label
             ^ required_mark x.validations.required
             ^ "\n\n"
             ^ (x.attributes.options
               |> List.map to_checkbox
               |> String.concat "\n")
             ^ "\n\n"
         | Form.ElementCheckboxes x ->
             h3 x.attributes.label
             ^ required_mark x.validations.required
             ^ "\n\n"
             ^ (x.attributes.options
               |> List.map (fun (o : Form.Checkboxes.option_) ->
                      o.label |> to_checkbox)
               |> String.concat "\n")
             ^ "\n\n")
    |> String.concat "";
  ]
  |> String.concat ""
