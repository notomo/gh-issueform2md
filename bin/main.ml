let read_stdin_all () =
  let rec aux acc =
    try
      let line = read_line () in
      aux (line :: acc)
    with End_of_file -> acc
  in
  String.concat "\n" (List.rev (aux []))

let str = read_stdin_all ()
let parsed = Issueform2md.Convert.from_str_to_yaml str;;

match parsed with
| Ok content ->
    let issue_form = Issueform2md.Convert.from_yaml_to_issue_form content in
    print_string
      (Issueform2md.Convert.from_issue_form_to_markdown_format issue_form)
| Error _ -> failwith "parse yaml"
