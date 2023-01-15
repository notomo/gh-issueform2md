Issueform2md.Input.from_stdin ()
|> Issueform2md.Convert.from_string_to_yaml
|> Issueform2md.Convert.from_yaml_to_issue_form
|> Issueform2md.Markdown.render
|> print_string
