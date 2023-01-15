open Issueform2md;;

Input.from_stdin ()
|> Str2yaml.convert
|> Yaml2form.convert
|> Form2md.convert
|> print_string
