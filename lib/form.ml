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
