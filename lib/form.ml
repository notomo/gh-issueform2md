type validations = { required : bool }

module Markdown = struct
  type markdown_attributes = { value : string }
  type t = { attributes : markdown_attributes }
end

module Textarea = struct
  type attributes = {
    label : string;
    description : string option;
    placeholder : string option;
    value : string option;
    render : string option;
    id : string option;
  }

  type t = { attributes : attributes; validations : validations }
end

module Input = struct
  type attributes = {
    label : string;
    description : string option;
    placeholder : string option;
    value : string option;
    id : string option;
  }

  type t = { attributes : attributes; validations : validations }
end

module Dropdown = struct
  type attributes = {
    label : string;
    description : string option;
    multiple : bool;
    options : string list;
    id : string option;
  }

  type t = { attributes : attributes; validations : validations }
end

module Checkboxes = struct
  type option_ = { label : string }

  type attributes = {
    label : string;
    description : string option;
    options : option_ list;
    id : string option;
  }

  type t = { attributes : attributes; validations : validations }
end

type body_element =
  | ElementMarkdown of Markdown.t
  | ElementTextarea of Textarea.t
  | ElementInput of Input.t
  | ElementDropdown of Dropdown.t
  | ElementCheckboxes of Checkboxes.t

type t = {
  name : string;
  description : string;
  title : string option;
  labels : string list;
  assignees : string list;
  body : body_element list;
}
