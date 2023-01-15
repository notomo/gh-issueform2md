let render (form : Form.t) =
  String.concat ""
    [
      "---\n";
      "name: ";
      form.name;
      "\n";
      "about: ";
      form.description;
      "\n";
      (match form.title with
      | Some x -> "title: " ^ x
      | _ -> "");
      (if List.length form.labels <> 0 then
       "labels: " ^ String.concat ", " form.labels ^ "\n"
      else "");
      (if List.length form.assignees <> 0 then
       "assignees: " ^ String.concat ", " form.assignees ^ "\n"
      else "");
      "\n---\n\n";
      String.concat ""
        (List.map
           (function
             | Form.ElementMarkdown x -> x.attributes.value ^ "\n"
             | Form.ElementTextarea x ->
                 "### "
                 ^ x.attributes.label
                 ^ "\n"
                 ^ (match x.attributes.description with
                   | Some x -> "<!--\n" ^ x ^ "-->\n"
                   | None -> "")
                 ^ (match x.attributes.value with
                   | Some x -> x
                   | None -> "")
                 ^ "\n"
             | Form.ElementInput x -> "### " ^ x.attributes.label ^ ": \n\n"
             | Form.ElementDropdown x ->
                 "### "
                 ^ x.attributes.label
                 ^ "\n\n"
                 ^ String.concat "\n"
                     (List.map (fun o -> "- [ ] " ^ o) x.attributes.options)
                 ^ "\n\n"
             | Form.ElementCheckboxes x ->
                 "### "
                 ^ x.attributes.label
                 ^ "\n\n"
                 ^ String.concat "\n"
                     (List.map
                        (fun (o : Form.Checkboxes.option_) ->
                          "- [ ] " ^ o.label)
                        x.attributes.options)
                 ^ "\n")
           form.body);
    ]
