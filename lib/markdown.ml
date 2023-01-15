open Form

let render form =
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
             | Markdown x -> x.attributes.value ^ "\n"
             | Textarea x ->
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
             | Input x -> "### " ^ x.attributes.label ^ ": \n\n"
             | Dropdown x ->
                 "### "
                 ^ x.attributes.label
                 ^ "\n\n"
                 ^ String.concat "\n"
                     (List.map (fun o -> "- [ ] " ^ o) x.attributes.options)
                 ^ "\n\n"
             | Checkboxes x ->
                 "### "
                 ^ x.attributes.label
                 ^ "\n\n"
                 ^ String.concat "\n"
                     (List.map
                        (fun (o : element_checkboxes_option) ->
                          "- [ ] " ^ o.label)
                        x.attributes.options)
                 ^ "\n")
           form.body);
    ]
