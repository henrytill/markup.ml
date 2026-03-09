(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

module Parsing =
struct
  type context_entry =
    {f        : string -> string option;
     previous : context_entry}

  type context = context_entry ref

  let parse qualified_name =
    try
      let colon_index = String.index qualified_name ':' in
      if colon_index = 0 then
        raise Not_found;
      let prefix = String.sub qualified_name 0 colon_index in
      let suffix =
        String.sub qualified_name
          (colon_index + 1)
          (String.length qualified_name - colon_index - 1)
      in
      prefix, suffix

    with Not_found -> ("", qualified_name)

  let init top_level =
    let f = function
      | "xml" -> Some xml_ns
      | "xmlns" -> Some xmlns_ns
      | s -> top_level s
    in
    let rec entry = {f; previous = entry} in
    ref entry

  let expand_element report context raw_element_name =
    let ns, name = parse raw_element_name in
    match !context.f ns with
    | Some uri -> (uri, name)
    | None ->
      match ns with
      | "" -> ("", name)
      | prefix ->
        report () (`Bad_namespace prefix);
        (prefix, name)

  let push report context raw_element_name raw_attributes throw k =
    let parsed_attributes =
      raw_attributes |> List.map (fun (name, value) -> parse name, value) in

    let f =
      parsed_attributes |> List.fold_left (fun f -> function
        | ("xmlns", prefix), uri ->
          (fun p -> if p = prefix then Some uri else f p)
        | ("", "xmlns"), uri ->
          (fun p -> if p = "" then Some uri else f p)
        | _ -> f)
        !context.f
    in

    let entry = {f; previous = !context} in
    context := entry;

    let expanded_element_name = expand_element report context raw_element_name in
    let expanded_attributes =
      parsed_attributes |> List.map begin fun (name, value) ->
        match name with
        | "", "xmlns" -> ((xmlns_ns, "xmlns"), value)
        | "", name -> (("", name), value)
        | ns, name ->
          match f ns with
          | Some uri -> ((uri, name), value)
          | None ->
            report () (`Bad_namespace ns);
            ((ns, name), value)
      end
    in
    ignore throw;
    k (expanded_element_name, expanded_attributes)

  let pop ({contents = {previous}} as context) =
    context := previous
end

module StringMap = Map.Make (String)

module Writing =
struct
  type context_entry =
    {namespace_to_prefix : string list StringMap.t;
     prefix_to_namespace : string StringMap.t;
     previous            : context_entry}

  type context = context_entry ref * (string -> string option)

  let init top_level =
    let namespace_to_prefix =
      StringMap.empty
      |> StringMap.add "" [""]
      |> StringMap.add xml_ns ["xml"]
      |> StringMap.add xmlns_ns ["xmlns"]
    in

    let prefix_to_namespace =
      StringMap.empty
      |> StringMap.add "" ""
      |> StringMap.add "xml" xml_ns
      |> StringMap.add "xmlns" xmlns_ns
    in

    let rec entry =
      {namespace_to_prefix; prefix_to_namespace; previous = entry} in

    ref entry, top_level

  let lookup report allow_default context namespace throw k =
    let candidate_prefixes =
      try StringMap.find namespace !(fst context).namespace_to_prefix
      with Not_found -> []
    in

    let prefix =
      try
        Some (candidate_prefixes |> List.find (fun prefix ->
          (allow_default || prefix <> "") &&
           begin
            try StringMap.find prefix !(fst context).prefix_to_namespace =
              namespace
            with Not_found -> false
           end))
      with Not_found -> None
    in

    let prefix =
      match prefix with
      | Some _ -> prefix
      | None ->
        match snd context namespace with
        | None -> None
        | Some prefix ->
          if not allow_default && prefix = "" ||
              StringMap.mem prefix !(fst context).prefix_to_namespace then
            None
          else Some prefix
    in

    ignore throw;
    match prefix with
    | None -> report () (`Bad_namespace namespace); k ""
    | Some prefix -> k prefix

  let format prefix name =
    match prefix with
    | "" -> name
    | prefix -> prefix ^ ":" ^ name

  let unexpand_element report context (namespace, name) throw k =
    lookup report true context namespace throw (fun prefix ->
    k (format prefix name))

  let unexpand_attribute report context ((namespace, name), value) throw k =
    match namespace with
    | "" -> k (name, value)
    | uri ->
      if uri = xmlns_ns && name = "xmlns" then k ("xmlns", value)
      else
        lookup report false context namespace throw (fun prefix ->
          k (format prefix name, value))

  let extend k v map =
    let vs =
      try StringMap.find k map
      with Not_found -> []
    in
    StringMap.add k (v::vs) map

  let push report context element_name attributes throw k =
    let namespace_to_prefix, prefix_to_namespace =
      attributes |> List.fold_left (fun (ns_to_prefix, prefix_to_ns) -> function
        | (ns, "xmlns"), uri when ns = xmlns_ns ->
          extend uri "" ns_to_prefix,
          StringMap.add "" uri prefix_to_ns
        | (ns, prefix), uri when ns = xmlns_ns ->
          extend uri prefix ns_to_prefix,
          StringMap.add prefix uri prefix_to_ns
        | _ -> ns_to_prefix, prefix_to_ns)
        (!(fst context).namespace_to_prefix, !(fst context).prefix_to_namespace)
    in

    let entry =
      {namespace_to_prefix; prefix_to_namespace; previous = !(fst context)} in
    (fst context) := entry;

    unexpand_element report context element_name throw (fun element_name ->
    let rec map_attrs acc = function
      | [] -> k (element_name, List.rev acc)
      | attr::rest ->
        unexpand_attribute report context attr throw (fun a ->
          map_attrs (a::acc) rest)
    in
    map_attrs [] attributes)

  let pop ({contents = {previous}}, _ as context) =
    (fst context) := previous
end
