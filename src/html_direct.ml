(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Direct-style fused HTML tokenizer.  This is the first half of a future
   direct-style fused HTML tokenizer+parser (the parser modes will be added
   later).  For now, process_token is a stub that delegates to i.mode.

   Many functions in this module are used via stored closures (i.tok_fn) or
   are exposed for future use by the parser modes.  Suppress unused-value and
   unused-open warnings for the whole module. *)
[@@@warning "-32-33"]

open Common
open Token_tag

(* ---- Doctype buffer helpers (copied from html_tokenizer.ml) ---- *)

let add_doctype_char buffer c =
  let buffer =
    match buffer with
    | None -> Buffer.create 32
    | Some buffer -> buffer
  in
  add_utf_8 buffer c;
  Some buffer

let sequence_to_lowercase = List.map (fun (l, c) -> l, to_lowercase c)

(* Local mutable doctype buffer (mirrors html_tokenizer.ml's Doctype_buffers) *)
type doctype_buffers = {
  mutable doctype_name      : Buffer.t option;
  mutable public_identifier : Buffer.t option;
  mutable system_identifier : Buffer.t option;
  mutable force_quirks      : bool;
}

(* ---- Windows-1252 and entity trie (copied from html_tokenizer.ml) ---- *)

let replace_windows_1252_entity = function
  | 0x80 -> 0x20AC
  | 0x82 -> 0x201A
  | 0x83 -> 0x0192
  | 0x84 -> 0x201E
  | 0x85 -> 0x2026
  | 0x86 -> 0x2020
  | 0x87 -> 0x2021
  | 0x88 -> 0x02C6
  | 0x89 -> 0x2030
  | 0x8A -> 0x0160
  | 0x8B -> 0x2039
  | 0x8C -> 0x0152
  | 0x8E -> 0x017D
  | 0x91 -> 0x2018
  | 0x92 -> 0x2019
  | 0x93 -> 0x201C
  | 0x94 -> 0x201D
  | 0x95 -> 0x2022
  | 0x96 -> 0x2013
  | 0x97 -> 0x2014
  | 0x98 -> 0x02DC
  | 0x99 -> 0x2122
  | 0x9A -> 0x0161
  | 0x9B -> 0x203A
  | 0x9C -> 0x0153
  | 0x9E -> 0x017E
  | 0x9F -> 0x0178
  | c -> c

let named_entity_trie =
  lazy begin
    let trie = Trie.create () in
    Array.fold_left (fun trie (name, characters) ->
      Trie.add name characters trie)
      trie
      Entities.entities
  end

(* ---- Parser types (shared with html_parser.ml style) ---- *)

type ns = [ `HTML | `MathML | `SVG | `Other of string ]
type qname = ns * string

module Ns = struct
  let to_string = function
    | `HTML -> Common.html_ns
    | `MathML -> Common.mathml_ns
    | `SVG -> Common.svg_ns
    | `Other s -> s
end

let list_mem_qname ((ns, tag) : qname) l =
  let rec loop = function
    | [] -> false
    | (ns', tag')::_ when ns' = ns && tag' = tag -> true
    | _::rest -> loop rest
  in
  loop l

type element =
  {element_name              : qname;
   location                  : location;
   is_html_integration_point : bool;
   suppress                  : bool;
   mutable buffering         : bool;
   mutable is_open           : bool;
   mutable attributes        : (Common.name * string) list;
   mutable end_location      : location;
   mutable children          : annotated_node list;
   mutable parent            : element}

and node =
  | Element of element
  | Text of string list
  | PI of string * string
  | Comment of string

and annotated_node = location * node

module Element = struct
  let rec dummy =
    {element_name              = `HTML, "dummy";
     location                  = 1, 1;
     is_html_integration_point = false;
     suppress                  = true;
     buffering                 = false;
     is_open                   = false;
     attributes                = [];
     end_location              = 1, 1;
     children                  = [];
     parent                    = dummy}

  let create ?(is_html_integration_point = false) ?(suppress = false) name location =
    {element_name = name;
     location;
     is_html_integration_point;
     suppress;
     buffering    = false;
     is_open      = true;
     attributes   = [];
     end_location = 1, 1;
     children     = [];
     parent       = dummy}

  let is_special name =
    list_mem_qname name
      [`HTML, "address"; `HTML, "applet"; `HTML, "area";
       `HTML, "article"; `HTML, "aside"; `HTML, "base";
       `HTML, "basefont"; `HTML, "bgsound"; `HTML, "blockquote";
       `HTML, "body"; `HTML, "br"; `HTML, "button";
       `HTML, "caption"; `HTML, "center"; `HTML, "col";
       `HTML, "colgroup"; `HTML, "dd"; `HTML, "details";
       `HTML, "dir"; `HTML, "div"; `HTML, "dl";
       `HTML, "dt"; `HTML, "embed"; `HTML, "fieldset";
       `HTML, "figcaption"; `HTML, "figure"; `HTML, "footer";
       `HTML, "form"; `HTML, "frame"; `HTML, "frameset";
       `HTML, "h1"; `HTML, "h2"; `HTML, "h3";
       `HTML, "h4"; `HTML, "h5"; `HTML, "h6";
       `HTML, "head"; `HTML, "header"; `HTML, "hgroup";
       `HTML, "hr"; `HTML, "html"; `HTML, "iframe";
       `HTML, "img"; `HTML, "input"; `HTML, "isindex";
       `HTML, "li"; `HTML, "link"; `HTML, "listing";
       `HTML, "main"; `HTML, "marquee"; `HTML, "meta";
       `HTML, "nav"; `HTML, "noembed"; `HTML, "noframes";
       `HTML, "noscript"; `HTML, "object"; `HTML, "ol";
       `HTML, "p"; `HTML, "param"; `HTML, "plaintext";
       `HTML, "pre"; `HTML, "script"; `HTML, "section";
       `HTML, "select"; `HTML, "source"; `HTML, "style";
       `HTML, "summary"; `HTML, "table"; `HTML, "tbody";
       `HTML, "td"; `HTML, "template"; `HTML, "textarea";
       `HTML, "tfoot"; `HTML, "th"; `HTML, "thead";
       `HTML, "title"; `HTML, "tr"; `HTML, "track";
       `HTML, "ul"; `HTML, "wbr"; `HTML, "xmp";
       `MathML, "mi"; `MathML, "mo"; `MathML, "mn";
       `MathML, "ms"; `MathML, "mtext"; `MathML, "annotation-xml";
       `SVG, "foreignObject"; `SVG, "desc"; `SVG, "title"]

  let is_not_hidden tag =
    tag.Token_tag.attributes |> List.exists (fun (name, value) ->
      name = "type" && value <> "hidden")
end

type context = [ `Document | `Fragment of qname ]

module Stack = struct
  type t = element list ref

  let create () = ref []

  let current_element open_elements =
    match !open_elements with
    | [] -> None
    | element::_ -> Some element

  let require_current_element open_elements =
    match current_element open_elements with
    | None -> failwith "require_current_element: None"
    | Some element -> element

  let current_element_is open_elements names =
    match !open_elements with
    | {element_name = `HTML, name}::_ -> list_mem_string name names
    | _ -> false

  let adjusted_current_element the_context open_elements =
    match !open_elements with
    | [e] ->
      (match the_context with
      | `Fragment name -> Some {e with element_name = name}
      | `Document -> Some e)
    | [] -> None
    | element::_ -> Some element

  let current_element_is_foreign the_context open_elements =
    match adjusted_current_element the_context open_elements with
    | Some {element_name = ns, _} when ns <> `HTML -> true
    | _ -> false

  let has open_elements name =
    List.exists
      (fun {element_name = ns, name'} -> ns = `HTML && name' = name)
      !open_elements

  let in_scope_general scope_delimiters open_elements name' =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name'' as name}::more ->
        if ns = `HTML && name'' = name' then true
        else if list_mem_qname name scope_delimiters then false
        else scan more
    in
    scan !open_elements

  let scope_delimiters =
    [`HTML, "applet"; `HTML, "caption"; `HTML, "html";
     `HTML, "table"; `HTML, "td"; `HTML, "th";
     `HTML, "marquee"; `HTML, "object"; `HTML, "template";
     `MathML, "mi"; `MathML, "mo"; `MathML, "mn";
     `MathML, "ms"; `MathML, "mtext"; `MathML, "annotation-xml";
     `SVG, "foreignObject"; `SVG, "desc"; `SVG, "title"]

  let in_scope = in_scope_general scope_delimiters
  let in_button_scope = in_scope_general ((`HTML, "button")::scope_delimiters)
  let in_list_item_scope =
    in_scope_general ((`HTML, "ol")::(`HTML, "ul")::scope_delimiters)
  let in_table_scope =
    in_scope_general [`HTML, "html"; `HTML, "table"; `HTML, "template"]

  let in_select_scope open_elements name =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name'}::more ->
        if ns <> `HTML then false
        else if name' = name then true
        else if name' = "optgroup" || name' = "option" then scan more
        else false
    in
    scan !open_elements

  let one_in_scope open_elements names =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name' as name}::more ->
        if ns = `HTML && list_mem_string name' names then true
        else if list_mem_qname name scope_delimiters then false
        else scan more
    in
    scan !open_elements

  let one_in_table_scope open_elements names =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name' as name}::more ->
        if ns = `HTML && list_mem_string name' names then true
        else if list_mem_qname name
            [`HTML, "html"; `HTML, "table"; `HTML, "template"] then false
        else scan more
    in
    scan !open_elements

  let target_in_scope open_elements node =
    let rec scan = function
      | [] -> false
      | e::more ->
        if e == node then true
        else if list_mem_qname node.element_name scope_delimiters then false
        else scan more
    in
    scan !open_elements

  let remove open_elements element =
    open_elements := List.filter ((!=) element) !open_elements;
    element.is_open <- false

  let replace open_elements ~old ~new_ =
    open_elements :=
      List.map (fun e ->
        if e == old then (e.is_open <- false; new_) else e) !open_elements

  let insert_below open_elements ~anchor ~new_ =
    let rec insert prefix = function
      | [] -> List.rev prefix
      | e::more when e == anchor -> (List.rev prefix) @ (new_::e::more)
      | e::more -> insert (e::prefix) more
    in
    open_elements := insert [] !open_elements
end

module Active = struct
  type entry =
    | Marker
    | Element_ of element * location * Token_tag.t

  type t = entry list ref

  let create () = ref []

  let add_marker active_formatting_elements =
    active_formatting_elements := Marker :: !active_formatting_elements

  let clear_until_marker active_formatting_elements =
    let rec iterate = function
      | Marker::rest -> rest
      | (Element_ _)::rest -> iterate rest
      | [] -> []
    in
    active_formatting_elements := iterate !active_formatting_elements

  let has active_formatting_elements element =
    !active_formatting_elements |> List.exists (function
      | Element_ (e, _, _) when e == element -> true
      | _ -> false)

  let remove active_formatting_elements element =
    active_formatting_elements :=
      !active_formatting_elements |> List.filter (function
        | Element_ (e, _, _) when e == element -> false
        | _ -> true)

  let replace active_formatting_elements ~old ~new_ =
    active_formatting_elements :=
      !active_formatting_elements |> List.map (function
        | Element_ (e, l, t) when e == old -> Element_ (new_, l, t)
        | e -> e)

  let insert_after active_formatting_elements ~anchor ~new_ =
    let rec insert prefix = function
      | [] -> List.rev prefix
      | (Element_ (e, l, t) as v)::more when e == anchor ->
        let new_entry = Element_ (new_, l, t) in
        (List.rev prefix) @ (v::new_entry::more)
      | v::more -> insert (v::prefix) more
    in
    active_formatting_elements := insert [] !active_formatting_elements

  let has_before_marker active_formatting_elements name =
    let rec scan = function
      | [] | Marker::_ -> None
      | Element_ (n, _, _)::_ when n.element_name = (`HTML, name) -> Some n
      | _::more -> scan more
    in
    scan !active_formatting_elements
end

module Subtree = struct
  type t =
    {open_elements    : Stack.t;
     mutable enabled  : bool;
     mutable position : element}

  let create open_elements =
    {open_elements;
     enabled  = false;
     position = Element.dummy}

  let accumulate subtree_buffer l s =
    if not subtree_buffer.enabled then true
    else begin
      begin match s with
      | `Start_element (_, attributes) ->
        let parent = subtree_buffer.position in
        let child = Stack.require_current_element subtree_buffer.open_elements in
        child.attributes <- attributes;
        child.parent <- parent;
        parent.children <- (l, Element child) :: parent.children;
        subtree_buffer.position <- child

      | `End_element ->
        subtree_buffer.position.end_location <- l;
        subtree_buffer.position <-
          Stack.require_current_element subtree_buffer.open_elements

      | `Text ss ->
        subtree_buffer.position.children <-
          (l, Text ss) :: subtree_buffer.position.children

      | `PI (t, s) ->
        subtree_buffer.position.children <-
          (l, PI (t, s)) :: subtree_buffer.position.children

      | `Comment s ->
        subtree_buffer.position.children <-
          (l, Comment s) :: subtree_buffer.position.children

      | `Xml _ | `Doctype _ -> ()
      end;
      false
    end

  let enable subtree_buffer =
    if subtree_buffer.enabled then ()
    else
      match Stack.current_element subtree_buffer.open_elements with
      | None -> ()
      | Some element ->
        element.buffering <- true;
        subtree_buffer.position <- element;
        subtree_buffer.enabled <- true

  let disable subtree_buffer =
    let rec traverse acc = function
      | l, Element {element_name; attributes; end_location; children} ->
        let name = Ns.to_string (fst element_name), snd element_name in
        let start_signal = l, `Start_element (name, attributes) in
        let end_signal = end_location, `End_element in
        start_signal :: (List.fold_left traverse (end_signal :: acc) children)
      | l, Text ss ->
        begin match acc with
        | (_, `Text ss')::rest -> (l, `Text (ss @ ss'))::rest
        | _ -> (l, `Text ss)::acc
        end
      | l, PI (t, s) -> (l, `PI (t, s))::acc
      | l, Comment s -> (l, `Comment s)::acc
    in
    let result =
      List.fold_left traverse []
        (Stack.require_current_element subtree_buffer.open_elements).children
    in
    subtree_buffer.enabled <- false;
    result

  let adoption_agency_algorithm subtree_buffer active_formatting_elements l subject =
    let open_elements = subtree_buffer.open_elements in

    let above_removed_nodes = ref [] in

    let rec above_in_stack node = function
      | e::e'::_ when e == node -> e'
      | _::more -> above_in_stack node more
      | [] -> failwith "above_in_stack: not found"
    in

    let above_node node =
      if node.is_open then above_in_stack node !open_elements
      else
        try List.find (fun (e, _) -> e == node) !above_removed_nodes |> snd
        with Not_found -> failwith "above_node: not found"
    in

    let remove_node node =
      above_removed_nodes :=
        (node, above_in_stack node !open_elements) :: !above_removed_nodes;
      Stack.remove open_elements node
    in

    let reparent node new_parent =
      let old_parent = node.parent in
      let entry, filtered_children =
        let rec remove prefix = function
          | (_, Element e as entry)::rest when e == node ->
            entry, (List.rev prefix) @ rest
          | e::rest -> remove (e::prefix) rest
          | [] -> (node.location, Element node), old_parent.children
        in
        remove [] old_parent.children
      in
      old_parent.children <- filtered_children;
      new_parent.children <- entry :: new_parent.children;
      node.parent <- new_parent
    in

    let inner_loop formatting_element furthest_block =
      let rec repeat inner_loop_counter node last_node bookmark =
        let node = above_node node in
        if node == formatting_element then last_node, bookmark
        else begin
          if inner_loop_counter > 3 then
            Active.remove active_formatting_elements node;
          if not @@ Active.has active_formatting_elements node then begin
            remove_node node;
            repeat (inner_loop_counter + 1) node last_node bookmark
          end else begin
            let new_node =
              {node with is_open = true; children = []; parent = Element.dummy}
            in
            node.end_location <- l;
            Stack.replace open_elements ~old:node ~new_:new_node;
            Active.replace active_formatting_elements ~old:node ~new_:new_node;
            reparent last_node new_node;
            repeat (inner_loop_counter + 1) new_node new_node
              (if last_node == furthest_block then Some new_node else bookmark)
          end
        end
      in
      repeat 1 furthest_block furthest_block None
    in

    let find_formatting_element () =
      let rec scan = function
        | [] -> None
        | Active.Marker::_ -> None
        | (Active.Element_ ({element_name = `HTML, n} as e, _, _))::_
            when n = subject -> Some e
        | _::rest -> scan rest
      in
      scan !active_formatting_elements
    in

    let find_furthest_block formatting_element =
      let rec scan furthest = function
        | [] -> furthest
        | e::_ when e == formatting_element -> furthest
        | e::more when Element.is_special e.element_name -> scan (Some e) more
        | _::more -> scan furthest more
      in
      scan None !open_elements
    in

    let pop_to_formatting_element formatting_element =
      let rec pop () =
        match !open_elements with
        | [] -> ()
        | e::more ->
          open_elements := more;
          e.is_open <- false;
          e.end_location <- l;
          if e != formatting_element then pop ()
      in
      pop ();
      subtree_buffer.position <- Stack.require_current_element open_elements
    in

    let rec outer_loop outer_loop_counter errors =
      let outer_loop_counter = outer_loop_counter + 1 in
      if outer_loop_counter >= 8 then true, List.rev errors
      else begin
        match find_formatting_element () with
        | None -> false, List.rev errors
        | Some formatting_element ->
          if not formatting_element.is_open then begin
            Active.remove active_formatting_elements formatting_element;
            true, List.rev ((l, `Unmatched_end_tag subject)::errors)
          end else begin
            if not @@ Stack.target_in_scope open_elements formatting_element then begin
              true, List.rev ((l, `Unmatched_end_tag subject)::errors)
            end else begin
              let errors =
                if Stack.require_current_element open_elements ==
                   formatting_element then errors
                else (l, `Unmatched_end_tag subject)::errors
              in
              match find_furthest_block formatting_element with
              | None ->
                pop_to_formatting_element formatting_element;
                Active.remove active_formatting_elements formatting_element;
                true, List.rev errors
              | Some furthest_block ->
                formatting_element.end_location <- l;
                let common_ancestor =
                  above_in_stack formatting_element !open_elements in
                let last_node, bookmark =
                  inner_loop formatting_element furthest_block in
                reparent last_node common_ancestor;
                let new_node =
                  {formatting_element with
                    is_open = true; children = []; parent = Element.dummy}
                in
                new_node.children <- furthest_block.children;
                furthest_block.children <- [];
                new_node.children |> List.iter (function
                  | _, Element child -> child.parent <- new_node
                  | _ -> ());
                reparent new_node furthest_block;
                begin match bookmark with
                | None ->
                  Active.replace active_formatting_elements
                    ~old:formatting_element ~new_:new_node
                | Some node ->
                  Active.remove active_formatting_elements formatting_element;
                  Active.insert_after
                    active_formatting_elements ~anchor:node ~new_:new_node
                end;
                Stack.remove open_elements formatting_element;
                Stack.insert_below
                  open_elements ~anchor:furthest_block ~new_:new_node;
                outer_loop outer_loop_counter errors
            end
          end
      end
    in

    let current_node = Stack.require_current_element open_elements in
    if current_node.element_name = (`HTML, subject) then begin
      open_elements := List.tl !open_elements;
      current_node.is_open <- false;
      current_node.end_location <- l;
      subtree_buffer.position <- Stack.require_current_element open_elements;
      Active.remove active_formatting_elements current_node;
      true, []
    end else outer_loop 0 []
end

module Foreign = struct
  let is_mathml_text_integration_point qname =
    list_mem_qname qname
      [`MathML, "mi"; `MathML, "mo"; `MathML, "mn"; `MathML, "ms";
       `MathML, "mtext"]

  let is_html_integration_point namespace tag_name attributes =
    match namespace with
    | `HTML | `Other _ -> false
    | `MathML ->
      tag_name = "annotation-xml" &&
      attributes |> List.exists (function
        | "encoding", "text/html" -> true
        | "encoding", "application/xhtml+xml" -> true
        | _ -> false)
    | `SVG ->
      list_mem_string tag_name ["foreignObject"; "desc"; "title"]

  let adjust_mathml_attributes attributes =
    attributes |> List.map (fun ((ns, name), value) ->
      let name =
        if ns = Common.mathml_ns && name = "definitionurl" then "definitionURL"
        else name
      in
      (ns, name), value)

  let adjust_svg_attributes attributes =
    attributes |> List.map (fun ((ns, name), value) ->
      let name =
        match name with
        | "attributename" -> "attributeName"
        | "attributetype" -> "attributeType"
        | "basefrequency" -> "baseFrequency"
        | "baseprofile" -> "baseProfile"
        | "calcmode" -> "calcMode"
        | "clippathunits" -> "clipPathUnits"
        | "contentscripttype" -> "contentScriptType"
        | "contentstyletype" -> "contentStyleType"
        | "diffuseconstant" -> "diffuseConstant"
        | "edgemode" -> "edgeMode"
        | "externalresourcesrequired" -> "externalResourcesRequired"
        | "filterres" -> "filterRes"
        | "filterunits" -> "filterUnits"
        | "glyphref" -> "glyphRef"
        | "gradienttransform" -> "gradientTransform"
        | "gradientunits" -> "gradientUnits"
        | "kernelmatrix" -> "kernelMatrix"
        | "kernelunitlength" -> "kernelUnitLength"
        | "keypoints" -> "keyPoints"
        | "keysplines" -> "keySplines"
        | "keytimes" -> "keyTimes"
        | "lengthadjust" -> "lengthAdjust"
        | "limitingconeangle" -> "limitingConeAngle"
        | "markerheight" -> "markerHeight"
        | "markerunits" -> "markerUnits"
        | "markerwidth" -> "markerWidth"
        | "maskcontentunits" -> "maskContentUnits"
        | "maskunits" -> "maskUnits"
        | "numoctaves" -> "numOctaves"
        | "pathlength" -> "pathLength"
        | "patterncontentunits" -> "patternContentUnits"
        | "patterntransform" -> "patternTransform"
        | "patternunits" -> "patternUnits"
        | "pointsatx" -> "pointsAtX"
        | "pointsaty" -> "pointsAtY"
        | "pointsatz" -> "pointsAtZ"
        | "preservealpha" -> "preserveAlpha"
        | "preserveaspectratio" -> "preserveAspectRatio"
        | "primitiveunits" -> "primitiveUnits"
        | "refx" -> "refX"
        | "refy" -> "refY"
        | "repeatcount" -> "repeatCount"
        | "repeatdur" -> "repeatDur"
        | "requiredextensions" -> "requiredExtensions"
        | "requiredfeatures" -> "requiredFeatures"
        | "specularconstant" -> "specularConstant"
        | "specularexponent" -> "specularExponent"
        | "spreadmethod" -> "spreadMethod"
        | "startoffset" -> "startOffset"
        | "stddeviation" -> "stdDeviation"
        | "stitchtiles" -> "stitchTiles"
        | "surfacescale" -> "surfaceScale"
        | "systemlanguage" -> "systemLanguage"
        | "tablevalues" -> "tableValues"
        | "targetx" -> "targetX"
        | "targety" -> "targetY"
        | "textlength" -> "textLength"
        | "viewbox" -> "viewBox"
        | "viewtarget" -> "viewTarget"
        | "xchannelselector" -> "xChannelSelector"
        | "ychannelselector" -> "yChannelSelector"
        | "zoomandpan" -> "zoomAndPan"
        | _ -> name
      in
      (ns, name), value)

  let adjust_svg_tag_name = function
    | "altglyph" -> "altGlyph"
    | "altglyphdef" -> "altGlyphDef"
    | "altglyphitem" -> "altGlyphItem"
    | "animatecolor" -> "animateColor"
    | "animatemotion" -> "animateMotion"
    | "animatetransform" -> "animateTransform"
    | "clippath" -> "clipPath"
    | "feblend" -> "feBlend"
    | "fecolormatrix" -> "feColorMatrix"
    | "fecomponenttransfer" -> "feComponentTransfer"
    | "fecomposite" -> "feComposite"
    | "feconvolvematrix" -> "feConvolveMatrix"
    | "fediffuselighting" -> "feDiffuseLighting"
    | "fedisplacementmap" -> "feDisplacementMap"
    | "fedistantlight" -> "feDistantLight"
    | "fedropshadow" -> "feDropShadow"
    | "feflood" -> "feFlood"
    | "fefunca" -> "feFuncA"
    | "fefuncb" -> "feFuncB"
    | "fefuncg" -> "feFuncG"
    | "fefuncr" -> "feFuncR"
    | "fegaussianblur" -> "feGaussianBlur"
    | "feimage" -> "feImage"
    | "femerge" -> "feMerge"
    | "femergenode" -> "feMergeNode"
    | "femorphology" -> "feMorphology"
    | "feoffset" -> "feOffset"
    | "fepointlight" -> "fePointLight"
    | "fespecularlighting" -> "feSpecularLighting"
    | "fespotlight" -> "feSpotLight"
    | "fetile" -> "feTile"
    | "feturbulence" -> "feTurbulence"
    | "foreignobject" -> "foreignObject"
    | "glyphref" -> "glyphRef"
    | "lineargradient" -> "linearGradient"
    | "radialgradient" -> "radialGradient"
    | "textpath" -> "textPath"
    | s -> s
end

(* ---- html_input record ---- *)

type html_input = {
  (* character layer *)
  decoder          : unit -> int;
  mutable c        : int;
  mutable cr       : bool;
  mutable line     : int;
  mutable col      : int;
  mutable first_char : bool;
  report           : location -> Error.t -> unit;
  mutable pushback : int list;
  (* tokenizer state *)
  mutable tok_fn   : html_input -> unit;
  mutable foreign  : unit -> bool;
  mutable last_start_tag : string option;
  (* tag building (reused per tag) *)
  tag_name_buf     : Buffer.t;
  attr_name_buf    : Buffer.t;
  attr_val_buf     : Buffer.t;
  mutable is_start_tag     : bool;
  mutable self_closing_tag : bool;
  mutable pending_attrs    : (string * string) list;
  (* parser state *)
  mutable current_mode : html_input -> location -> Html_tokenizer.token -> unit;
  the_context      : context;
  open_elements    : Stack.t;
  active_formatting_elements : Active.t;
  subtree_buffer   : Subtree.t;
  template_insertion_modes :
    (html_input -> location -> Html_tokenizer.token -> unit) list ref;
  frameset_ok      : bool ref;
  head_seen        : bool ref;
  form_element_pointer : element option ref;
  (* table text accumulation (for in_table_text_mode) *)
  mutable table_text_only_space : bool;
  mutable table_text_chars : (location * Html_tokenizer.token) list;
  mutable table_text_return_mode :
    html_input -> location -> Html_tokenizer.token -> unit;
  (* signal output *)
  mutable prepend  : (location * signal) list;
  queue            : (location * signal) Queue.t;
  mutable done_    : bool;
  (* text accumulation *)
  text             : Text.t;
  (* kept for tokenizer: delegates to current_mode *)
  mutable mode     : html_input -> location -> Html_tokenizer.token -> unit;
}

(* ---- Character layer ---- *)

let loc i = (i.line, i.col)

let rec nextc i =
  (* Update position for the *current* char before advancing.
     When c = -2 (sentinel) neither branch fires, position stays (1,1). *)
  if i.c = 0x0A then (i.line <- i.line + 1; i.col <- 1)
  else if i.c >= 0 then i.col <- i.col + 1;
  let raw = match i.pushback with
    | x :: rest -> i.pushback <- rest; x
    | []        -> i.decoder ()
  in
  (* Skip BOM at very start *)
  if raw = 0xFEFF && i.first_char then (i.first_char <- false; nextc i)
  else begin
    i.first_char <- false;
    if raw = 0x0D then (i.cr <- true; i.c <- 0x0A)
    else if raw = 0x0A && i.cr then (i.cr <- false; nextc i)
    else (i.cr <- false; i.c <- raw)
  end

(* Push char c back so it will be the next char read.
   i.c is put into the pushback list and c becomes the new lookahead. *)
let pushback_char i c =
  i.pushback <- i.c :: i.pushback;
  i.c <- c

(* Push a list of (location * int) pairs back (discards locations).
   The list should be in oldest-first order; they will be re-read
   in that order. *)
let pushback_lchars i pairs =
  (* pairs = [(l1,c1);(l2,c2);...] to be read in order c1 c2 ...
     We push them all back: current i.c goes onto pushback, then we
     prepend [c1;c2;...;cn] with i.c = c1. *)
  match pairs with
  | [] -> ()
  | (_, first) :: rest ->
    (* push rest and current i.c back *)
    let tail = List.rev_map snd rest in
    i.pushback <- (List.rev tail) @ (i.c :: i.pushback);
    i.c <- first

(* ---- Text and signal helpers ---- *)

let flush_text i =
  match Text.emit i.text with
  | None -> ()
  | Some (l, strings) ->
    if Subtree.accumulate i.subtree_buffer l (`Text strings) then
      Queue.push (l, `Text strings) i.queue

(* Emit a signal, respecting the subtree buffer *)
let emit_signal i l s =
  flush_text i;
  if Subtree.accumulate i.subtree_buffer l s then
    Queue.push (l, s) i.queue

let emit_done i =
  flush_text i;
  i.done_ <- true

(* ---- Tag building helpers ---- *)

let finish_current_attr i =
  if Buffer.length i.attr_name_buf > 0 then begin
    let name = Buffer.contents i.attr_name_buf in
    let value = Buffer.contents i.attr_val_buf in
    Buffer.clear i.attr_name_buf;
    Buffer.clear i.attr_val_buf;
    i.pending_attrs <- (name, value) :: i.pending_attrs
  end

(* All of the following are mutually recursive: emit_tag_token, process_token,
   is_appropriate_end_tag, and all the tokenizer state functions. *)
let rec emit_tag_token i l =
  (* Deduplicate attributes (keeping first occurrence, as HTML spec requires) *)
  let attrs_rev = List.rev i.pending_attrs in
  let rec rev_dedup acc seen = function
    | [] -> acc
    | (n, v) :: more ->
      if list_mem_string n seen then begin
        i.report l (`Bad_token (n, "tag", "duplicate attribute"));
        rev_dedup acc seen more
      end else
        rev_dedup ((n, v) :: acc) (n :: seen) more
  in
  let attributes = List.rev (rev_dedup [] [] attrs_rev) in

  let tag =
    {Token_tag.name = Buffer.contents i.tag_name_buf;
     self_closing    = i.self_closing_tag;
     attributes}
  in
  Buffer.clear i.tag_name_buf;
  i.pending_attrs <- [];
  i.self_closing_tag <- false;

  if i.is_start_tag then begin
    i.last_start_tag <- Some tag.Token_tag.name;
    i.tok_fn <- data_state;
    i.mode i l (`Start tag)
  end else begin
    (match attributes with
    | (n, _) :: _ ->
      i.report l (`Bad_token (n, "tag", "end tag with attributes"))
    | [] -> ());
    if tag.Token_tag.self_closing then
      i.report l (`Bad_token ("/>", "tag", "end tag cannot be self-closing"));
    i.tok_fn <- data_state;
    i.mode i l (`End tag)
  end

(* ---- process_token: dispatches via i.mode, which handles foreign check ---- *)
(* i.mode is set in make() to a function that does the foreign content check
   and then dispatches to i.current_mode or foreign_content. *)
and process_token i l (token : Html_tokenizer.token) =
  i.mode i l token

(* ---- Helper: is_appropriate_end_tag ---- *)

and is_appropriate_end_tag i =
  match i.last_start_tag with
  | None -> false
  | Some name -> Buffer.contents i.tag_name_buf = name

(* ============================================================
   Tokenizer state functions (converted from html_tokenizer.ml)
   ============================================================ *)

(* 8.2.4.1. Data state *)
and data_state i =
  match i.c with
  | 0x0026 ->
    let l = loc i in
    nextc i;
    character_reference_state data_state i l
  | 0x003C ->
    let l = loc i in
    nextc i;
    tag_open_state i l
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "content", "null"));
    nextc i;
    i.tok_fn <- data_state;
    process_token i l (`Char 0)
  | -1 ->
    let l = loc i in
    process_token i l `EOF
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- data_state;
    process_token i l (`Char c)

(* 8.2.4.2 / 8.2.4.4  Character reference state *)
and character_reference_state next_state i l =
  let result = consume_character_reference i ~in_attribute:false ~additional:None l in
  (match result with
  | None ->
    i.tok_fn <- next_state;
    process_token i l (`Char 0x0026)
  | Some (`One c) ->
    i.tok_fn <- next_state;
    process_token i l (`Char c)
  | Some (`Two (c, c')) ->
    process_token i l (`Char c);
    i.tok_fn <- next_state;
    process_token i l (`Char c'));
  next_state i

(* 8.2.4.3  RCDATA state *)
and rcdata_state i =
  match i.c with
  | 0x0026 ->
    let l = loc i in
    nextc i;
    character_reference_state rcdata_state i l
  | 0x003C ->
    let l = loc i in
    nextc i;
    text_less_than_sign_state rcdata_state i l [(l, 0x003C)]
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "content", "null"));
    nextc i;
    i.tok_fn <- rcdata_state;
    process_token i l (`Char u_rep)
  | -1 ->
    let l = loc i in
    process_token i l `EOF
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- rcdata_state;
    process_token i l (`Char c)

(* 8.2.4.5  RAWTEXT state *)
and rawtext_state i =
  match i.c with
  | 0x003C ->
    let l = loc i in
    nextc i;
    text_less_than_sign_state rawtext_state i l [(l, 0x003C)]
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "content", "null"));
    nextc i;
    i.tok_fn <- rawtext_state;
    process_token i l (`Char u_rep)
  | -1 ->
    let l = loc i in
    process_token i l `EOF
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- rawtext_state;
    process_token i l (`Char c)

(* 8.2.4.6  Script data state *)
and script_data_state i =
  match i.c with
  | 0x003C ->
    let l = loc i in
    nextc i;
    script_data_less_than_sign_state i l [(l, 0x003C)]
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "content", "null"));
    nextc i;
    i.tok_fn <- script_data_state;
    process_token i l (`Char u_rep)
  | -1 ->
    let l = loc i in
    process_token i l `EOF
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- script_data_state;
    process_token i l (`Char c)

(* 8.2.4.7  PLAINTEXT state *)
and plaintext_state i =
  match i.c with
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "content", "null"));
    nextc i;
    i.tok_fn <- plaintext_state;
    process_token i l (`Char u_rep)
  | -1 ->
    let l = loc i in
    process_token i l `EOF
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- plaintext_state;
    process_token i l (`Char c)

(* 8.2.4.8  Tag open state *)
and tag_open_state i l' =
  match i.c with
  | 0x0021 ->
    nextc i;
    markup_declaration_open_state i l'
  | 0x002F ->
    nextc i;
    end_tag_open_state i l'
  | c when is_alphabetic c ->
    (* start tag *)
    Buffer.clear i.tag_name_buf;
    add_utf_8 i.tag_name_buf (to_lowercase c);
    i.is_start_tag <- true;
    i.self_closing_tag <- false;
    i.pending_attrs <- [];
    nextc i;
    tag_name_state i l'
  | 0x003F ->
    i.report l'
      (`Bad_token ("<?", "content",
                   "HTML does not have processing instructions"));
    bogus_comment_state i l'
  | c ->
    let l = loc i in
    (match c with
    | -1 ->
      i.report l (`Unexpected_eoi "tag")
    | _ ->
      i.report l (`Bad_token (char c, "tag", "invalid start character")));
    i.tok_fn <- data_state;
    process_token i l' (`Char 0x003C)

(* 8.2.4.9  End tag open state *)
and end_tag_open_state i l' =
  match i.c with
  | c when is_alphabetic c ->
    Buffer.clear i.tag_name_buf;
    add_utf_8 i.tag_name_buf (to_lowercase c);
    i.is_start_tag <- false;
    i.self_closing_tag <- false;
    i.pending_attrs <- [];
    nextc i;
    tag_name_state i l'
  | 0x003E ->
    i.report l' (`Bad_token ("</>", "tag", "no tag name"));
    nextc i;
    data_state i
  | -1 ->
    let l = loc i in
    i.report l (`Unexpected_eoi "tag");
    let line, column = l' in
    process_token i l' (`Char 0x003C);
    process_token i (line, column + 1) (`Char 0x002F);
    i.tok_fn <- data_state
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "tag", "invalid start character"));
    bogus_comment_state i l'

(* 8.2.4.10  Tag name state *)
and tag_name_state i l' =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_attribute_name_state i l'
  | 0x002F ->
    nextc i;
    self_closing_start_tag_state i l'
  | 0x003E ->
    nextc i;
    emit_tag_token i l'
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "tag name", "null"));
    add_utf_8 i.tag_name_buf u_rep;
    nextc i;
    tag_name_state i l'
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | c ->
    add_utf_8 i.tag_name_buf (to_lowercase c);
    nextc i;
    tag_name_state i l'

(* 8.2.4.11 / 8.2.4.14  Text less-than sign state *)
and text_less_than_sign_state state i l' cs =
  match i.c with
  | 0x002F ->
    let v = (loc i, 0x002F) in
    nextc i;
    text_end_tag_open_state state i l' (v :: cs)
  | _ ->
    (* Emit collected chars as char tokens; i.c stays as-is for state to handle next *)
    i.tok_fn <- state;
    List.iter (fun (l, c) -> process_token i l (`Char c)) cs

(* 8.2.4.12 / 8.2.4.15 / 8.2.4.18 / 8.2.4.26  Text end tag open state *)
and text_end_tag_open_state state i l' cs =
  match i.c with
  | c when is_alphabetic c ->
    let name_buffer = Buffer.create 32 in
    add_utf_8 name_buffer (to_lowercase c);
    let v = (loc i, c) in
    nextc i;
    text_end_tag_name_state state i l' (v :: cs) name_buffer
  | _ ->
    i.tok_fn <- state;
    List.iter (fun (l, c) -> process_token i l (`Char c)) (List.rev cs)

(* 8.2.4.13 / 8.2.4.16 / 8.2.4.19 / 8.2.4.27  Text end tag name state *)
and text_end_tag_name_state state i l' cs name_buffer =
  let create_tag () =
    {Token_tag.name = Buffer.contents name_buffer;
     self_closing    = false;
     attributes      = []}
  in
  match i.c with
  | (0x0009 | 0x000A | 0x000C | 0x0020)
      when Buffer.contents name_buffer = (match i.last_start_tag with Some n -> n | None -> "") ->
    nextc i;
    (* Set up end-tag state properly *)
    Buffer.clear i.tag_name_buf;
    Buffer.add_string i.tag_name_buf (Buffer.contents name_buffer);
    i.is_start_tag <- false;
    i.self_closing_tag <- false;
    i.pending_attrs <- [];
    before_attribute_name_state i l'
  | 0x002F
      when Buffer.contents name_buffer = (match i.last_start_tag with Some n -> n | None -> "") ->
    nextc i;
    Buffer.clear i.tag_name_buf;
    Buffer.add_string i.tag_name_buf (Buffer.contents name_buffer);
    i.is_start_tag <- false;
    i.self_closing_tag <- false;
    i.pending_attrs <- [];
    self_closing_start_tag_state i l'
  | 0x003E
      when Buffer.contents name_buffer = (match i.last_start_tag with Some n -> n | None -> "") ->
    nextc i;
    let tag = create_tag () in
    i.tok_fn <- data_state;
    i.mode i l' (`End tag)
  | c when is_alphabetic c ->
    add_utf_8 name_buffer (to_lowercase c);
    let v = (loc i, c) in
    nextc i;
    text_end_tag_name_state state i l' (v :: cs) name_buffer
  | _ ->
    i.tok_fn <- state;
    List.iter (fun (l, c) -> process_token i l (`Char c)) (List.rev cs)

(* 8.2.4.17  Script data less-than sign state *)
and script_data_less_than_sign_state i l' cs =
  match i.c with
  | 0x002F ->
    let v = (loc i, 0x002F) in
    nextc i;
    text_end_tag_open_state script_data_state i l' (v :: cs)
  | 0x0021 ->
    let lv = (loc i, 0x0021) in
    nextc i;
    (* emit all cs + '!' as chars, then go to escape start *)
    let all = List.rev (lv :: cs) in
    List.iter (fun (l, c) -> process_token i l (`Char c)) all;
    script_data_escape_start_state i l'
  | _ ->
    i.tok_fn <- script_data_state;
    List.iter (fun (l, c) -> process_token i l (`Char c)) cs

(* 8.2.4.20  Script data escape start state *)
and script_data_escape_start_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escape_start_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_escape_start_dash_state i l'
  | _ ->
    script_data_state i

(* 8.2.4.21  Script data escape start dash state *)
and script_data_escape_start_dash_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_dash_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_escaped_dash_dash_state i l'
  | _ ->
    script_data_state i

(* 8.2.4.22  Script data escaped state *)
and script_data_escaped_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_escaped_dash_state i l'
  | 0x003C ->
    let l = loc i in
    let cs = [(l, 0x003C)] in
    nextc i;
    script_data_escaped_less_than_sign_state i l' l cs
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "script", "null"));
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_state i l');
    process_token i l (`Char u_rep);
    script_data_escaped_state i l'
  | -1 ->
    let l = loc i in
    i.report l (`Unexpected_eoi "script");
    data_state i
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_state i l');
    process_token i l (`Char c);
    script_data_escaped_state i l'

(* 8.2.4.23  Script data escaped dash state *)
and script_data_escaped_dash_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_dash_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_escaped_dash_dash_state i l'
  | 0x003C ->
    let l = loc i in
    let cs = [(l, 0x003C)] in
    nextc i;
    script_data_escaped_less_than_sign_state i l' l cs
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "script", "null"));
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_state i l');
    process_token i l (`Char u_rep);
    script_data_escaped_state i l'
  | -1 ->
    let l = loc i in
    i.report l (`Unexpected_eoi "script");
    data_state i
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_state i l');
    process_token i l (`Char c);
    script_data_escaped_state i l'

(* 8.2.4.24  Script data escaped dash-dash state *)
and script_data_escaped_dash_dash_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_dash_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_escaped_dash_dash_state i l'
  | 0x003C ->
    let l = loc i in
    let cs = [(l, 0x003C)] in
    nextc i;
    script_data_escaped_less_than_sign_state i l' l cs
  | 0x003E ->
    let l = loc i in
    nextc i;
    i.tok_fn <- script_data_state;
    process_token i l (`Char 0x003E)
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "script", "null"));
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_state i l');
    process_token i l (`Char u_rep);
    script_data_escaped_state i l'
  | -1 ->
    let l = loc i in
    i.report l (`Unexpected_eoi "script");
    data_state i
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_escaped_state i l');
    process_token i l (`Char c);
    script_data_escaped_state i l'

(* 8.2.4.25  Script data escaped less-than sign state *)
and script_data_escaped_less_than_sign_state i l' _l'' cs =
  match i.c with
  | 0x002F ->
    let v = (loc i, 0x002F) in
    nextc i;
    text_end_tag_open_state (fun i -> script_data_escaped_state i l') i l' (v :: cs)
  | c when is_alphabetic c ->
    let tag_buffer = Buffer.create 32 in
    add_utf_8 tag_buffer (to_lowercase c);
    let v = (loc i, c) in
    nextc i;
    let all = List.rev (v :: cs) in
    List.iter (fun (l, ch) -> process_token i l (`Char ch)) all;
    script_data_double_escape_start_state i l' tag_buffer
  | _ ->
    i.tok_fn <- (fun i -> script_data_escaped_state i l');
    List.iter (fun (l, c) -> process_token i l (`Char c)) (List.rev cs)

(* 8.2.4.28  Script data double escape start state *)
and script_data_double_escape_start_state i l' tag_buffer =
  match i.c with
  | (0x0009 | 0x000A | 0x000C | 0x0020 | 0x002F | 0x003E as c) ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (if Buffer.contents tag_buffer = "script"
                 then (fun i -> script_data_double_escaped_state i l')
                 else (fun i -> script_data_escaped_state i l'));
    process_token i l (`Char c);
    if Buffer.contents tag_buffer = "script"
    then script_data_double_escaped_state i l'
    else script_data_escaped_state i l'
  | c when is_alphabetic c ->
    let l = loc i in
    add_utf_8 tag_buffer (to_lowercase c);
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escape_start_state i l' tag_buffer);
    process_token i l (`Char c);
    script_data_double_escape_start_state i l' tag_buffer
  | _ ->
    script_data_escaped_state i l'

(* 8.2.4.29  Script data double escaped state *)
and script_data_double_escaped_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_double_escaped_dash_state i l'
  | 0x003C ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_less_than_sign_state i l');
    process_token i l (`Char 0x003C);
    script_data_double_escaped_less_than_sign_state i l'
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "script", "null"));
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_state i l');
    process_token i l (`Char u_rep);
    script_data_double_escaped_state i l'
  | -1 ->
    let l = loc i in
    i.report l (`Unexpected_eoi "script");
    data_state i
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_state i l');
    process_token i l (`Char c);
    script_data_double_escaped_state i l'

(* 8.2.4.30  Script data double escaped dash state *)
and[@warning "-32"] script_data_double_escaped_dash_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_dash_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_double_escaped_dash_dash_state i l'
  | 0x003C ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_less_than_sign_state i l');
    process_token i l (`Char 0x003C);
    script_data_double_escaped_less_than_sign_state i l'
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "script", "null"));
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_state i l');
    process_token i l (`Char u_rep);
    script_data_double_escaped_state i l'
  | -1 ->
    let l = loc i in
    i.report l (`Unexpected_eoi "script");
    data_state i
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_state i l');
    process_token i l (`Char c);
    script_data_double_escaped_state i l'

(* 8.2.4.31  Script data double escaped dash-dash state *)
and[@warning "-32"] script_data_double_escaped_dash_dash_state i l' =
  match i.c with
  | 0x002D ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_dash_dash_state i l');
    process_token i l (`Char 0x002D);
    script_data_double_escaped_dash_dash_state i l'
  | 0x003C ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_less_than_sign_state i l');
    process_token i l (`Char 0x003C);
    script_data_double_escaped_less_than_sign_state i l'
  | 0x003E ->
    let l = loc i in
    nextc i;
    i.tok_fn <- script_data_state;
    process_token i l (`Char 0x003E)
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "script", "null"));
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_state i l');
    process_token i l (`Char u_rep);
    script_data_double_escaped_state i l'
  | -1 ->
    let l = loc i in
    i.report l (`Unexpected_eoi "script");
    data_state i
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escaped_state i l');
    process_token i l (`Char c);
    script_data_double_escaped_state i l'

(* 8.2.4.32  Script data double escaped less-than sign state *)
and[@warning "-32"] script_data_double_escaped_less_than_sign_state i l' =
  match i.c with
  | 0x002F ->
    let l = loc i in
    let tag_buffer = Buffer.create 32 in
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escape_end_state i l' tag_buffer);
    process_token i l (`Char 0x002F);
    script_data_double_escape_end_state i l' tag_buffer
  | _ ->
    script_data_double_escaped_state i l'

(* 8.2.4.33  Script data double escape end state *)
and[@warning "-32"] script_data_double_escape_end_state i l' tag_buffer =
  match i.c with
  | (0x0009 | 0x000A | 0x000C | 0x0020 | 0x002F | 0x003E as c) ->
    let l = loc i in
    nextc i;
    i.tok_fn <- (if Buffer.contents tag_buffer = "script"
                 then (fun i -> script_data_escaped_state i l')
                 else (fun i -> script_data_double_escaped_state i l'));
    process_token i l (`Char c);
    if Buffer.contents tag_buffer = "script"
    then script_data_escaped_state i l'
    else script_data_double_escaped_state i l'
  | c when is_alphabetic c ->
    let l = loc i in
    add_utf_8 tag_buffer (to_lowercase c);
    nextc i;
    i.tok_fn <- (fun i -> script_data_double_escape_end_state i l' tag_buffer);
    process_token i l (`Char c);
    script_data_double_escape_end_state i l' tag_buffer
  | _ ->
    script_data_double_escaped_state i l'

(* 8.2.4.34  Before attribute name state *)
and before_attribute_name_state i l' =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_attribute_name_state i l'
  | 0x002F ->
    nextc i;
    self_closing_start_tag_state i l'
  | 0x003E ->
    nextc i;
    emit_tag_token i l'
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "attribute name", "null"));
    Buffer.clear i.attr_name_buf;
    add_utf_8 i.attr_name_buf u_rep;
    nextc i;
    attribute_name_state i l'
  | (0x0022 | 0x0027 | 0x003C | 0x003D as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "attribute name", "invalid start character"));
    Buffer.clear i.attr_name_buf;
    add_utf_8 i.attr_name_buf c;
    nextc i;
    attribute_name_state i l'
  | c ->
    Buffer.clear i.attr_name_buf;
    add_utf_8 i.attr_name_buf (to_lowercase c);
    nextc i;
    attribute_name_state i l'

(* 8.2.4.35  Attribute name state *)
and attribute_name_state i l' =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    let name = Buffer.contents i.attr_name_buf in
    nextc i;
    after_attribute_name_state i l' name
  | 0x002F ->
    let name = Buffer.contents i.attr_name_buf in
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    nextc i;
    self_closing_start_tag_state i l'
  | 0x003D ->
    let name = Buffer.contents i.attr_name_buf in
    nextc i;
    before_attribute_value_state i l' name
  | 0x003E ->
    let name = Buffer.contents i.attr_name_buf in
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    nextc i;
    emit_tag_token i l'
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "attribute name", "null"));
    add_utf_8 i.attr_name_buf u_rep;
    nextc i;
    attribute_name_state i l'
  | (0x0022 | 0x0027 | 0x003C as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "attribute name", "invalid name character"));
    add_utf_8 i.attr_name_buf c;
    nextc i;
    attribute_name_state i l'
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | c ->
    add_utf_8 i.attr_name_buf (to_lowercase c);
    nextc i;
    attribute_name_state i l'

(* 8.2.4.36  After attribute name state *)
and after_attribute_name_state i l' name =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    after_attribute_name_state i l' name
  | 0x002F ->
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    nextc i;
    self_closing_start_tag_state i l'
  | 0x003D ->
    nextc i;
    before_attribute_value_state i l' name
  | 0x003E ->
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    nextc i;
    emit_tag_token i l'
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "attribute name", "null"));
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    Buffer.clear i.attr_name_buf;
    add_utf_8 i.attr_name_buf u_rep;
    nextc i;
    attribute_name_state i l'
  | (0x0022 | 0x0027 | 0x003C as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "attribute name", "invalid start character"));
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    Buffer.clear i.attr_name_buf;
    add_utf_8 i.attr_name_buf c;
    nextc i;
    attribute_name_state i l'
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | c ->
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    Buffer.clear i.attr_name_buf;
    add_utf_8 i.attr_name_buf (to_lowercase c);
    nextc i;
    attribute_name_state i l'

(* 8.2.4.37  Before attribute value state *)
and before_attribute_value_state i l' name =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_attribute_value_state i l' name
  | (0x0022 | 0x0027 as c) ->
    Buffer.clear i.attr_val_buf;
    nextc i;
    attribute_value_quoted_state i l' name c
  | 0x0026 ->
    (* don't consume; go directly to unquoted *)
    Buffer.clear i.attr_val_buf;
    attribute_value_unquoted_state i l' name
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "attribute value", "null"));
    Buffer.clear i.attr_val_buf;
    add_utf_8 i.attr_val_buf u_rep;
    nextc i;
    attribute_value_unquoted_state i l' name
  | 0x003E ->
    let l = loc i in
    i.report l (`Bad_token (">", "tag", "expected attribute value after '='"));
    i.pending_attrs <- (name, "") :: i.pending_attrs;
    nextc i;
    emit_tag_token i l'
  | (0x003C | 0x003D | 0x0060 as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "attribute value", "invalid start character"));
    Buffer.clear i.attr_val_buf;
    add_utf_8 i.attr_val_buf c;
    nextc i;
    attribute_value_unquoted_state i l' name
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | c ->
    Buffer.clear i.attr_val_buf;
    add_utf_8 i.attr_val_buf c;
    nextc i;
    attribute_value_unquoted_state i l' name

(* 8.2.4.38 and 8.2.4.39  Attribute value quoted state *)
and attribute_value_quoted_state i l' name quote =
  match i.c with
  | c when c = quote ->
    i.pending_attrs <- (name, Buffer.contents i.attr_val_buf) :: i.pending_attrs;
    nextc i;
    after_attribute_value_quoted_state i l'
  | 0x0026 ->
    let l = loc i in
    nextc i;
    character_reference_in_attribute i ~allowed:quote l i.attr_val_buf;
    attribute_value_quoted_state i l' name quote
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "attribute value", "null"));
    add_utf_8 i.attr_val_buf u_rep;
    nextc i;
    attribute_value_quoted_state i l' name quote
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "attribute value");
    data_state i
  | c ->
    add_utf_8 i.attr_val_buf c;
    nextc i;
    attribute_value_quoted_state i l' name quote

(* 8.2.4.40  Attribute value unquoted state *)
and attribute_value_unquoted_state i l' name =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    i.pending_attrs <- (name, Buffer.contents i.attr_val_buf) :: i.pending_attrs;
    nextc i;
    before_attribute_name_state i l'
  | 0x0026 ->
    let l = loc i in
    nextc i;
    character_reference_in_attribute i ~allowed:0x003E l i.attr_val_buf;
    attribute_value_unquoted_state i l' name
  | 0x003E ->
    i.pending_attrs <- (name, Buffer.contents i.attr_val_buf) :: i.pending_attrs;
    nextc i;
    emit_tag_token i l'
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "attribute value", "null"));
    add_utf_8 i.attr_val_buf u_rep;
    nextc i;
    attribute_value_unquoted_state i l' name
  | (0x0022 | 0x0027 | 0x003C | 0x003D | 0x0060 as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "attribute value", "invalid character"));
    add_utf_8 i.attr_val_buf c;
    nextc i;
    attribute_value_unquoted_state i l' name
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | c ->
    add_utf_8 i.attr_val_buf c;
    nextc i;
    attribute_value_unquoted_state i l' name

(* 8.2.4.41  Character reference in attribute *)
and character_reference_in_attribute i ~allowed l value_buffer =
  let result = consume_character_reference i ~in_attribute:true ~additional:(Some allowed) l in
  match result with
  | None ->
    add_utf_8 value_buffer 0x0026
  | Some (`One c) ->
    add_utf_8 value_buffer c
  | Some (`Two (c, c')) ->
    add_utf_8 value_buffer c;
    add_utf_8 value_buffer c'

(* 8.2.4.42  After attribute value (quoted) state *)
and after_attribute_value_quoted_state i l' =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_attribute_name_state i l'
  | 0x002F ->
    nextc i;
    self_closing_start_tag_state i l'
  | 0x003E ->
    nextc i;
    emit_tag_token i l'
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | c ->
    (* push back c and go to before_attribute_name *)
    (* c is current i.c; we don't advance - just report and go *)
    let l = loc i in
    i.report l (`Bad_token (char c, "tag", "expected whitespace before attribute"));
    before_attribute_name_state i l'

(* 8.2.4.43  Self-closing start tag state *)
and self_closing_start_tag_state i l' =
  match i.c with
  | 0x003E ->
    i.self_closing_tag <- true;
    nextc i;
    emit_tag_token i l'
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "tag");
    data_state i
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "tag", "expected '/>'"));
    before_attribute_name_state i l'

(* 8.2.4.44  Bogus comment state *)
and bogus_comment_state i l' =
  let buffer = Buffer.create 256 in
  let rec consume () =
    match i.c with
    | 0x003E ->
      nextc i;
      emit_comment i l' buffer
    | 0 ->
      add_utf_8 buffer u_rep;
      nextc i;
      consume ()
    | -1 ->
      emit_comment i l' buffer
    | c ->
      add_utf_8 buffer c;
      nextc i;
      consume ()
  in
  consume ()

and emit_comment i l' buffer =
  let s = Buffer.contents buffer in
  i.tok_fn <- data_state;
  process_token i l' (`Comment s)

(* 8.2.4.45  Markup declaration open state *)
and markup_declaration_open_state i l' =
  (* Peek at next 2 chars to detect '--' *)
  let c0 = i.c in
  if c0 = 0x002D then begin
    nextc i;
    let c1 = i.c in
    if c1 = 0x002D then begin
      nextc i;
      comment_start_state i l' (Buffer.create 64)
    end else begin
      (* push back: we consumed c0 but c1 is still i.c *)
      pushback_char i c0;
      (* Now check for DOCTYPE (7 chars) or CDATA (7 chars) *)
      check_doctype_or_cdata i l'
    end
  end else
    check_doctype_or_cdata i l'

and check_doctype_or_cdata i l' =
  (* Try to match "DOCTYPE" (case-insensitive, 7 chars) *)
  let doctype_chars = [| 0x64; 0x6F; 0x63; 0x74; 0x79; 0x70; 0x65 |] in (* doctype *)
  let cdata_chars   = [| 0x5B; 0x43; 0x44; 0x41; 0x54; 0x41; 0x5B |] in (* [CDATA[ *)
  let collected = Array.make 7 (-1) in
  let n = ref 0 in
  while !n < 7 && i.c >= 0 do
    collected.(!n) <- i.c;
    incr n;
    if !n < 7 then nextc i
  done;
  let is_doctype =
    !n = 7 &&
    Array.for_all2 (fun a b -> to_lowercase a = b) collected doctype_chars
  in
  let is_cdata =
    !n = 7 &&
    Array.for_all2 (=) collected cdata_chars
  in
  if is_doctype then begin
    nextc i; (* advance past the last 'e' in 'doctype' *)
    doctype_state i l'
  end else if is_cdata then begin
    nextc i; (* advance past '[' *)
    if i.foreign () then
      cdata_section_state i
    else begin
      i.report l'
        (`Bad_token ("<![CDATA[", "content",
                     "CDATA sections not allowed in HTML"));
      bogus_comment_state i l'
    end
  end else begin
    (* push all collected chars back *)
    if !n > 0 then begin
      let pb = ref [] in
      for j = !n - 1 downto 1 do
        pb := collected.(j) :: !pb
      done;
      i.pushback <- !pb @ i.pushback;
      i.c <- collected.(0)
    end;
    i.report l'
      (`Bad_token ("<!", "comment", "should begin with '<!--'"));
    bogus_comment_state i l'
  end

(* 8.2.4.46  Comment start state *)
and comment_start_state i l' buffer =
  match i.c with
  | 0x002D ->
    nextc i;
    comment_start_dash_state i l' buffer
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "comment", "null"));
    add_utf_8 buffer u_rep;
    nextc i;
    comment_state i l' buffer
  | 0x003E ->
    i.report l' (`Bad_token ("<!-->", "comment", "'-->' overlaps '<!--'"));
    nextc i;
    emit_comment i l' buffer
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "comment");
    emit_comment i l' buffer
  | c ->
    add_utf_8 buffer c;
    nextc i;
    comment_state i l' buffer

(* 8.2.4.47  Comment start dash state *)
and comment_start_dash_state i l' buffer =
  match i.c with
  | 0x002D ->
    nextc i;
    comment_end_state i l' buffer
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "comment", "null"));
    Buffer.add_char buffer '-';
    add_utf_8 buffer u_rep;
    nextc i;
    comment_state i l' buffer
  | 0x003E ->
    i.report l' (`Bad_token ("<!--->", "comment", "'-->' overlaps '<!--'"));
    nextc i;
    emit_comment i l' buffer
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "comment");
    emit_comment i l' buffer
  | c ->
    Buffer.add_char buffer '-';
    add_utf_8 buffer c;
    nextc i;
    comment_state i l' buffer

(* 8.2.4.48  Comment state *)
and comment_state i l' buffer =
  match i.c with
  | 0x002D ->
    nextc i;
    comment_end_dash_state i l' buffer
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "comment", "null"));
    add_utf_8 buffer u_rep;
    nextc i;
    comment_state i l' buffer
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "comment");
    emit_comment i l' buffer
  | c ->
    add_utf_8 buffer c;
    nextc i;
    comment_state i l' buffer

(* 8.2.4.49  Comment end dash state *)
and comment_end_dash_state i l' buffer =
  match i.c with
  | 0x002D ->
    nextc i;
    comment_end_state i l' buffer
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "comment", "null"));
    Buffer.add_char buffer '-';
    add_utf_8 buffer u_rep;
    nextc i;
    comment_state i l' buffer
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "comment");
    emit_comment i l' buffer
  | c ->
    Buffer.add_char buffer '-';
    add_utf_8 buffer c;
    nextc i;
    comment_state i l' buffer

(* 8.2.4.50  Comment end state *)
and comment_end_state i l' buffer =
  match i.c with
  | 0x003E ->
    nextc i;
    emit_comment i l' buffer
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "comment", "null"));
    Buffer.add_string buffer "--";
    add_utf_8 buffer u_rep;
    nextc i;
    comment_state i l' buffer
  | 0x0021 ->
    let l = loc i in
    i.report l (`Bad_token ("--!", "comment", "'--' should be in '-->'"));
    nextc i;
    comment_end_bang_state i l' buffer
  | 0x002D ->
    let l = loc i in
    i.report l (`Bad_token ("---", "comment", "'--' should be in '-->'"));
    Buffer.add_char buffer '-';
    nextc i;
    comment_end_state i l' buffer
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "comment");
    emit_comment i l' buffer
  | c ->
    let l = loc i in
    i.report l (`Bad_token ("--" ^ (char c), "comment",
                            "'--' should be in '-->'"));
    Buffer.add_string buffer "--";
    add_utf_8 buffer c;
    nextc i;
    comment_state i l' buffer

(* 8.2.4.51  Comment end bang state *)
and comment_end_bang_state i l' buffer =
  match i.c with
  | 0x002D ->
    Buffer.add_string buffer "--!";
    nextc i;
    comment_end_dash_state i l' buffer
  | 0x003E ->
    nextc i;
    emit_comment i l' buffer
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "comment", "null"));
    Buffer.add_string buffer "--!";
    add_utf_8 buffer u_rep;
    nextc i;
    comment_state i l' buffer
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "comment");
    emit_comment i l' buffer
  | c ->
    Buffer.add_string buffer "--!";
    add_utf_8 buffer c;
    nextc i;
    comment_state i l' buffer

(* 8.2.5.52  DOCTYPE state *)
and doctype_state i l' =
  let doctype =
    {doctype_name      = None;
     public_identifier = None;
     system_identifier = None;
     force_quirks      = false}
  in
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_doctype_name_state i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "expected whitespace"));
    before_doctype_name_state i l' doctype

(* 8.2.5.53  Before DOCTYPE name state *)
and before_doctype_name_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_doctype_name_state i l' doctype
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "doctype", "null"));
    doctype.doctype_name <-
      add_doctype_char doctype.doctype_name u_rep;
    nextc i;
    doctype_name_state i l' doctype
  | 0x003E ->
    let l = loc i in
    i.report l (`Bad_token (">", "doctype", "expected name"));
    nextc i;
    emit_doctype_token ~quirks:true i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    doctype.doctype_name <-
      add_doctype_char doctype.doctype_name (to_lowercase c);
    nextc i;
    doctype_name_state i l' doctype

(* 8.2.5.54  DOCTYPE name state *)
and doctype_name_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    after_doctype_name_state i l' doctype
  | 0x003E ->
    nextc i;
    emit_doctype_token i l' doctype
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "doctype", "null"));
    doctype.doctype_name <-
      add_doctype_char doctype.doctype_name u_rep;
    nextc i;
    doctype_name_state i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    doctype.doctype_name <-
      add_doctype_char doctype.doctype_name (to_lowercase c);
    nextc i;
    doctype_name_state i l' doctype

(* 8.2.4.55  After DOCTYPE name state *)
and after_doctype_name_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    after_doctype_name_state i l' doctype
  | 0x003E ->
    nextc i;
    emit_doctype_token i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | _ ->
    (* Peek 6 chars to check for PUBLIC or SYSTEM *)
    let peek_chars = Array.make 6 (-1) in
    let n = ref 0 in
    let first_c = i.c in
    let first_l = loc i in
    while !n < 6 && i.c >= 0 do
      peek_chars.(!n) <- i.c;
      incr n;
      if !n < 6 then nextc i
    done;
    let lc = Array.map to_lowercase peek_chars in
    let is_public =
      !n = 6 &&
      lc.(0) = 0x70 && lc.(1) = 0x75 && lc.(2) = 0x62 &&
      lc.(3) = 0x6C && lc.(4) = 0x69 && lc.(5) = 0x63
    in
    let is_system =
      !n = 6 &&
      lc.(0) = 0x73 && lc.(1) = 0x79 && lc.(2) = 0x73 &&
      lc.(3) = 0x74 && lc.(4) = 0x65 && lc.(5) = 0x6D
    in
    if is_public then begin
      nextc i; (* advance past the last char *)
      after_doctype_public_keyword_state i l' doctype
    end else if is_system then begin
      nextc i;
      after_doctype_system_keyword_state i l' doctype
    end else begin
      (* push all collected chars back *)
      if !n > 0 then begin
        let pb = ref [] in
        for j = !n - 1 downto 1 do
          pb := peek_chars.(j) :: !pb
        done;
        i.pushback <- !pb @ i.pushback;
        i.c <- peek_chars.(0)
      end;
      i.report first_l (`Bad_token (char first_c, "doctype",
                                    "expected 'PUBLIC' or 'SYSTEM'"));
      doctype.force_quirks <- true;
      bogus_doctype_state i l' doctype
    end

(* Helper: begin public identifier *)
and begin_public_identifier i quote l' doctype =
  doctype.public_identifier <- Some (Buffer.create 32);
  let add_fn doctype c =
    doctype.public_identifier <-
      add_doctype_char doctype.public_identifier c
  in
  doctype_identifier_quoted_state i add_fn quote after_doctype_public_identifier_state l' doctype

(* Helper: begin system identifier *)
and begin_system_identifier i quote l' doctype =
  doctype.system_identifier <- Some (Buffer.create 32);
  let add_fn doctype c =
    doctype.system_identifier <-
      add_doctype_char doctype.system_identifier c
  in
  doctype_identifier_quoted_state i add_fn quote after_doctype_system_identifier_state l' doctype

(* 8.2.4.56  After DOCTYPE public keyword state *)
and after_doctype_public_keyword_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_doctype_public_identifier_state i l' doctype
  | (0x0022 | 0x0027 as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "expected whitespace"));
    nextc i;
    begin_public_identifier i c l' doctype
  | 0x003E ->
    let l = loc i in
    i.report l (`Bad_token (">", "doctype", "expected public identifier"));
    nextc i;
    emit_doctype_token ~quirks:true i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "expected whitespace"));
    doctype.force_quirks <- true;
    bogus_doctype_state i l' doctype

(* 8.2.4.57  Before DOCTYPE public identifier state *)
and before_doctype_public_identifier_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_doctype_public_identifier_state i l' doctype
  | (0x0022 | 0x0027 as c) ->
    nextc i;
    begin_public_identifier i c l' doctype
  | 0x003E ->
    let l = loc i in
    i.report l (`Bad_token (">", "doctype", "expected public identifier"));
    nextc i;
    emit_doctype_token ~quirks:true i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "public identifier must be quoted"));
    doctype.force_quirks <- true;
    bogus_doctype_state i l' doctype

(* 8.2.4.58 / 8.2.4.59 / 8.2.4.64 / 8.2.4.65  DOCTYPE identifier quoted state *)
and doctype_identifier_quoted_state i add quote next_state l' doctype =
  match i.c with
  | c when c = quote ->
    nextc i;
    next_state i l' doctype
  | 0 ->
    let l = loc i in
    i.report l (`Bad_token ("U+0000", "doctype", "null"));
    add doctype u_rep;
    nextc i;
    doctype_identifier_quoted_state i add quote next_state l' doctype
  | 0x003E ->
    let l = loc i in
    i.report l (`Bad_token (">", "doctype", "'>' in identifier"));
    nextc i;
    emit_doctype_token ~quirks:true i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    add doctype c;
    nextc i;
    doctype_identifier_quoted_state i add quote next_state l' doctype

(* 8.2.4.60  After DOCTYPE public identifier state *)
and after_doctype_public_identifier_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    between_doctype_public_and_system_identifiers i l' doctype
  | 0x003E ->
    nextc i;
    emit_doctype_token i l' doctype
  | (0x0022 | 0x0027 as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "expected whitespace"));
    nextc i;
    begin_system_identifier i c l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "system identifier must be quoted"));
    doctype.force_quirks <- true;
    bogus_doctype_state i l' doctype

(* 8.2.4.61  Between DOCTYPE public and system identifiers state *)
and between_doctype_public_and_system_identifiers i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    between_doctype_public_and_system_identifiers i l' doctype
  | 0x003E ->
    nextc i;
    emit_doctype_token i l' doctype
  | (0x0022 | 0x0027 as c) ->
    nextc i;
    begin_system_identifier i c l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "system identifier must be quoted"));
    doctype.force_quirks <- true;
    bogus_doctype_state i l' doctype

(* 8.2.4.62  After DOCTYPE system keyword state *)
and after_doctype_system_keyword_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_doctype_system_identifier_state i l' doctype
  | (0x0022 | 0x0027 as c) ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "expected whitespace"));
    nextc i;
    begin_system_identifier i c l' doctype
  | 0x003E ->
    let l = loc i in
    i.report l (`Bad_token (">", "doctype", "expected system identifier"));
    nextc i;
    emit_doctype_token ~quirks:true i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "expected whitespace"));
    doctype.force_quirks <- true;
    bogus_doctype_state i l' doctype

(* 8.2.4.63  Before DOCTYPE system identifier state *)
and before_doctype_system_identifier_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    before_doctype_system_identifier_state i l' doctype
  | (0x0022 | 0x0027 as c) ->
    nextc i;
    begin_system_identifier i c l' doctype
  | 0x003E ->
    let l = loc i in
    i.report l (`Bad_token (">", "doctype", "expected system identifier"));
    nextc i;
    emit_doctype_token ~quirks:true i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "system identifier must be quoted"));
    doctype.force_quirks <- true;
    bogus_doctype_state i l' doctype

(* 8.2.4.66  After DOCTYPE system identifier state *)
and after_doctype_system_identifier_state i l' doctype =
  match i.c with
  | 0x0009 | 0x000A | 0x000C | 0x0020 ->
    nextc i;
    after_doctype_system_identifier_state i l' doctype
  | 0x003E ->
    nextc i;
    emit_doctype_token i l' doctype
  | -1 ->
    i.report (loc i) (`Unexpected_eoi "doctype");
    emit_doctype_token ~quirks:true i l' doctype
  | c ->
    let l = loc i in
    i.report l (`Bad_token (char c, "doctype", "junk after system identifier"));
    bogus_doctype_state i l' doctype

(* 8.2.4.67  Bogus DOCTYPE state *)
and bogus_doctype_state i l' doctype =
  match i.c with
  | 0x003E ->
    nextc i;
    emit_doctype_token i l' doctype
  | -1 ->
    emit_doctype_token i l' doctype
  | _ ->
    nextc i;
    bogus_doctype_state i l' doctype

and emit_doctype_token ?(quirks = false) i l' doctype =
  if quirks then
    doctype.force_quirks <- true;
  let if_not_missing = function
    | None -> None
    | Some buffer -> Some (Buffer.contents buffer)
  in
  let d =
    {Common.doctype_name = if_not_missing doctype.doctype_name;
     public_identifier   = if_not_missing doctype.public_identifier;
     system_identifier   = if_not_missing doctype.system_identifier;
     raw_text            = None;
     force_quirks        = doctype.force_quirks}
  in
  i.tok_fn <- data_state;
  process_token i l' (`Doctype d)

(* 8.2.4.68  CDATA section state *)
and cdata_section_state i =
  match i.c with
  | -1 ->
    data_state i
  | 0x005D ->
    let l = loc i in
    nextc i;
    (* peek 2 more *)
    let c1 = i.c in
    if c1 = 0x005D then begin
      nextc i;
      let c2 = i.c in
      if c2 = 0x003E then begin
        nextc i;
        data_state i
      end else begin
        (* push back: emit ']', keep scanning *)
        i.tok_fn <- cdata_section_state;
        process_token i l (`Char 0x005D);
        (* don't re-push c1 or c2: c1=']' is current i.c, c2 is current i.c too *)
        (* We need to re-check from c1. c1 is now consumed (we did nextc after it).
           c2 is current i.c. So we need to push c1 back. *)
        pushback_char i c1;
        cdata_section_state i
      end
    end else begin
      (* not ]] - emit ']' and continue with current i.c = c1 *)
      i.tok_fn <- cdata_section_state;
      process_token i l (`Char 0x005D);
      cdata_section_state i
    end
  | c ->
    let l = loc i in
    nextc i;
    i.tok_fn <- cdata_section_state;
    process_token i l (`Char c)

(* ---- consume_character_reference ---- *)

(* Implementation of HTML 8.2.4.69 Tokenizing character references.
   Returns None (emit '&' literally) or Some (`One c) or Some (`Two (c1,c2)). *)
and consume_character_reference i ~in_attribute ~additional l =
  let peek = i.c in
  (* Return None for certain characters without consuming *)
  match peek with
  | 0x0009 | 0x000A | 0x000C | 0x0020 | 0x003C | 0x0026 | (-1) -> None
  | c when Some c = additional -> None
  | 0x0023 ->
    (* Numeric character reference *)
    nextc i;
    let after_hash = i.c in
    (match after_hash with
    | (0x0078 | 0x0058 as hex_marker) ->
      (* Hex reference &#x... or &#X... *)
      let prefix = Printf.sprintf "&#%c" (Char.chr hex_marker) in
      nextc i;
      let buffer = Buffer.create 8 in
      let rec consume_hex () =
        if is_hex_digit i.c then begin
          Buffer.add_char buffer (Char.chr i.c);
          nextc i;
          consume_hex ()
        end
      in
      consume_hex ();
      if Buffer.length buffer = 0 then begin
        (* Failed: push back hash and hex_marker char *)
        pushback_char i hex_marker;
        pushback_char i 0x0023;
        i.report l (`Bad_token (prefix, "character reference", "expected digits"));
        None
      end else begin
        let s = Buffer.contents buffer in
        let has_semi = i.c = 0x003B in
        if has_semi then nextc i;
        let semicolon = if has_semi then ";" else "" in
        if not has_semi then
          i.report l (`Bad_token (prefix ^ s, "character reference", "missing ';' at end"));
        let maybe_n = try Some (int_of_string ("0x" ^ s)) with Failure _ -> None in
        match maybe_n with
        | None ->
          i.report l (`Bad_token (prefix ^ s ^ semicolon, "character reference", "out of range"));
          Some (`One u_rep)
        | Some n ->
          let n = replace_windows_1252_entity n in
          validate_codepoint i l (prefix ^ s ^ semicolon) n
      end
    | _ ->
      (* Decimal reference &#... *)
      let prefix = "&#" in
      let buffer = Buffer.create 8 in
      let rec consume_dec () =
        if is_digit i.c then begin
          Buffer.add_char buffer (Char.chr i.c);
          nextc i;
          consume_dec ()
        end
      in
      consume_dec ();
      if Buffer.length buffer = 0 then begin
        pushback_char i 0x0023;
        i.report l (`Bad_token (prefix, "character reference", "expected digits"));
        None
      end else begin
        let s = Buffer.contents buffer in
        let has_semi = i.c = 0x003B in
        if has_semi then nextc i;
        let semicolon = if has_semi then ";" else "" in
        if not has_semi then
          i.report l (`Bad_token (prefix ^ s, "character reference", "missing ';' at end"));
        let maybe_n = try Some (int_of_string s) with Failure _ -> None in
        match maybe_n with
        | None ->
          i.report l (`Bad_token (prefix ^ s ^ semicolon, "character reference", "out of range"));
          Some (`One u_rep)
        | Some n' ->
          let n = replace_windows_1252_entity n' in
          if n <> n' then begin
            i.report l (`Bad_token (prefix ^ s ^ semicolon, "character reference",
                                    "Windows-1252 character"));
            Some (`One n)
          end else
            validate_codepoint i l (prefix ^ s ^ semicolon) n
      end)
  | _ ->
    (* Named entity matching *)
    let trie = Lazy.force named_entity_trie in
    (* We need to scan forward matching the trie, tracking all consumed chars *)
    let all = ref [] in  (* newest-first list of consumed chars *)
    let best = ref None in  (* best match so far: (text, codepoints) *)
    let n_replace = ref 0 in  (* chars after the match (to push back) *)
    let text = Buffer.create 16 in

    let rec scan_named () =
      let c = i.c in
      if c < 0 then
        finish ()
      else begin
        let trie' = Trie.advance c trie in
        (* Note: we're passing the current trie state, not the advanced one.
           Actually Trie.advance returns the next state after consuming c. *)
        add_utf_8 text c;
        all := c :: !all;
        (match Trie.matches trie' with
        | Trie.No ->
          n_replace := !n_replace + 1;
          nextc i;
          finish ()
        | Trie.Prefix ->
          n_replace := !n_replace + 1;
          nextc i;
          scan_named_with trie'
        | Trie.Multiple m ->
          let w = Buffer.contents text in
          best := Some (w, m);
          n_replace := 0;
          nextc i;
          scan_named_with trie'
        | Trie.Yes m ->
          let w = Buffer.contents text in
          best := Some (w, m);
          n_replace := 0;
          nextc i;
          finish ())
      end

    and scan_named_with trie_state =
      let c = i.c in
      if c < 0 then
        finish ()
      else begin
        let trie' = Trie.advance c trie_state in
        add_utf_8 text c;
        all := c :: !all;
        (match Trie.matches trie' with
        | Trie.No ->
          n_replace := !n_replace + 1;
          nextc i;
          finish ()
        | Trie.Prefix ->
          n_replace := !n_replace + 1;
          nextc i;
          scan_named_with trie'
        | Trie.Multiple m ->
          let w = Buffer.contents text in
          best := Some (w, m);
          n_replace := 0;
          nextc i;
          scan_named_with trie'
        | Trie.Yes m ->
          let w = Buffer.contents text in
          best := Some (w, m);
          n_replace := 0;
          nextc i;
          finish ())
      end

    and finish () =
      (* Push back the n_replace newest chars in all *)
      let all_list = !all in
      let rec split_rev n acc = function
        | rest when n = 0 ->
          (* push acc (oldest-first) back *)
          (match acc with
          | [] -> ()
          | first :: rest_acc ->
            let tail = List.rev rest_acc in
            i.pushback <- tail @ (i.c :: i.pushback);
            i.c <- first);
          rest
        | v :: rest -> split_rev (n - 1) (v :: acc) rest
        | [] -> acc
      in
      let _matched = split_rev !n_replace [] all_list in
      ignore _matched
    in
    scan_named ();

    (* Now check the best match *)
    match !best with
    | None ->
      (* No entity found - check if it looks like an entity (for error reporting) *)
      (* The chars were already pushed back by finish(); i.c is restored *)
      None
    | Some (text_s, code_points) ->
      (* Check for semicolon *)
      (match i.c with
      | 0x003B ->
        nextc i;
        Some code_points
      | _ ->
        let unterminated () =
          i.report l (`Bad_token ("&" ^ text_s, "entity reference", "missing ';' at end"));
          Some code_points
        in
        if not in_attribute then
          unterminated ()
        else
          match i.c with
          | c when is_alphanumeric c ->
            (* In attribute: push matched chars back, return None *)
            let n_matched = List.length !all - !n_replace in
            let rec push_back_n n = function
              | [] -> ()
              | _ when n = 0 -> ()
              | c' :: rest ->
                push_back_n (n-1) rest;
                pushback_char i c'
            in
            let matched_chars = List.filteri (fun idx _ -> idx < n_matched) !all in
            push_back_n (List.length matched_chars) matched_chars;
            None
          | 0x003D ->
            i.report l (`Bad_token ("&" ^ text_s ^ "=", "attribute",
                                    "unterminated entity reference followed by '='"));
            let n_matched = List.length !all - !n_replace in
            let rec push_back_n n = function
              | [] -> ()
              | _ when n = 0 -> ()
              | c' :: rest ->
                push_back_n (n-1) rest;
                pushback_char i c'
            in
            let matched_chars = List.filteri (fun idx _ -> idx < n_matched) !all in
            push_back_n (List.length matched_chars) matched_chars;
            None
          | _ ->
            unterminated ())

and validate_codepoint i l ref_text n =
  ignore i;
  if not (is_scalar n) || n = 0 then begin
    i.report l (`Bad_token (ref_text, "character reference", "out of range"));
    Some (`One u_rep)
  end else if is_control_character n || is_non_character n then begin
    i.report l (`Bad_token (ref_text, "character reference", "invalid HTML character"));
    Some (`One n)
  end else
    Some (`One n)

(* ---- Parser helper: report_if ---- *)

let report_if_p i condition location msg =
  if condition then i.report location msg

(* ---- Parser helper: set_tokenizer_state ---- *)

let set_tokenizer_state i = function
  | `Data -> i.tok_fn <- data_state
  | `RCDATA -> i.tok_fn <- rcdata_state
  | `RAWTEXT -> i.tok_fn <- rawtext_state
  | `Script_data -> i.tok_fn <- script_data_state
  | `PLAINTEXT -> i.tok_fn <- plaintext_state

(* ---- Emit helpers for parser ---- *)

(* emit signal and set new mode *)
let emit_and_set_mode i l s next_mode =
  i.current_mode <- next_mode;
  emit_signal i l s

(* emit list of signals *)
let rec emit_list i = function
  | [] -> ()
  | (l, s)::more ->
    emit_signal i l s;
    emit_list i more

(* ---- Pop helpers ---- *)

let pop_element i l =
  match !(i.open_elements) with
  | [] -> ()
  | element::more ->
    flush_text i;
    (if element.buffering then begin
      let signals = Subtree.disable i.subtree_buffer in
      emit_list i signals
    end);
    i.open_elements := more;
    element.is_open <- false;
    if not element.suppress then
      emit_signal i l `End_element

let rec pop_until condition i l =
  match !(i.open_elements) with
  | [] -> ()
  | element::_ ->
    if condition element then ()
    else begin
      pop_element i l;
      pop_until condition i l
    end

let close_element_ns ?(ns = `HTML) i l name =
  pop_until
    (fun {element_name = ns', name'} -> ns' = ns && name' = name) i l;
  pop_element i l

let pop_until_and_raise_errors i names l =
  let rec iterate () =
    match !(i.open_elements) with
    | [] -> ()
    | {element_name = ns, name} :: _ ->
      if ns = `HTML && list_mem_string name names then pop_element i l
      else begin
        i.report l (`Unmatched_start_tag name);
        pop_element i l;
        iterate ()
      end
  in
  iterate ()

let pop_implied ?(except = "") i l =
  pop_until (fun {element_name = _, name} ->
    name = except ||
    not @@ list_mem_string name
      ["dd"; "dt"; "li"; "option"; "optgroup"; "p"; "rb"; "rp"; "rt"; "rtc"])
    i l

let pop_to_table_context i l =
  pop_until (function
    | {element_name = `HTML, ("table" | "template" | "html")} -> true
    | _ -> false) i l

let pop_to_table_body_context i l =
  pop_until (function
    | {element_name =
        `HTML, ("tbody" | "thead" | "tfoot" | "template" | "html")} -> true
    | _ -> false) i l

let pop_to_table_row_context i l =
  pop_until (function
    | {element_name = `HTML, ("tr" | "template" | "html")} -> true
    | _ -> false) i l

(* ---- push_and_emit ---- *)

let push_and_emit ?(formatting = false) ?(acknowledge = false) ?(namespace = `HTML)
    ?(set_form_element_pointer = false) i l
    ({Token_tag.name; attributes; self_closing} as tag) =
  report_if_p i (self_closing && not acknowledge) l
    (`Bad_token ("/>", "tag", "should not be self-closing"));

  let namespace_string = Ns.to_string namespace in

  let tag_name =
    match namespace with
    | `SVG -> Foreign.adjust_svg_tag_name name
    | _ -> name
  in

  let is_html_integration_point =
    Foreign.is_html_integration_point namespace tag_name attributes in

  let attributes =
    List.map (fun (n, v) -> Namespace.Parsing.parse n, v) attributes in
  let attributes =
    match namespace with
    | `HTML | `Other _ -> attributes
    | `MathML -> Foreign.adjust_mathml_attributes attributes
    | `SVG -> Foreign.adjust_svg_attributes attributes
  in

  let element_entry =
    Element.create ~is_html_integration_point (namespace, name) l in
  i.open_elements := element_entry :: !(i.open_elements);

  if set_form_element_pointer then
    i.form_element_pointer := Some element_entry;

  if formatting then
    i.active_formatting_elements :=
      Active.Element_ (element_entry, l, tag) ::
        !(i.active_formatting_elements);

  emit_signal i l (`Start_element ((namespace_string, tag_name), attributes))

let push_implicit i l name =
  push_and_emit i l
    {Token_tag.name = name; attributes = []; self_closing = false}

(* ---- close_element_with_implied ---- *)

let close_element_with_implied i l name =
  pop_implied ~except:name i l;
  (match Stack.current_element i.open_elements with
  | Some {element_name = `HTML, name'} when name' = name -> ()
  | Some {element_name = _, name'; location} ->
    i.report location (`Unmatched_start_tag name')
  | None ->
    i.report l (`Unmatched_end_tag name));
  close_element_ns i l name

let close_cell i l =
  pop_implied i l;
  (match Stack.current_element i.open_elements with
  | Some {element_name = `HTML, ("td" | "th")} -> ()
  | Some {element_name = _, name} ->
    i.report l (`Unmatched_end_tag name)
  | None ->
    i.report l (`Unmatched_end_tag ""));
  pop_until (function
    | {element_name = `HTML, ("td" | "th")} -> true
    | _ -> false) i l;
  pop_element i l

let close_current_p_element i l =
  if Stack.in_button_scope i.open_elements "p" then
    close_element_with_implied i l "p"

let close_preceding_tag i names l =
  let rec scan = function
    | [] -> ()
    | {element_name = (ns, name) as name'}::more ->
      if ns = `HTML && list_mem_string name names then
        close_element_with_implied i l name
      else if Element.is_special name' &&
        not @@ list_mem_qname name'
          [`HTML, "address"; `HTML, "div"; `HTML, "p"] then
        ()
      else
        scan more
  in
  scan !(i.open_elements)

(* ---- report_if_stack_has_other_than ---- *)

let report_if_stack_has_other_than i names _l =
  let rec iterate = function
    | [] -> ()
    | {element_name = ns, name; location} :: more ->
      if not (ns = `HTML && list_mem_string name names) then
        i.report location (`Unmatched_start_tag name);
      iterate more
  in
  iterate !(i.open_elements)

(* ---- emit_end ---- *)

let emit_end i l =
  pop_until (fun _ -> false) i l;
  flush_text i;
  i.done_ <- true

(* ---- reconstruct_active_formatting_elements ---- *)

let reconstruct_active_formatting_elements i =
  let rec get_prefix prefix = function
    | [] -> prefix, []
    | Active.Marker::_ as l -> prefix, l
    | Active.Element_ ({is_open = true}, _, _)::_ as l -> prefix, l
    | Active.Element_ ({is_open = false}, l, tag)::more ->
      get_prefix ((l, tag)::prefix) more
  in
  let to_reopen, remainder = get_prefix [] !(i.active_formatting_elements) in
  i.active_formatting_elements := remainder;

  begin match to_reopen with
  | [] -> ()
  | _::_ -> Subtree.enable i.subtree_buffer
  end;

  List.iter (fun (l, tag) -> push_and_emit ~formatting:true i l tag) to_reopen

(* ---- adoption_agency_algorithm ---- *)

let run_adoption_agency i l name =
  Subtree.enable i.subtree_buffer;
  flush_text i;
  let handled, errors =
    Subtree.adoption_agency_algorithm
      i.subtree_buffer i.active_formatting_elements l name
  in
  List.iter (fun (el, e) -> i.report el e) errors;
  if not handled then begin
    (* any_other_end_tag_in_body *)
    let rec close = function
      | [] -> ()
      | {element_name = (ns, name') as name''}::rest ->
        if ns = `HTML && name' = name then begin
          pop_implied ~except:name i l;
          pop_element i l
        end else if Element.is_special name'' then begin
          i.report l (`Unmatched_end_tag name)
        end else close rest
    in
    close !(i.open_elements)
  end

(* ---- select_in_body ---- *)

let select_in_body i l t next_mode =
  i.frameset_ok := false;
  reconstruct_active_formatting_elements i;
  push_and_emit i l t;
  i.current_mode <- next_mode

(* ---- reset_mode ---- *)

let rec reset_mode i =
  let rec iterate last = function
    | [e] when not last && i.the_context <> `Document ->
      begin match i.the_context with
      | `Document -> assert false
      | `Fragment name -> iterate true [{e with element_name = name}]
      end
    | {element_name = _, "select"}::ancestors ->
      let rec iterate' = function
        | [] -> in_select_mode
        | {element_name = _, "template"}::_ -> in_select_mode
        | {element_name = _, "table"}::_ -> in_select_in_table_mode
        | _::ancestors -> iterate' ancestors
      in
      i.current_mode <- iterate' ancestors
    | {element_name = _, ("tr" | "th")}::_::_ ->
      i.current_mode <- in_cell_mode
    | {element_name = _, "tr"}::_ ->
      i.current_mode <- in_row_mode
    | {element_name = _, ("tbody" | "thead" | "tfoot")}::_ ->
      i.current_mode <- in_table_body_mode
    | {element_name = _, "caption"}::_ ->
      i.current_mode <- in_caption_mode
    | {element_name = _, "colgroup"}::_ ->
      i.current_mode <- in_column_group_mode
    | {element_name = _, "table"}::_ ->
      i.current_mode <- in_table_mode
    | {element_name = _, "template"}::_ ->
      i.current_mode <-
        (match !(i.template_insertion_modes) with
        | [] -> initial_mode
        | mode::_ -> mode)
    | {element_name = _, "head"}::_ ->
      i.current_mode <- in_head_mode
    | {element_name = _, "body"}::_ ->
      i.current_mode <- in_body_mode
    | {element_name = _, "frameset"}::_ ->
      i.current_mode <- in_frameset_mode
    | {element_name = _, "html"}::_ ->
      i.current_mode <-
        (if !(i.head_seen) then after_head_mode else before_head_mode)
    | _::rest -> iterate last rest
    | [] -> i.current_mode <- in_body_mode
  in
  iterate false !(i.open_elements)

(* ---- Parser modes ---- *)

(* 8.2.5.4.1 initial_mode *)
and initial_mode i l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
    ()
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype d ->
    emit_and_set_mode i l (`Doctype d) before_html_mode
  | _ ->
    i.current_mode <- before_html_mode;
    before_html_mode i l token

(* 8.2.5.4.2 before_html_mode *)
and before_html_mode i l token =
  match token with
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
    ()
  | `Start ({name = "html"} as t) ->
    push_and_emit i l t;
    i.current_mode <- before_head_mode
  | `End {name}
      when not @@ list_mem_string name ["head"; "body"; "html"; "br"] ->
    i.report l (`Unmatched_end_tag name)
  | _ ->
    push_implicit i l "html";
    i.current_mode <- before_head_mode;
    before_head_mode i l token

(* 8.2.5.4.3 before_head_mode *)
and before_head_mode i l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
    ()
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "html" l token
  | `Start ({name = "head"} as t) ->
    i.head_seen := true;
    push_and_emit i l t;
    i.current_mode <- in_head_mode
  | `End {name}
      when not @@ list_mem_string name ["head"; "body"; "html"; "br"] ->
    i.report l (`Unmatched_end_tag name)
  | _ ->
    i.head_seen := true;
    push_implicit i l "head";
    i.current_mode <- in_head_mode;
    in_head_mode i l token

(* 8.2.5.4.4 in_head_mode *)
and in_head_mode i l token =
  in_head_mode_rules i in_head_mode l token

(* 8.2.5.4.4 in_head_mode_rules *)
and in_head_mode_rules i mode l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "head" l token
  | `Start ({name = "base" | "basefont" | "bgsound" | "link" | "meta"} as t) ->
    push_and_emit ~acknowledge:true i l t;
    pop_element i l;
    i.current_mode <- mode
  | `Start ({name = "title"} as t) ->
    push_and_emit i l t;
    set_tokenizer_state i `RCDATA;
    i.current_mode <- text_mode_fn mode
  | `Start ({name = "noframes" | "style"} as t) ->
    push_and_emit i l t;
    set_tokenizer_state i `RAWTEXT;
    i.current_mode <- text_mode_fn mode
  | `Start ({name = "noscript"} as t) ->
    push_and_emit i l t;
    i.current_mode <- in_head_noscript_mode
  | `Start ({name = "script"} as t) ->
    push_and_emit i l t;
    set_tokenizer_state i `Script_data;
    i.current_mode <- text_mode_fn mode
  | `End {name = "head"} ->
    pop_element i l;
    i.current_mode <- after_head_mode
  | `Start ({name = "template"} as t) ->
    Active.add_marker i.active_formatting_elements;
    i.frameset_ok := false;
    i.template_insertion_modes :=
      in_template_mode :: !(i.template_insertion_modes);
    push_and_emit i l t;
    i.current_mode <- in_template_mode
  | `End {name = "template"} ->
    if not @@ Stack.has i.open_elements "template" then
      i.report l (`Unmatched_end_tag "template")
    else begin
      Active.clear_until_marker i.active_formatting_elements;
      (match !(i.template_insertion_modes) with
      | [] -> ()
      | _::rest -> i.template_insertion_modes := rest);
      close_element_with_implied i l "template";
      reset_mode i
    end
  | `Start ({name = "head"} as t) ->
    i.report l (`Misnested_tag (t.name, "head", t.Token_tag.attributes))
  | `End {name} when not @@ list_mem_string name ["body"; "html"; "br"] ->
    i.report l (`Unmatched_end_tag name)
  | _ ->
    pop_element i l;
    i.current_mode <- after_head_mode;
    after_head_mode i l token

(* 8.2.5.4.5 in_head_noscript_mode *)
and in_head_noscript_mode i l token =
  match token with
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "noscript" l token
  | `End {name = "noscript"} ->
    pop_element i l;
    i.current_mode <- in_head_mode
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
  | `Comment _
  | `Start {name = "basefont" | "bgsound" | "link" | "meta" | "noframes" | "style"} ->
    in_head_mode_rules i in_head_noscript_mode l token
  | `Start ({name = "head" | "noscript"} as t) ->
    i.report l (`Misnested_tag (t.name, "noscript", t.Token_tag.attributes))
  | `End {name} when name <> "br" ->
    i.report l (`Unmatched_end_tag name)
  | _ ->
    i.report l (`Bad_content "noscript");
    pop_element i l;
    i.current_mode <- in_head_mode;
    in_head_mode i l token

(* 8.2.5.4.6 after_head_mode *)
and after_head_mode i l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "html" l token
  | `Start ({name = "body"} as t) ->
    i.frameset_ok := false;
    push_and_emit i l t;
    i.current_mode <- in_body_mode
  | `Start ({name = "frameset"} as t) ->
    push_and_emit i l t;
    i.current_mode <- in_frameset_mode
  | `Start ({name = "base" | "basefont" | "bgsound" | "link" | "meta" |
      "noframes" | "script" | "style" | "template" | "title"} as t) ->
    i.report l (`Misnested_tag (t.name, "html", t.Token_tag.attributes));
    in_head_mode_rules i after_head_mode l token
  | `End {name = "template"} ->
    in_head_mode_rules i after_head_mode l token
  | `Start {name = "head"} ->
    i.report l (`Bad_document "duplicate head element")
  | `End {name} when not @@ list_mem_string name ["body"; "html"; "br"] ->
    i.report l (`Unmatched_end_tag name)
  | `EOF when (i.the_context = `Fragment (`HTML, "html")
             || i.the_context = `Fragment (`HTML, "head")) ->
    emit_end i l
  | _ ->
    push_implicit i l "body";
    i.current_mode <- in_body_mode;
    in_body_mode i l token

(* 8.2.5.4.7 in_body_mode *)
and in_body_mode i l token =
  in_body_mode_rules i "body" l token

(* 8.2.5.4.7 in_body_mode_rules *)
and in_body_mode_rules i context_name l token =
  match token with
  | `Char 0 ->
    i.report l (`Bad_token ("U+0000", "body", "null"))
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
    reconstruct_active_formatting_elements i;
    Text.add i.text l c
  | `Char c ->
    i.frameset_ok := false;
    reconstruct_active_formatting_elements i;
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start ({name = "html"} as t) ->
    i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes))
  | `Start {name = "base" | "basefont" | "bgsound" | "link" | "meta" |
      "noframes" | "script" | "style" | "template" | "title"}
  | `End {name = "template"} ->
    in_head_mode_rules i i.current_mode l token
  | `Start ({name = "body"} as t) ->
    i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes))
  | `Start ({name = "frameset"} as t) ->
    i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes));
    (match !(i.open_elements) with
    | [_] -> ()
    | _ ->
      let rec second_is_body = function
        | [{element_name = `HTML, "body"}; _] -> true
        | [] -> false
        | _::more -> second_is_body more
      in
      if second_is_body !(i.open_elements) && !(i.frameset_ok) then begin
        pop_until
          (fun _ -> match !(i.open_elements) with [_] -> true | _ -> false)
          i l;
        push_and_emit i l t;
        i.current_mode <- in_frameset_mode
      end)
  | `EOF ->
    report_if_stack_has_other_than i
      ["dd"; "dt"; "li"; "p"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr";
       "body"; "html"] l;
    (match !(i.template_insertion_modes) with
    | [] -> emit_end i l
    | _ -> in_template_mode_rules i i.current_mode l token)
  | `End {name = "body"} ->
    if not @@ Stack.in_scope i.open_elements "body" then
      i.report l (`Unmatched_end_tag "body")
    else begin
      report_if_stack_has_other_than i
        ["dd"; "dt"; "li"; "optgroup"; "option"; "p"; "rb"; "rp"; "rt";
         "rtc"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr"; "body"; "html"] l;
      i.current_mode <- after_body_mode
    end
  | `End {name = "html"} ->
    if not @@ Stack.in_scope i.open_elements "body" then
      i.report l (`Unmatched_end_tag "html")
    else begin
      report_if_stack_has_other_than i
        ["dd"; "dt"; "li"; "optgroup"; "option"; "p"; "rb"; "rp"; "rt";
         "rtc"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr"; "body"; "html"] l;
      i.current_mode <- after_body_mode;
      after_body_mode i l token
    end
  | `Start ({name = "address" | "article" | "aside" | "blockquote" | "center" |
      "details" | "dialog" | "dir" | "div" | "dl" | "fieldset" |
      "figcaption" | "figure" | "footer" | "header" | "hgroup" | "main" |
      "nav" | "ol" | "p" | "section" | "summary" | "ul"} as t) ->
    close_current_p_element i l;
    push_and_emit i l t
  | `Start ({name = "h1" | "h2" | "h3" | "h4" | "h5" | "h6"} as t) ->
    close_current_p_element i l;
    (match Stack.current_element i.open_elements with
    | Some {element_name = `HTML, ("h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name')} ->
      i.report l (`Misnested_tag (t.name, name', t.Token_tag.attributes));
      pop_element i l
    | _ -> ());
    push_and_emit i l t
  | `Start ({name = "pre" | "listing"} as t) ->
    i.frameset_ok := false;
    close_current_p_element i l;
    push_and_emit i l t;
    (* skip initial newline - handled by setting skip_newline flag via tok_fn *)
    i.current_mode <- skip_newline_mode i.current_mode
  | `Start ({name = "form"} as t) ->
    if !(i.form_element_pointer) <> None &&
       not @@ Stack.has i.open_elements "template" then
      i.report l (`Misnested_tag (t.name, "form", t.Token_tag.attributes))
    else begin
      close_current_p_element i l;
      let in_template = Stack.has i.open_elements "template" in
      push_and_emit ~set_form_element_pointer:(not in_template) i l t
    end
  | `Start ({name = "li"} as t) ->
    i.frameset_ok := false;
    close_preceding_tag i ["li"] l;
    close_current_p_element i l;
    push_and_emit i l t
  | `Start ({name = "dd" | "dt"} as t) ->
    i.frameset_ok := false;
    close_preceding_tag i ["dd"; "dt"] l;
    close_current_p_element i l;
    push_and_emit i l t
  | `Start ({name = "plaintext"} as t) ->
    close_current_p_element i l;
    set_tokenizer_state i `PLAINTEXT;
    push_and_emit i l t
  | `Start ({name = "button"} as t) ->
    (if Stack.in_scope i.open_elements "button" then begin
      i.report l (`Misnested_tag (t.name, "button", t.Token_tag.attributes));
      close_element_with_implied i l "button"
    end);
    i.frameset_ok := false;
    reconstruct_active_formatting_elements i;
    push_and_emit i l t
  | `End {name = "address" | "article" | "aside" | "blockquote" | "button" |
      "center" | "details" | "dialog" | "dir" | "div" | "dl" | "fieldset" |
      "figcaption" | "figure" | "footer" | "header" | "hgroup" | "listing" |
      "main" | "nav" | "ol" | "pre" | "section" | "summary" | "ul" as name} ->
    if not @@ Stack.in_scope i.open_elements name then
      i.report l (`Unmatched_end_tag name)
    else
      close_element_with_implied i l name
  | `End {name = "form"} ->
    if not @@ Stack.has i.open_elements "template" then begin
      let form_element = !(i.form_element_pointer) in
      i.form_element_pointer := None;
      match form_element with
      | Some element when Stack.target_in_scope i.open_elements element ->
        pop_implied i l;
        (match Stack.current_element i.open_elements with
        | Some element' when element' == element ->
          pop_element i l
        | _ ->
          i.report element.location (`Unmatched_start_tag "form");
          pop_until (fun element' -> element' == element) i l;
          pop_element i l)
      | _ ->
        i.report l (`Unmatched_end_tag "form")
    end else begin
      if not @@ Stack.in_scope i.open_elements "form" then
        i.report l (`Unmatched_end_tag "form")
      else
        close_element_with_implied i l "form"
    end
  | `End {name = "p"} ->
    if not @@ Stack.in_button_scope i.open_elements "p" then begin
      i.report l (`Unmatched_end_tag "p");
      push_implicit i l "p";
      close_element_with_implied i l "p"
    end else
      close_element_with_implied i l "p"
  | `End {name = "li"} ->
    if not @@ Stack.in_list_item_scope i.open_elements "li" then
      i.report l (`Unmatched_end_tag "li")
    else
      close_element_with_implied i l "li"
  | `End {name = "dd" | "dt" as name} ->
    if not @@ Stack.in_scope i.open_elements name then
      i.report l (`Unmatched_end_tag name)
    else
      close_element_with_implied i l name
  | `End {name = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name} ->
    if not @@ Stack.one_in_scope i.open_elements
        ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] then
      i.report l (`Unmatched_end_tag name)
    else begin
      pop_implied i l;
      (match Stack.current_element i.open_elements with
      | Some {element_name = `HTML, name'}
          when list_mem_string name' ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] -> ()
      | _ -> i.report l (`Unmatched_end_tag name));
      pop_until_and_raise_errors i ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] l
    end
  | `Start ({name = "a"} as t) ->
    (match Active.has_before_marker i.active_formatting_elements "a" with
    | None -> ()
    | Some existing ->
      i.report l (`Misnested_tag (t.name, "a", t.Token_tag.attributes));
      run_adoption_agency i l "a";
      Stack.remove i.open_elements existing;
      Active.remove i.active_formatting_elements existing);
    Subtree.enable i.subtree_buffer;
    reconstruct_active_formatting_elements i;
    push_and_emit ~formatting:true i l t
  | `Start ({name = "b" | "big" | "code" | "em" | "font" | "i" | "s" | "small" |
      "strike" | "strong" | "tt" | "u"} as t) ->
    Subtree.enable i.subtree_buffer;
    reconstruct_active_formatting_elements i;
    push_and_emit ~formatting:true i l t
  | `Start ({name = "nobr"} as t) ->
    Subtree.enable i.subtree_buffer;
    reconstruct_active_formatting_elements i;
    (if Stack.in_scope i.open_elements "nobr" then begin
      i.report l (`Misnested_tag (t.name, "nobr", t.Token_tag.attributes));
      run_adoption_agency i l "nobr";
      reconstruct_active_formatting_elements i
    end);
    push_and_emit ~formatting:true i l t
  | `End {name = "a" | "b" | "big" | "code" | "em" | "font" | "i" | "nobr" |
      "s" | "small" | "strike" | "strong" | "tt" | "u" as name} ->
    run_adoption_agency i l name
  | `Start ({name = "applet" | "marquee" | "object"} as t) ->
    i.frameset_ok := false;
    reconstruct_active_formatting_elements i;
    Active.add_marker i.active_formatting_elements;
    push_and_emit i l t
  | `End {name = "applet" | "marquee" | "object" as name} ->
    if not @@ Stack.in_scope i.open_elements name then
      i.report l (`Unmatched_end_tag name)
    else begin
      Active.clear_until_marker i.active_formatting_elements;
      close_element_with_implied i l name
    end
  | `Start ({name = "table"} as t) ->
    i.frameset_ok := false;
    close_current_p_element i l;
    push_and_emit i l t;
    i.current_mode <- in_table_mode
  | `End {name = "br"} ->
    i.report l (`Unmatched_end_tag "br");
    in_body_mode_rules i context_name l
      (`Start {Token_tag.name = "br"; attributes = []; self_closing = false})
  | `Start ({name = "area" | "br" | "embed" | "img" | "keygen" | "wbr"} as t) ->
    i.frameset_ok := false;
    reconstruct_active_formatting_elements i;
    push_and_emit ~acknowledge:true i l t;
    pop_element i l
  | `Start ({name = "input"} as t) ->
    if Element.is_not_hidden t then i.frameset_ok := false;
    reconstruct_active_formatting_elements i;
    push_and_emit ~acknowledge:true i l t;
    pop_element i l
  | `Start ({name = "param" | "source" | "track"} as t) ->
    push_and_emit ~acknowledge:true i l t;
    pop_element i l
  | `Start ({name = "hr"} as t) ->
    i.frameset_ok := false;
    close_current_p_element i l;
    push_and_emit ~acknowledge:true i l t;
    pop_element i l
  | `Start ({name = "image"} as t) ->
    i.report l (`Bad_token ("image", "tag", "should be 'img'"));
    in_body_mode_rules i context_name l
      (`Start {t with Token_tag.name = "img"})
  | `Start ({name = "textarea"} as t) ->
    i.frameset_ok := false;
    push_and_emit i l t;
    set_tokenizer_state i `RCDATA;
    i.current_mode <- skip_newline_then_text_mode i.current_mode
  | `Start {name = "xmp"} ->
    i.frameset_ok := false;
    close_current_p_element i l;
    reconstruct_active_formatting_elements i;
    set_tokenizer_state i `RAWTEXT;
    i.current_mode <- text_mode_fn i.current_mode
  | `Start ({name = "iframe"} as t) ->
    i.frameset_ok := false;
    push_and_emit i l t;
    set_tokenizer_state i `RAWTEXT;
    i.current_mode <- text_mode_fn i.current_mode
  | `Start ({name = "noembed"} as t) ->
    push_and_emit i l t;
    set_tokenizer_state i `RAWTEXT;
    i.current_mode <- text_mode_fn i.current_mode
  | `Start ({name = "select"} as t) ->
    select_in_body i l t in_select_mode
  | `Start ({name = "optgroup" | "option"} as t) ->
    (if Stack.current_element_is i.open_elements ["option"] then
      pop_element i l);
    reconstruct_active_formatting_elements i;
    push_and_emit i l t
  | `Start ({name = "rb" | "rtc"} as t) ->
    (if Stack.in_scope i.open_elements "ruby" then begin
      pop_implied i l;
      if not @@ Stack.current_element_is i.open_elements ["ruby"] then
        i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes))
    end else
      i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes)));
    push_and_emit i l t
  | `Start ({name = "rp" | "rt"} as t) ->
    (if Stack.in_scope i.open_elements "ruby" then begin
      pop_implied ~except:"rtc" i l;
      if not @@ Stack.current_element_is i.open_elements ["ruby"; "rtc"] then
        i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes))
    end else
      i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes)));
    push_and_emit i l t
  | `Start ({name = "math"} as t) ->
    reconstruct_active_formatting_elements i;
    push_and_emit ~acknowledge:true ~namespace:`MathML i l t;
    if t.self_closing then pop_element i l
  | `Start ({name = "svg"} as t) ->
    reconstruct_active_formatting_elements i;
    push_and_emit ~acknowledge:true ~namespace:`SVG i l t;
    if t.self_closing then pop_element i l
  | `Start ({name = "caption" | "col" | "colgroup" | "frame" | "head" |
      "tbody" | "td" | "tfoot" | "th" | "thead" | "tr"} as t) ->
    i.report l (`Misnested_tag (t.name, context_name, t.Token_tag.attributes))
  | `Start t ->
    reconstruct_active_formatting_elements i;
    push_and_emit i l t
  | `End {name} ->
    any_other_end_tag_in_body i l name

(* Part of 8.2.5.4.7 *)
and any_other_end_tag_in_body i l name =
  let rec close = function
    | [] -> ()
    | {element_name = (ns, name') as name''}::rest ->
      if ns = `HTML && name' = name then begin
        pop_implied ~except:name i l;
        pop_element i l
      end else if Element.is_special name'' then
        i.report l (`Unmatched_end_tag name)
      else close rest
  in
  close !(i.open_elements)

(* text_mode: used for RCDATA/RAWTEXT/script content *)
and text_mode_fn original_mode i l token =
  match token with
  | `Char c ->
    Text.add i.text l c
  | `EOF ->
    i.report l (`Unexpected_eoi "content");
    pop_element i l;
    i.current_mode <- original_mode;
    original_mode i l token
  | `End _ ->
    pop_element i l;
    i.current_mode <- original_mode
  | _ ->
    ()  (* ignore other tokens in text mode *)

(* skip_newline_mode: skip one leading newline, then stay in current mode *)
and skip_newline_mode current_mode i l token =
  match token with
  | `Char 0x000A ->
    i.current_mode <- current_mode
  | _ ->
    i.current_mode <- current_mode;
    current_mode i l token

(* skip_newline_then_text_mode: for textarea - skip newline then enter text_mode *)
and skip_newline_then_text_mode original_mode i l token =
  let tm = text_mode_fn original_mode in
  match token with
  | `Char 0x000A ->
    i.current_mode <- tm
  | _ ->
    i.current_mode <- tm;
    tm i l token

(* 8.2.5.4.9 in_table_mode *)
and in_table_mode i l token =
  in_table_mode_rules i in_table_mode l token

and in_table_mode_rules i mode l token =
  match token with
  | `Char _ when Stack.current_element_is i.open_elements
        ["table"; "tbody"; "tfoot"; "thead"; "tr"] ->
    (* start in_table_text_mode *)
    i.table_text_only_space <- true;
    i.table_text_chars <- [(l, token)];
    i.table_text_return_mode <- mode;
    i.current_mode <- in_table_text_mode
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start ({name = "caption"} as t) ->
    pop_to_table_context i l;
    Active.add_marker i.active_formatting_elements;
    push_and_emit i l t;
    i.current_mode <- in_caption_mode
  | `Start ({name = "colgroup"} as t) ->
    pop_to_table_context i l;
    push_and_emit i l t;
    i.current_mode <- in_column_group_mode
  | `Start {name = "col"} ->
    pop_to_table_context i l;
    push_implicit i l "colgroup";
    i.current_mode <- in_column_group_mode;
    in_column_group_mode i l token
  | `Start ({name = "tbody" | "tfoot" | "thead"} as t) ->
    pop_to_table_context i l;
    push_and_emit i l t;
    i.current_mode <- in_table_body_mode
  | `Start {name = "td" | "th" | "tr"} ->
    pop_to_table_context i l;
    push_implicit i l "tbody";
    i.current_mode <- in_table_body_mode;
    in_table_body_mode i l token
  | `Start ({name = "table"} as t) ->
    i.report l (`Misnested_tag (t.name, "table", t.Token_tag.attributes));
    if Stack.has i.open_elements "table" then begin
      i.current_mode <- mode;
      close_element_ns i l "table";
      reset_mode i;
      i.current_mode i l token
    end
  | `End {name = "table"} ->
    if not @@ Stack.in_table_scope i.open_elements "table" then
      i.report l (`Unmatched_end_tag "table")
    else begin
      close_element_ns i l "table";
      reset_mode i
    end
  | `End {name = "body" | "caption" | "col" | "colgroup" | "html" | "tbody" |
      "td" | "tfoot" | "th" | "thead" | "tr" as name} ->
    i.report l (`Unmatched_end_tag name)
  | `Start {name = "style" | "script" | "template"}
  | `End {name = "template"} ->
    in_head_mode_rules i mode l token
  | `Start ({name = "input"} as t) when Element.is_not_hidden t ->
    i.report l (`Misnested_tag (t.name, "table", t.Token_tag.attributes));
    push_and_emit ~acknowledge:true i l t;
    pop_element i l
  | `Start ({name = "form"} as t) ->
    i.report l (`Misnested_tag (t.name, "table", t.Token_tag.attributes));
    push_and_emit i l t;
    pop_element i l
  | `EOF ->
    in_body_mode_rules i "table" l token
  | _ ->
    anything_else_in_table i mode l token

and anything_else_in_table i mode l token =
  i.report l (`Bad_content "table");
  in_body_mode_rules i "table" l token;
  (* Note: after in_body_mode_rules, i.current_mode may have changed;
     we need to preserve the table mode for next iteration if not in body *)
  ignore mode

(* 8.2.5.4.10 in_table_text_mode *)
and in_table_text_mode i l token =
  match token with
  | `Char 0 ->
    i.report l (`Bad_token ("U+0000", "table", "null"))
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
    i.table_text_chars <- (l, token) :: i.table_text_chars
  | `Char _ ->
    i.table_text_only_space <- false;
    i.table_text_chars <- (l, token) :: i.table_text_chars
  | _ ->
    (* flush table text chars *)
    let chars = List.rev i.table_text_chars in
    i.table_text_chars <- [];
    let return_mode = i.table_text_return_mode in
    if not i.table_text_only_space then begin
      List.iter (fun (l', tok') ->
        anything_else_in_table i return_mode l' tok') chars
    end else begin
      List.iter (function
        | l', `Char c -> Text.add i.text l' c
        | _ -> ()) chars
    end;
    i.current_mode <- return_mode;
    return_mode i l token

(* 8.2.5.4.11 in_caption_mode *)
and in_caption_mode i l token =
  match token with
  | `End {name = "caption"} ->
    if not @@ Stack.in_table_scope i.open_elements "caption" then
      i.report l (`Unmatched_end_tag "caption")
    else begin
      Active.clear_until_marker i.active_formatting_elements;
      close_element_with_implied i l "caption";
      i.current_mode <- in_table_mode
    end
  | `Start ({name = "caption" | "col" | "colgroup" | "tbody" | "td" | "tfoot" |
      "th" | "thead" | "tr"} as t) ->
    i.report l (`Misnested_tag (t.name, "caption", t.Token_tag.attributes));
    if Stack.in_table_scope i.open_elements "caption" then begin
      Active.clear_until_marker i.active_formatting_elements;
      close_element_ns i l "caption";
      i.current_mode <- in_table_mode;
      in_table_mode i l token
    end
  | `End {name = "table"} ->
    i.report l (`Unmatched_end_tag "table");
    if Stack.in_table_scope i.open_elements "caption" then begin
      Active.clear_until_marker i.active_formatting_elements;
      close_element_ns i l "caption";
      i.current_mode <- in_table_mode;
      in_table_mode i l token
    end
  | `End {name = "body" | "col" | "colgroup" | "html" | "tbody" | "td" |
      "tfoot" | "th" | "thead" | "tr" as name} ->
    i.report l (`Unmatched_end_tag name)
  | `Start ({name = "select"} as t) ->
    select_in_body i l t in_select_in_table_mode
  | _ ->
    in_body_mode_rules i "caption" l token

(* 8.2.5.4.12 in_column_group_mode *)
and in_column_group_mode i l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "colgroup" l token
  | `Start ({name = "col"} as t) ->
    push_and_emit ~acknowledge:true i l t;
    pop_element i l
  | `End {name = "colgroup"} ->
    if not @@ Stack.current_element_is i.open_elements ["colgroup"] then
      i.report l (`Unmatched_end_tag "colgroup")
    else begin
      pop_element i l;
      i.current_mode <- in_table_mode
    end
  | `End {name = "col"} ->
    i.report l (`Unmatched_end_tag "col")
  | `Start {name = "template"}
  | `End {name = "template"} ->
    in_head_mode_rules i in_column_group_mode l token
  | `EOF ->
    in_body_mode_rules i "colgroup" l token
  | _ ->
    if not @@ Stack.current_element_is i.open_elements ["colgroup"] then begin
      i.report l (`Bad_content "colgroup");
      i.current_mode <- in_table_mode
    end else begin
      pop_element i l;
      i.current_mode <- in_table_mode;
      in_table_mode i l token
    end

(* 8.2.5.4.13 in_table_body_mode *)
and in_table_body_mode i l token =
  match token with
  | `Start ({name = "tr"} as t) ->
    pop_to_table_body_context i l;
    push_and_emit i l t;
    i.current_mode <- in_row_mode
  | `Start ({name = "th" | "td"} as t) ->
    i.report l (`Misnested_tag (t.name, "table", t.Token_tag.attributes));
    pop_to_table_body_context i l;
    push_implicit i l "tr";
    i.current_mode <- in_row_mode;
    in_row_mode i l token
  | `End {name = "tbody" | "tfoot" | "thead" as name} ->
    if not @@ Stack.in_table_scope i.open_elements name then
      i.report l (`Unmatched_end_tag name)
    else begin
      pop_to_table_body_context i l;
      pop_element i l;
      i.current_mode <- in_table_mode
    end
  | `Start ({name = "caption" | "col" | "colgroup" | "tbody" | "tfoot" | "thead"} as t) ->
    if not @@ Stack.one_in_table_scope i.open_elements ["tbody"; "thead"; "tfoot"] then
      i.report l (`Misnested_tag (t.name, "table", t.Token_tag.attributes))
    else begin
      pop_to_table_body_context i l;
      pop_element i l;
      i.current_mode <- in_table_mode;
      in_table_mode i l token
    end
  | `End {name = "table"} ->
    if not @@ Stack.one_in_table_scope i.open_elements ["tbody"; "thead"; "tfoot"] then
      i.report l (`Unmatched_end_tag "table")
    else begin
      pop_to_table_body_context i l;
      pop_element i l;
      i.current_mode <- in_table_mode;
      in_table_mode i l token
    end
  | `End {name = "body" | "caption" | "col" | "colgroup" | "html" | "td" |
      "th" | "tr" as name} ->
    i.report l (`Unmatched_end_tag name)
  | _ ->
    in_table_mode_rules i in_table_body_mode l token

(* 8.2.5.4.14 in_row_mode *)
and in_row_mode i l token =
  match token with
  | `Start ({name = "th" | "td"} as t) ->
    Active.add_marker i.active_formatting_elements;
    pop_to_table_row_context i l;
    push_and_emit i l t;
    i.current_mode <- in_cell_mode
  | `End {name = "tr"} ->
    if not @@ Stack.in_table_scope i.open_elements "tr" then
      i.report l (`Unmatched_end_tag "tr")
    else begin
      pop_to_table_row_context i l;
      pop_element i l;
      i.current_mode <- in_table_body_mode
    end
  | `Start {name = "caption" | "col" | "colgroup" | "tbody" | "tfoot" |
      "thead" | "tr"}
  | `End {name = "table"} ->
    if not @@ Stack.in_table_scope i.open_elements "tr" then begin
      (match token with
      | `Start t ->
        i.report l (`Misnested_tag (t.name, "tr", t.Token_tag.attributes))
      | `End {name} ->
        i.report l (`Unmatched_end_tag name)
      | _ -> ())
    end else begin
      pop_to_table_row_context i l;
      pop_element i l;
      i.current_mode <- in_table_body_mode;
      in_table_body_mode i l token
    end
  | `End {name = "tbody" | "tfoot" | "thead" as name} ->
    if not @@ Stack.in_table_scope i.open_elements name then
      i.report l (`Unmatched_end_tag name)
    else if not @@ Stack.in_table_scope i.open_elements "tr" then ()
    else begin
      pop_to_table_row_context i l;
      pop_element i l;
      i.current_mode <- in_table_body_mode;
      in_table_body_mode i l token
    end
  | `End {name = "body" | "caption" | "col" | "colgroup" | "html" | "td" |
      "th" as name} ->
    i.report l (`Unmatched_end_tag name)
  | _ ->
    in_table_mode_rules i in_row_mode l token

(* 8.2.5.4.15 in_cell_mode *)
and in_cell_mode i l token =
  match token with
  | `End {name = "td" | "th" as name} ->
    if not @@ Stack.in_table_scope i.open_elements name then
      i.report l (`Unmatched_end_tag name)
    else begin
      close_element_with_implied i l name;
      Active.clear_until_marker i.active_formatting_elements;
      i.current_mode <- in_row_mode
    end
  | `Start ({name = "caption" | "col" | "colgroup" | "tbody" | "td" | "tfoot" |
      "th" | "thead" | "tr"} as t) ->
    if not @@ Stack.one_in_table_scope i.open_elements ["td"; "th"] then
      i.report l (`Misnested_tag (t.name, "td/th", t.Token_tag.attributes))
    else begin
      close_cell i l;
      Active.clear_until_marker i.active_formatting_elements;
      i.current_mode <- in_row_mode;
      in_row_mode i l token
    end
  | `End {name = "body" | "caption" | "col" | "colgroup" | "html" as name} ->
    i.report l (`Unmatched_end_tag name)
  | `End {name = "table" | "tbody" | "tfoot" | "thead" | "tr" as name} ->
    if not @@ Stack.in_table_scope i.open_elements name then
      i.report l (`Unmatched_end_tag name)
    else begin
      close_cell i l;
      Active.clear_until_marker i.active_formatting_elements;
      i.current_mode <- in_row_mode;
      in_row_mode i l token
    end
  | `Start ({name = "select"} as t) ->
    select_in_body i l t in_select_in_table_mode
  | _ ->
    in_body_mode_rules i "td" l token

(* 8.2.5.4.16 in_select_mode *)
and in_select_mode i l token =
  in_select_mode_rules i in_select_mode l token

and in_select_mode_rules i mode l token =
  match token with
  | `Char 0 ->
    i.report l (`Bad_token ("U+0000", "select", "null"))
  | `Char c ->
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "select" l token
  | `Start ({name = "option"} as t) ->
    (if Stack.current_element_is i.open_elements ["option"] then
      pop_element i l);
    push_and_emit i l t
  | `Start ({name = "optgroup"} as t) ->
    (if Stack.current_element_is i.open_elements ["option"] then
      pop_element i l);
    (if Stack.current_element_is i.open_elements ["optgroup"] then
      pop_element i l);
    push_and_emit i l t
  | `End {name = "optgroup"} ->
    (match !(i.open_elements) with
    | {element_name = `HTML, "option"} ::
        {element_name = `HTML, "optgroup"} :: _ ->
      pop_element i l
    | _ -> ());
    if Stack.current_element_is i.open_elements ["optgroup"] then
      pop_element i l
    else
      i.report l (`Unmatched_end_tag "optgroup")
  | `End {name = "option"} ->
    if Stack.current_element_is i.open_elements ["option"] then
      pop_element i l
    else
      i.report l (`Unmatched_end_tag "option")
  | `End {name = "select"} ->
    if not @@ Stack.in_select_scope i.open_elements "select" then
      i.report l (`Unmatched_end_tag "select")
    else begin
      close_element_ns i l "select";
      reset_mode i
    end
  | `Start ({name = "select"} as t) ->
    i.report l (`Misnested_tag (t.name, "select", t.Token_tag.attributes));
    close_element_ns i l "select";
    reset_mode i
  | `Start ({name = "input" | "keygen" | "textarea"} as t) ->
    i.report l (`Misnested_tag (t.name, "select", t.Token_tag.attributes));
    if Stack.in_select_scope i.open_elements "select" then begin
      close_element_ns i l "select";
      reset_mode i;
      i.current_mode i l token
    end
  | `Start {name = "script" | "template"}
  | `End {name = "template"} ->
    in_head_mode_rules i mode l token
  | `EOF ->
    in_body_mode_rules i "select" l token
  | _ ->
    i.report l (`Bad_content "select")

(* 8.2.5.4.17 in_select_in_table_mode *)
and in_select_in_table_mode i l token =
  match token with
  | `Start ({name = "caption" | "table" | "tbody" | "tfoot" | "thead" | "tr" |
      "td" | "th"} as t) ->
    i.report l (`Misnested_tag (t.name, "table", t.Token_tag.attributes));
    close_element_ns i l "select";
    reset_mode i;
    i.current_mode i l token
  | `End {name = "caption" | "table" | "tbody" | "tfoot" | "thead" | "tr" |
      "td" | "th" as name} ->
    i.report l (`Unmatched_end_tag "name");
    if Stack.in_table_scope i.open_elements name then begin
      close_element_ns i l "select";
      reset_mode i;
      i.current_mode i l token
    end
  | _ ->
    in_select_mode_rules i in_select_in_table_mode l token

(* 8.2.5.4.18 in_template_mode *)
and in_template_mode i l token =
  in_template_mode_rules i in_template_mode l token

and in_template_mode_rules i mode l token =
  match token with
  | `Char _ | `Comment _ | `Doctype _ ->
    in_body_mode_rules i "template" l token
  | `Start {name = "base" | "basefont" | "bgsound" | "link" | "meta" |
      "noframes" | "script" | "style" | "template" | "title"}
  | `End {name = "template"} ->
    in_head_mode_rules i mode l token
  | `Start {name = "caption" | "colgroup" | "tbody" | "tfoot" | "thead"} ->
    (match !(i.template_insertion_modes) with
    | [] -> ()
    | _::rest -> i.template_insertion_modes := rest);
    i.template_insertion_modes := in_table_mode :: !(i.template_insertion_modes);
    i.current_mode <- in_table_mode;
    in_table_mode i l token
  | `Start {name = "col"} ->
    (match !(i.template_insertion_modes) with
    | [] -> ()
    | _::rest -> i.template_insertion_modes := rest);
    i.template_insertion_modes := in_column_group_mode :: !(i.template_insertion_modes);
    i.current_mode <- in_column_group_mode;
    in_column_group_mode i l token
  | `Start {name = "tr"} ->
    (match !(i.template_insertion_modes) with
    | [] -> ()
    | _::rest -> i.template_insertion_modes := rest);
    i.template_insertion_modes := in_table_body_mode :: !(i.template_insertion_modes);
    i.current_mode <- in_table_body_mode;
    in_table_body_mode i l token
  | `Start {name = "td" | "th"} ->
    (match !(i.template_insertion_modes) with
    | [] -> ()
    | _::rest -> i.template_insertion_modes := rest);
    i.template_insertion_modes := in_row_mode :: !(i.template_insertion_modes);
    i.current_mode <- in_row_mode;
    in_row_mode i l token
  | `Start _ ->
    (match !(i.template_insertion_modes) with
    | [] -> ()
    | _::rest -> i.template_insertion_modes := rest);
    i.template_insertion_modes := in_body_mode :: !(i.template_insertion_modes);
    i.current_mode <- in_body_mode;
    in_body_mode i l token
  | `End {name} ->
    i.report l (`Unmatched_end_tag name)
  | `EOF ->
    if not @@ Stack.has i.open_elements "template" then
      emit_end i l
    else begin
      i.report l (`Unmatched_end_tag "template");
      Active.clear_until_marker i.active_formatting_elements;
      (match !(i.template_insertion_modes) with
      | [] -> ()
      | _::rest -> i.template_insertion_modes := rest);
      close_element_ns i l "template";
      reset_mode i;
      i.current_mode i l token
    end

(* 8.2.5.4.19 after_body_mode *)
and after_body_mode i l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
    in_body_mode_rules i "html" l token
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "html" l token
  | `End {name = "html"} ->
    i.current_mode <- after_after_body_mode
  | `EOF ->
    emit_end i l
  | _ ->
    i.report l (`Bad_document "content after body");
    i.current_mode <- in_body_mode;
    in_body_mode i l token

(* 8.2.5.4.20 in_frameset_mode *)
and in_frameset_mode i l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "frameset" l token
  | `Start ({name = "frameset"} as t) ->
    push_and_emit i l t
  | `End {name = "frameset"} ->
    if Stack.current_element_is i.open_elements ["html"] then
      i.report l (`Unmatched_end_tag "frameset")
    else begin
      pop_element i l;
      if not @@ Stack.current_element_is i.open_elements ["frameset"] then
        i.current_mode <- after_frameset_mode
    end
  | `Start ({name = "frame"} as t) ->
    push_and_emit ~acknowledge:true i l t;
    pop_element i l
  | `Start {name = "noframes"} ->
    in_head_mode_rules i in_frameset_mode l token
  | `EOF ->
    if not @@ Stack.current_element_is i.open_elements ["html"] then
      i.report l (`Unexpected_eoi "frameset");
    emit_end i l
  | _ ->
    i.report l (`Bad_content "frameset")

(* 8.2.5.4.21 after_frameset_mode *)
and after_frameset_mode i l token =
  match token with
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start {name = "html"} ->
    in_body_mode_rules i "html" l token
  | `End {name = "html"} ->
    close_element_ns i l "html";
    i.current_mode <- after_after_frameset_mode
  | `Start {name = "noframes"} ->
    in_head_mode_rules i after_frameset_mode l token
  | `EOF ->
    emit_end i l
  | _ ->
    i.report l (`Bad_content "html")

(* 8.2.5.4.22 after_after_body_mode *)
and after_after_body_mode i l token =
  match token with
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
  | `Start {name = "html"} ->
    in_body_mode_rules i "html" l token
  | `EOF ->
    emit_end i l
  | _ ->
    i.report l (`Bad_content "html");
    i.current_mode <- in_body_mode;
    in_body_mode i l token

(* 8.2.5.4.23 after_after_frameset_mode *)
and after_after_frameset_mode i l token =
  match token with
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
  | `Start {name = "html"} ->
    in_body_mode_rules i "html" l token
  | `EOF ->
    emit_end i l
  | `Start {name = "noframes"} ->
    in_head_mode_rules i after_after_frameset_mode l token
  | _ ->
    i.report l (`Bad_content "html")

(* 8.2.5.5 foreign_content *)
and foreign_start_tag i l tag =
  let namespace =
    match Stack.adjusted_current_element i.the_context i.open_elements with
    | None -> `HTML
    | Some {element_name = ns, _} -> ns
  in
  push_and_emit ~acknowledge:true ~namespace i l tag;
  if tag.self_closing then pop_element i l

and is_html_font_tag tag =
  tag.Token_tag.attributes |> List.exists (function
    | ("color" | "face" | "size"), _ -> true
    | _ -> false)

and foreign_content i l token =
  match token with
  | `Char 0 ->
    i.report l (`Bad_token ("U+0000", "foreign content", "null"));
    Text.add i.text l Common.u_rep
  | `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
    Text.add i.text l c
  | `Char c ->
    i.frameset_ok := false;
    Text.add i.text l c
  | `Comment s ->
    emit_signal i l (`Comment s)
  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be first")
  | `Start ({name = "b" | "big" | "blockquote" | "body" | "br" | "center" |
      "code" | "dd" | "div" | "dl" | "dt" | "em" | "embed" | "font" | "h1" |
      "h2" | "h3" | "h4" | "h5" | "h6" | "head" | "hr" | "i" | "img" | "li" |
      "listing" | "main" | "meta" | "nobr" | "ol" | "p" | "pre" | "ruby" |
      "s" | "small" | "span" | "strong" | "strike" | "sub" | "sup" |
      "table" | "tt" | "u" | "ul" | "var" as name} as t) ->
    if name = "font" && not @@ is_html_font_tag t then
      foreign_start_tag i l t
    else begin
      i.report l (`Misnested_tag (t.name, "xml tag", t.Token_tag.attributes));
      pop_element i l;
      pop_until (function
        | {element_name = `HTML, _} -> true
        | {is_html_integration_point = true} -> true
        | {element_name} ->
          Foreign.is_mathml_text_integration_point element_name)
        i l;
      i.current_mode i l token
    end
  | `Start t ->
    foreign_start_tag i l t
  | `End {name = "script"}
      when (match Stack.current_element i.open_elements with
            | Some {element_name = `SVG, "script"} -> true
            | _ -> false) ->
    pop_element i l
  | `End {name} ->
    (match Stack.current_element i.open_elements with
    | Some {element_name = _, name'} when String.lowercase_ascii name' = name -> ()
    | _ -> i.report l (`Unmatched_end_tag name));
    let rec scan = function
      | [] -> ()
      | {element_name = ns, name'}::_
          when String.lowercase_ascii name' = name ->
        close_element_ns ~ns i l name
      | {element_name = `HTML, _}::_ ->
        (* force html: just call current mode with same token *)
        i.current_mode i l token
      | _::rest -> scan rest
    in
    scan !(i.open_elements)
  | `EOF ->
    i.current_mode i l token

(* ---- make and next_signal ---- *)

let make ~report ?context decoder =
  let the_context =
    match context with
    | None | Some `Document -> `Document
    | Some (`Fragment name) ->
      let name = String.lowercase_ascii name in
      let ns = match name with
        | "svg" -> `SVG
        | "math" -> `MathML
        | _ -> `HTML
      in
      `Fragment (ns, name)
  in

  let initial_tok_state = match the_context with
    | `Fragment (`HTML, ("title" | "textarea")) -> rcdata_state
    | `Fragment (`HTML, ("style" | "xmp" | "iframe" | "noembed" | "noframes")) ->
      rawtext_state
    | `Fragment (`HTML, "script") -> script_data_state
    | `Fragment (`HTML, "plaintext") -> plaintext_state
    | _ -> data_state
  in

  let open_elements = Stack.create () in

  begin match the_context with
  | `Fragment _ ->
    let notional_root =
      Element.create ~suppress:true (`HTML, "html") (1, 1) in
    open_elements := [notional_root]
  | `Document -> ()
  end;

  let template_modes = ref [] in

  begin match the_context with
  | `Fragment (`HTML, "template") ->
    template_modes := [in_template_mode]
  | _ -> ()
  end;

  let head_seen = ref (match context with
    | Some (`Fragment ("body" | "frameset")) -> true
    | _ -> false) in

  let subtree_buffer = Subtree.create open_elements in
  let active = Active.create () in

  let initial_mode_fn =
    match the_context with
    | `Fragment _ ->
      (* Will be set after i is created by calling reset_mode *)
      in_body_mode  (* placeholder *)
    | `Document -> initial_mode
  in

  let dummy_mode _i _l _tok = () in

  let i = {
    decoder;
    c         = -2;
    cr        = false;
    line      = 1;
    col       = 1;
    first_char = true;
    report;
    pushback  = [];
    tok_fn    = initial_tok_state;
    foreign   = (fun () -> false);
    last_start_tag = None;
    tag_name_buf   = Buffer.create 32;
    attr_name_buf  = Buffer.create 32;
    attr_val_buf   = Buffer.create 256;
    is_start_tag     = true;
    self_closing_tag = false;
    pending_attrs    = [];
    current_mode = initial_mode_fn;
    the_context;
    open_elements;
    active_formatting_elements = active;
    subtree_buffer;
    template_insertion_modes = template_modes;
    frameset_ok = ref true;
    head_seen;
    form_element_pointer = ref None;
    table_text_only_space = true;
    table_text_chars = [];
    table_text_return_mode = dummy_mode;
    prepend   = [];
    queue     = Queue.create ();
    done_     = false;
    text      = Text.prepare ();
    mode      = (fun i l tok ->
      (* Foreign content check (mirrors dispatch in html_parser.ml) *)
      let is_foreign =
        match Stack.adjusted_current_element i.the_context i.open_elements, tok with
        | None, _ -> false
        | Some {element_name = `HTML, _}, _ -> false
        | Some {element_name}, `Start {name}
            when Foreign.is_mathml_text_integration_point element_name
            && name <> "mglyph" && name <> "malignmark" -> false
        | Some {element_name = `MathML, "annotation-xml"}, `Start {name = "svg"} -> false
        | Some {is_html_integration_point = true}, `Start _ -> false
        | Some {is_html_integration_point = true}, `Char _ -> false
        | _, `EOF -> false
        | _ -> true
      in
      if not is_foreign then i.current_mode i l tok
      else foreign_content i l tok);
  } in

  (* Set foreign check using direct-style context *)
  i.foreign <- (fun () ->
    Stack.current_element_is_foreign i.the_context i.open_elements);

  (* For fragment parsing, reset_mode determines the actual initial mode *)
  begin match the_context with
  | `Fragment _ -> reset_mode i
  | `Document -> ()
  end;

  nextc i;
  i

let next_signal i =
  match i.prepend with
  | x :: rest -> i.prepend <- rest; Some x
  | [] ->
    while Queue.is_empty i.queue && not i.done_ do
      i.tok_fn i
    done;
    if Queue.is_empty i.queue then None
    else Some (Queue.pop i.queue)
