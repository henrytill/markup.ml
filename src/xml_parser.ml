(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Kstream
open Token_tag

let is_whitespace_only strings = List.for_all is_whitespace_only strings

(* -------------------------------------------------------------------------
   Old CPS-based parse function (kept for backward compatibility).
   ---------------------------------------------------------------------- *)

let parse context namespace report tokens =
  let open_elements = ref [] in
  let namespaces = Namespace.Parsing.init namespace in
  let is_fragment = ref false in
  let fragment_allowed = ref true in

  let throw = ref (fun _ -> ()) in
  let ended = ref (fun _ -> ()) in
  let output = ref (fun _ -> ()) in

  let rec current_state = ref (fun () ->
    match context with
    | None -> initial_state []
    | Some `Document ->
      fragment_allowed := false;
      document_state ()
    | Some `Fragment ->
      is_fragment := false;
      content_state ())

  and emit l signal state = current_state := state; !output (l, signal)

  and push_and_emit l {name = raw_name; attributes} state =
    Namespace.Parsing.push (fun () -> report l) namespaces raw_name attributes
      !throw (fun (expanded_name, expanded_attributes) ->

    let attributes =
      List.fold_left (fun acc ((n, _) as attr) ->
        if acc |> List.exists (fun (n', _) -> n' = n) then begin
          report l (`Bad_token (snd n, "tag", "duplicate attribute"));
          acc
        end else
          attr :: acc
      ) [] expanded_attributes
      |> List.rev
    in

    open_elements := (l, expanded_name, raw_name)::!open_elements;
    emit l (`Start_element (expanded_name, attributes)) state)

  and pop l state =
    match !open_elements with
    | [] -> state ()
    | _::more ->
      Namespace.Parsing.pop namespaces;
      open_elements := more;
      emit l `End_element state

  and emit_end () =
    current_state := (fun () -> !ended ());
    !ended ()

  and initial_state leading =
    next_expected tokens !throw begin function
      | _, (`Xml _ | `Doctype _ | `Start _ | `End _) as v ->
        push tokens v;
        push_list tokens (List.rev leading);
        document_state ()

      | _, `Chars s as v when is_whitespace_only s ->
        initial_state (v::leading)

      | _, (`Comment _ | `PI _) as v ->
        initial_state (v::leading)

      | _, (`Chars _ | `EOF) as v ->
        is_fragment := true;
        push tokens v;
        push_list tokens (List.rev leading);
        content_state ()
    end

  and document_state () =
    next_expected tokens !throw begin function
      | l, `Xml declaration ->
        fragment_allowed := false;
        emit l (`Xml declaration) doctype_state

      | v ->
        push tokens v;
        doctype_state ()
    end

  and doctype_state () =
    next_expected tokens !throw begin function
      | l, `Doctype d ->
        fragment_allowed := false;
        emit l (`Doctype d) root_state

      | _, `Chars s when is_whitespace_only s ->
        doctype_state ()

      | l, `Comment s ->
        emit l (`Comment s) doctype_state

      | l, `PI s ->
        emit l (`PI s) doctype_state

      | l, `Xml _ ->
        report l (`Bad_document "XML declaration must be first");
        doctype_state ()

      | l, `Chars _ ->
        report l (`Bad_document "text at top level");
        doctype_state ()

      | v ->
        push tokens v;
        root_state ()
    end

  and root_state () =
    next_expected tokens !throw begin function
      | l, `Start t ->
        if t.self_closing then
          push_and_emit l t (fun () ->
          pop l after_root_state)
        else
          push_and_emit l t content_state

      | _, `Chars s when is_whitespace_only s ->
        root_state ()

      | l, `Comment s ->
        emit l (`Comment s) root_state

      | l, `PI s ->
        emit l (`PI s) root_state

      | l, `Xml _ ->
        report l (`Bad_document "XML declaration must be first");
        root_state ()

      | l, `EOF ->
        report l (`Unexpected_eoi "document before root element");
        emit_end ()

      | l, _ ->
        report l (`Bad_document "expected root element");
        root_state ()
    end

  and after_root_state () =
    next_expected tokens !throw begin function
      | _, `Chars s when is_whitespace_only s ->
        after_root_state ()

      | l, `Comment s ->
        emit l (`Comment s) after_root_state

      | l, `PI s ->
        emit l (`PI s) after_root_state

      | _, `EOF ->
        emit_end ()

      | _, (`Chars _ | `Start _ | `End _) as v when !fragment_allowed ->
        is_fragment := true;
        push tokens v;
        content_state ()

      | l, _ as v ->
        report l (`Bad_document "not allowed after root element");
        is_fragment := true;
        push tokens v;
        content_state ()
    end

  and content_state () =
    next_expected tokens !throw begin function
      | l, `Start t ->
        if t.self_closing then
          push_and_emit l t (fun () ->
          pop l content_state)
        else
          push_and_emit l t content_state

      | l, `End {name = raw_name} ->
        let expanded_name =
          Namespace.Parsing.expand_element (fun () -> report l) namespaces
            raw_name
        in

        let is_on_stack =
          !open_elements
          |> List.exists (fun (_, name, _) -> name = expanded_name)
        in

        if not is_on_stack then begin
          report l (`Unmatched_end_tag raw_name);
          content_state ()
        end else
          let rec pop_until_match () =
            match !open_elements with
            | (_, name, _)::_ when name = expanded_name ->
              pop l (fun () ->
              match !open_elements with
              | [] when not !is_fragment -> after_root_state ()
              | _ -> content_state ())

            | (l', _, name)::_ ->
              report l' (`Unmatched_start_tag name);
              pop l pop_until_match

            | _ -> failwith "impossible"
          in
          pop_until_match ()

      | l, `Chars s ->
        emit l (`Text s) content_state

      | l, `PI s ->
        emit l (`PI s) content_state

      | l, `Comment s ->
        emit l (`Comment s) content_state

      | l, `EOF ->
        let rec pop_stack () =
          match !open_elements with
          | [] -> emit_end ()
          | (l', _, raw_name)::_ ->
            report l' (`Unmatched_start_tag raw_name);
            pop l pop_stack
        in
        pop_stack ()

      | l, `Xml _ ->
        report l (`Bad_document "XML declaration should be at top level");
        content_state ()

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be at top level");
        content_state ()
    end

  in

  (fun throw_ e k ->
    throw := throw_;
    ended := e;
    output := k;
    !current_state ())
  |> make


(* -------------------------------------------------------------------------
   New direct-style fused tokenizer+parser (xml_input).
   ---------------------------------------------------------------------- *)

(** Character-level checkpoint for entity backtracking. *)
type checkpoint = {
  mutable active   : bool;
  mutable start_c  : int;
  mutable buf      : int list;
}

type xml_input = {
  (* character layer *)
  decoder          : unit -> int;
  mutable c        : int;
  mutable cr       : bool;
  mutable line     : int;
  mutable col      : int;
  mutable first_char : bool;
  is_valid_char    : int -> bool;
  report           : location -> Error.t -> unit;
  mutable pushback : int list;
  chk              : checkpoint;
  (* tokenizer state *)
  mutable tok_state : xml_input -> unit;
  resolve_reference : string -> string option;
  (* parser state *)
  mutable parse_state : xml_input -> (location * Xml_tokenizer.token) -> unit;
  open_elements    : (location * Common.name * string) list ref;
  namespaces       : Namespace.Parsing.context;
  mutable is_fragment : bool;
  mutable fragment_allowed : bool;
  context          : [ `Document | `Fragment ] option;
  (* text accumulation *)
  text_buf         : Buffer.t;
  mutable text_loc : location;
  mutable in_text  : bool;
  (* signal output *)
  mutable prepend  : (location * Common.signal) list;
  queue            : (location * Common.signal) Queue.t;
  mutable done_    : bool;
  (* pending tokens for initial_state lookahead replay *)
  mutable pending_tokens : (location * Xml_tokenizer.token) list;
}

(* --- Character layer --- *)

let loc i = (i.line, i.col)

let rec nextc i =
  (* Update position for the *current* char before advancing.
     When c = -2 (sentinel) neither branch fires so position stays (1,1). *)
  if i.c = 0x0A then (i.line <- i.line + 1; i.col <- 1)
  else if i.c >= 0 then i.col <- i.col + 1;
  (* Get next raw codepoint *)
  let raw = match i.pushback with
    | x :: rest -> i.pushback <- rest; x
    | []        -> i.decoder ()
  in
  (* Record in checkpoint if active (record the char we're leaving) *)
  if i.chk.active then i.chk.buf <- i.c :: i.chk.buf;
  (* CR/CRLF normalization and BOM handling *)
  if i.first_char then begin
    i.first_char <- false;
    if raw = 0xFEFF then nextc i  (* skip BOM *)
    else begin
      if raw >= 0 && not (i.is_valid_char raw) then
        i.report (loc i) (`Bad_token (Common.format_char raw, "input", "out of range"));
      if raw = 0x0D then (i.cr <- true; i.c <- 0x0A)
      else (i.cr <- false; i.c <- raw)
    end
  end else if raw = 0x0A && i.cr then begin
    i.cr <- false;
    nextc i
  end else begin
    i.cr <- (raw = 0x0D);
    let c = if raw = 0x0D then 0x0A else raw in
    if c >= 0 && not (i.is_valid_char c) then
      i.report (loc i) (`Bad_token (Common.format_char c, "input", "out of range"));
    i.c <- c
  end

let begin_checkpoint i =
  i.chk.active <- true;
  i.chk.start_c <- i.c;
  i.chk.buf <- []

let rollback i =
  i.pushback <- (List.rev i.chk.buf) @ i.pushback;
  i.c <- i.chk.start_c;
  i.chk.active <- false;
  i.chk.buf <- []

let commit i =
  i.chk.active <- false;
  i.chk.buf <- []

(* --- Text accumulation --- *)

let add_text i c =
  if not i.in_text then begin
    i.text_loc <- loc i;
    i.in_text <- true
  end;
  Common.add_utf_8 i.text_buf c

let add_text_string i s =
  if not i.in_text then begin
    i.text_loc <- loc i;
    i.in_text <- true
  end;
  Buffer.add_string i.text_buf s

let flush_text i =
  if i.in_text then begin
    let s = Buffer.contents i.text_buf in
    Buffer.clear i.text_buf;
    i.in_text <- false;
    Queue.push (i.text_loc, `Text [s]) i.queue
  end

(* Emit a signal (flushes pending text first) *)
let emit_signal i l s =
  flush_text i;
  Queue.push (l, s) i.queue

let emit_done i =
  i.done_ <- true

(* --- Token dispatch --- *)

let dispatch_token i l token =
  flush_text i;
  i.parse_state i (l, token)

(* --- Tokenizer helpers --- *)

let is_name_start_char c =
  is_in_range 0x0041 0x005A c
  || is_in_range 0x0061 0x007A c
  || c = 0x003A
  || c = 0x005F
  || is_in_range 0x00C0 0x00D6 c
  || is_in_range 0x00D8 0x00F6 c
  || is_in_range 0x00F8 0x02FF c
  || is_in_range 0x0370 0x037D c
  || is_in_range 0x037F 0x1FFF c
  || is_in_range 0x200C 0x200D c
  || is_in_range 0x2070 0x218F c
  || is_in_range 0x2C00 0x2FEF c
  || is_in_range 0x3001 0xD7EF c
  || is_in_range 0xF900 0xFDCF c
  || is_in_range 0xFDF0 0xFFFD c
  || is_in_range 0x10000 0xEFFFF c

let is_name_char c =
  is_name_start_char c
  || is_in_range 0x0030 0x0039 c
  || c = 0x002D
  || c = 0x002E
  || c = 0x00B7
  || is_in_range 0x0300 0x036F c
  || is_in_range 0x203F 0x2040 c

let resolve_builtin_reference = function
  | "quot" -> Some "\""
  | "amp"  -> Some "&"
  | "apos" -> Some "'"
  | "lt"   -> Some "<"
  | "gt"   -> Some ">"
  | _      -> None

let resolve_ref i s =
  match resolve_builtin_reference s with
  | Some _ as v -> v
  | None -> i.resolve_reference s

let report_if i cond l mk =
  if cond then i.report l (mk ())

(* --- parse_reference: returns Some s or None --- *)

let parse_reference i l' =
  begin_checkpoint i;

  let unresolved () =
    rollback i;
    None
  in

  let unexpected_eoi () =
    i.report (loc i) (`Unexpected_eoi "reference");
    rollback i
  in

  let char_ref filter notation_prefix reference_prefix =
    let buffer = Buffer.create 32 in
    let rec read () =
      if i.c = -1 then begin
        unexpected_eoi ();
        None
      end else if i.c = 0x003B then begin
        nextc i;
        if Buffer.length buffer = 0 then begin
          i.report l' (`Bad_token
            (Printf.sprintf "&#%s;" reference_prefix, "reference",
             "empty character reference"));
          unresolved ()
        end else
          let s = Buffer.contents buffer in
          let maybe_n =
            try Some (int_of_string (notation_prefix ^ s))
            with Failure _ -> None
          in
          begin match maybe_n with
          | None ->
            i.report l' (`Bad_token
              (Printf.sprintf "&#%s%s;" reference_prefix s, "reference",
               "number out of range"));
            unresolved ()
          | Some n ->
            let utf_8_encoded = Buffer.create 8 in
            Common.add_utf_8 utf_8_encoded n;
            commit i;
            Some (Buffer.contents utf_8_encoded)
          end
      end else if filter i.c then begin
        Common.add_utf_8 buffer i.c;
        nextc i;
        read ()
      end else begin
        let l = loc i in
        i.report l (`Bad_token (Common.char i.c, "reference", "expected digit"));
        unresolved ()
      end
    in
    read ()
  in

  if i.c = -1 then begin
    unexpected_eoi ();
    None
  end else if i.c = 0x003B then begin
    nextc i;
    i.report l' (`Bad_token ("&;", "reference", "empty reference"));
    unresolved ()
  end else if i.c = 0x0023 then begin
    (* character reference *)
    nextc i;
    if i.c = -1 then begin
      unexpected_eoi ();
      None
    end else if i.c = 0x0078 then begin
      nextc i;
      char_ref is_hex_digit "0x" "x"
    end else if is_digit i.c || i.c = 0x003B then begin
      (* decimal: don't advance, char_ref will see it *)
      char_ref is_digit "" ""
    end else begin
      let l = loc i in
      i.report l (`Bad_token (Common.char i.c, "reference", "expected digit"));
      unresolved ()
    end
  end else if is_name_start_char i.c then begin
    let buffer = Buffer.create 32 in
    Common.add_utf_8 buffer i.c;
    nextc i;
    let rec read () =
      if i.c = -1 then begin
        unexpected_eoi ();
        None
      end else if i.c = 0x003B then begin
        nextc i;
        let s = Buffer.contents buffer in
        begin match resolve_ref i s with
        | Some s -> commit i; Some s
        | None ->
          i.report l' (`Bad_token (s, "reference", "unknown entity"));
          unresolved ()
        end
      end else if is_name_char i.c then begin
        Common.add_utf_8 buffer i.c;
        nextc i;
        read ()
      end else begin
        let l = loc i in
        i.report l
          (`Bad_token (Common.char i.c, "reference", "invalid name character"));
        unresolved ()
      end
    in
    read ()
  end else begin
    let l = loc i in
    i.report l
      (`Bad_token (Common.char i.c, "reference", "invalid start character"));
    unresolved ()
  end

(* --- Attribute parsing --- *)

let parse_attribute i with_references terminators l =
  let name_buffer = Buffer.create 32 in
  let value_buffer = Buffer.create 256 in
  let quote_opened = ref false in
  let quote_closed = ref false in

  let extra_whitespace where c =
    i.report (loc i)
      (`Bad_token (Common.char c, where, "whitespace not allowed here"))
  in

  let is_terminator () = i.c = -1 || List.mem i.c terminators in

  let finish () =
    if Buffer.length name_buffer = 0 then None
    else
      let result () =
        Some (Buffer.contents name_buffer, Buffer.contents value_buffer)
      in
      if !quote_opened then begin
        if not !quote_closed then
          i.report (loc i) (`Unexpected_eoi "attribute value");
        result ()
      end else if Buffer.length value_buffer = 0 then begin
        i.report l (`Bad_token
          (Buffer.contents name_buffer, "attribute", "has no value"));
        result ()
      end else
        result ()
  in

  let handle_ampersand state =
    let la = loc i in
    nextc i; (* consume & *)
    (match parse_reference i la with
    | Some s -> Buffer.add_string value_buffer s
    | None ->
      i.report la (`Bad_token ("&", "attribute", "replace with '&amp;'"));
      Common.add_utf_8 value_buffer 0x0026);
    state ()
  in

  let handle_lt () =
    i.report (loc i) (`Bad_token ("<", "attribute", "replace with '&lt;'"));
    Common.add_utf_8 value_buffer 0x003C
  in

  let rec name_start_state () =
    if is_terminator () then finish ()
    else begin
      let la = loc i in
      let c = i.c in
      report_if i (not @@ is_name_start_char c) la (fun () ->
        `Bad_token (Common.char c, "attribute", "invalid start character"));
      Common.add_utf_8 name_buffer c;
      nextc i;
      name_state ()
    end

  and name_state () =
    if is_terminator () then finish ()
    else
      let c = i.c in
      if c = 0x003D then begin
        nextc i; value_state ()
      end else if is_whitespace c then begin
        let la = loc i in
        extra_whitespace "attribute" c;
        ignore la;
        nextc i;
        while i.c >= 0 && is_whitespace i.c do nextc i done;
        equals_state ()
      end else begin
        let la = loc i in
        report_if i (not @@ is_name_char c) la (fun () ->
          `Bad_token (Common.char c, "attribute", "invalid name character"));
        Common.add_utf_8 name_buffer c;
        nextc i;
        name_state ()
      end

  and equals_state () =
    if is_terminator () then finish ()
    else if i.c = 0x003D then begin
      nextc i; value_state ()
    end else
      finish ()

  and value_state () =
    if is_terminator () then finish ()
    else
      let c = i.c in
      if is_whitespace c then begin
        extra_whitespace "attribute" c;
        nextc i;
        while i.c >= 0 && is_whitespace i.c do nextc i done;
        value_state ()
      end else if c = 0x0022 || c = 0x0027 then begin
        quote_opened := true;
        nextc i;
        quoted_value_state c
      end else begin
        i.report (loc i)
          (`Bad_token (Common.char c, "attribute", "unquoted value"));
        unquoted_value_state ()
      end

  and quoted_value_state quote =
    if i.c = -1 then finish ()
    else
      let c = i.c in
      if c = quote then begin
        quote_closed := true; nextc i; finish ()
      end else if c = 0x0026 && with_references then
        handle_ampersand (fun () -> quoted_value_state quote)
      else if c = 0x003C then begin
        handle_lt (); nextc i; quoted_value_state quote
      end else begin
        Common.add_utf_8 value_buffer c; nextc i; quoted_value_state quote
      end

  and unquoted_value_state () =
    if is_terminator () || (i.c >= 0 && is_whitespace i.c) then finish ()
    else
      let c = i.c in
      if c = 0x0026 && with_references then
        handle_ampersand unquoted_value_state
      else if c = 0x003C then begin
        handle_lt (); nextc i; unquoted_value_state ()
      end else begin
        Common.add_utf_8 value_buffer c; nextc i; unquoted_value_state ()
      end

  in
  name_start_state ()

(* --- PI / XML declaration parsing --- *)
(* next' implements the ?> detection wrapper used inside <?...?>.
   It calls f () unless the current char sequence completes ?>. *)

let pi_next' i context_name finish_fn f =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi context_name);
    finish_fn ()
  end else if i.c = 0x003F then begin
    nextc i;
    if i.c = -1 then begin
      i.report (loc i) (`Unexpected_eoi context_name);
      finish_fn ()
    end else if i.c = 0x003E then begin
      nextc i;
      finish_fn ()
    end else begin
      (* push back current char, replace c with ? *)
      i.pushback <- i.c :: i.pushback;
      i.c <- 0x003F;
      f ()
    end
  end else
    f ()

(* Parse everything inside <?...?> starting just after the first ?.
   Calls dispatch_token with `PI or `Xml on completion. *)
let tok_pi_or_xml i l' =
  let pi_name = "processing instruction" in
  let xml_name = "xml declaration" in

  let target_buffer = Buffer.create 32 in
  let text_buffer = Buffer.create 512 in
  let attributes = ref [] in

  let extra_ws where c =
    i.report (loc i) (`Bad_token (Common.char c, where, "whitespace not allowed here"))
  in

  let rec target_start_state () =
    pi_next' i pi_name finish_pi (fun () ->
      let c = i.c in
      if is_whitespace c then begin
        extra_ws pi_name c;
        nextc i;
        while i.c >= 0 && is_whitespace i.c do nextc i done;
        target_start_state ()
      end else begin
        let la = loc i in
        report_if i (not @@ is_name_start_char c) la (fun () ->
          `Bad_token (Common.char c, pi_name, "invalid start character"));
        Common.add_utf_8 target_buffer c;
        nextc i;
        target_state ()
      end)

  and target_state () =
    pi_next' i pi_name finish_pi (fun () ->
      let c = i.c in
      if is_whitespace c then begin
        nextc i;
        if String.lowercase_ascii (Buffer.contents target_buffer) = "xml" then
          xml_decl_state ()
        else
          text_state ()
      end else begin
        let la = loc i in
        report_if i (not @@ is_name_char c) la (fun () ->
          `Bad_token (Common.char c, pi_name, "invalid name character"));
        Common.add_utf_8 target_buffer c;
        nextc i;
        target_state ()
      end)

  and text_state () =
    pi_next' i pi_name finish_pi (fun () ->
      Common.add_utf_8 text_buffer i.c;
      nextc i;
      text_state ())

  and xml_decl_state () =
    pi_next' i xml_name finish_xml (fun () ->
      let c = i.c in
      if is_whitespace c then begin
        nextc i;
        xml_decl_state ()
      end else if c = 0x003F then begin
        nextc i;
        xml_decl_state ()
      end else begin
        let la = loc i in
        (match parse_attribute i false [0x003F] la with
        | None -> ()
        | Some (name, value) ->
          attributes := (la, name, value) :: !attributes);
        xml_decl_state ()
      end)

  and finish_pi () =
    if Buffer.length target_buffer = 0 then
      i.report l' (`Bad_token ("<?...", pi_name, "empty"))
      (* no token dispatched - return to initial_tok_state *)
    else if String.lowercase_ascii (Buffer.contents target_buffer) = "xml" then
      finish_xml ()
    else
      dispatch_token i l'
        (`PI (Buffer.contents target_buffer, Buffer.contents text_buffer))

  and finish_xml () =
    let split f l =
      let rec scan prefix = function
        | x::suffix when f x -> Some (List.rev prefix, x, suffix)
        | x::suffix -> scan (x::prefix) suffix
        | [] -> None
      in
      scan [] l
    in

    let matches s (_, name, _) = String.lowercase_ascii name = s in

    let version_valid s =
      String.length s = 3 &&
      s.[0] = '1' && s.[1] = '.' && is_digit (Char.code s.[2])
    in

    let rec check_name attributes =
      let target = Buffer.contents target_buffer in
      report_if i (target <> "xml") l' (fun () ->
        `Bad_token (target, xml_name, "must be 'xml'"));
      version_state attributes

    and version_state attributes =
      match split (matches "version") attributes with
      | None ->
        i.report l' (`Bad_token ("<?xml...", xml_name, "missing version"));
        encoding_state "1.0" attributes

      | Some (prefix, (la, name, value), suffix) ->
        report_if i (name <> "version") la (fun () ->
          `Bad_token (name, xml_name, "must be 'version'"));
        report_if i (List.length prefix <> 0) la (fun () ->
          `Bad_token (name, xml_name, "must be first"));
        report_if i (not @@ version_valid value) la (fun () ->
          `Bad_token (value, xml_name, "must match 1.x"));
        encoding_state value (prefix @ suffix)

    and encoding_state version attributes =
      match split (matches "encoding") attributes with
      | None ->
        standalone_state version None 0 attributes

      | Some (prefix, (la, name, value), suffix) ->
        report_if i (name <> "encoding") la (fun () ->
          `Bad_token (name, xml_name, "must be 'encoding'"));
        standalone_state version (Some value) (List.length prefix) (prefix @ suffix)

    and standalone_state version encoding encoding_index attributes =
      match split (matches "standalone") attributes with
      | None ->
        final_state version encoding None attributes

      | Some (prefix, (la, name, value), suffix) ->
        report_if i (name <> "standalone") la (fun () ->
          `Bad_token (name, xml_name, "must be 'standalone'"));
        report_if i (List.length prefix < encoding_index) la (fun () ->
          `Bad_token (name, xml_name, "must come after 'encoding'"));
        let v =
          match value with
          | "yes" -> Some true
          | "no"  -> Some false
          | _ ->
            i.report la (`Bad_token (value, xml_name, "must be 'yes' or 'no'"));
            (match String.lowercase_ascii value with
            | "yes" -> Some true
            | "no"  -> Some false
            | _     -> None)
        in
        final_state version encoding v (prefix @ suffix)

    and final_state version encoding standalone attributes =
      (match attributes with
      | (la, name, _)::_ ->
        i.report la (`Bad_token (name, xml_name, "not allowed here"))
      | [] -> ());
      dispatch_token i l' (`Xml {version; encoding; standalone})

    in
    check_name (List.rev !attributes)

  in
  target_start_state ()

(* --- Main tokenizer state functions --- *)

(* All return unit; they set i.tok_state to themselves or another state
   when they need more input, or call dispatch_token which calls parse_state. *)

let lt_in_text i l =
  i.report l (`Bad_token ("<", "text", "replace with '&lt;'"))

let rec initial_tok i =
  if i.c = -1 then begin
    let l = loc i in
    dispatch_token i l `EOF
  end else begin
    let c = i.c in
    let l = loc i in
    if c = 0x005D then begin
      add_text i c;
      nextc i;
      one_bracket i l
    end else if c = 0x003C then begin
      nextc i;
      begin_markup i l
    end else if c = 0x0026 then begin
      nextc i;
      (match parse_reference i l with
      | None ->
        i.report l (`Bad_token (Common.char c, "text", "replace with '&amp;'"));
        add_text i c
      | Some s ->
        add_text_string i s);
      initial_tok i
    end else begin
      add_text i c;
      nextc i;
      initial_tok i
    end
  end

and one_bracket i l' =
  if i.c = -1 || i.c <> 0x005D then
    initial_tok i
  else begin
    let l = loc i in
    add_text i i.c;
    nextc i;
    two_brackets i l' l
  end

and two_brackets i l' l'' =
  if i.c = -1 then
    initial_tok i
  else if i.c = 0x003E then begin
    i.report l' (`Bad_token ("]]>", "text", "must end a CDATA section"));
    add_text i i.c;
    nextc i;
    initial_tok i
  end else if i.c = 0x005D then begin
    let l = loc i in
    add_text i i.c;
    nextc i;
    two_brackets i l'' l
  end else
    initial_tok i

and begin_markup i l' =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi "tag");
    lt_in_text i l';
    add_text i 0x003C;
    initial_tok i
  end else begin
    let c = i.c in
    if c = 0x0021 then begin
      nextc i;
      comment_cdata_or_doctype i l'
    end else if c = 0x003F then begin
      nextc i;
      tok_pi_or_xml i l';
      i.tok_state <- initial_tok
    end else if c = 0x002F then begin
      nextc i;
      end_tag i l'
    end else if is_name_start_char c then begin
      let buf = Buffer.create 32 in
      Common.add_utf_8 buf c;
      nextc i;
      start_tag i l' buf
    end else begin
      let l = loc i in
      i.report l (`Bad_token (Common.char c, "tag", "invalid start character"));
      lt_in_text i l';
      add_text i 0x003C;
      (* don't consume c; initial_tok will handle it *)
      initial_tok i
    end
  end

and start_tag i l' buf =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi "tag");
    lt_in_text i l';
    add_text i 0x003C;
    add_text_string i (Buffer.contents buf);
    initial_tok i
  end else begin
    let c = i.c in
    if c = 0x003E then begin
      nextc i;
      let tag = {name = Buffer.contents buf; self_closing = false; attributes = []} in
      dispatch_token i l' (`Start tag);
      i.tok_state <- initial_tok
    end else if c = 0x002F then begin
      let l'' = loc i in
      nextc i;
      close_empty i l' l'' (Buffer.contents buf) []
    end else if is_whitespace c then begin
      nextc i;
      start_tag_attrs i l' (Buffer.contents buf) []
    end else if is_name_char c then begin
      Common.add_utf_8 buf c;
      nextc i;
      start_tag i l' buf
    end else begin
      let l = loc i in
      i.report l (`Bad_token (Common.char c, "tag", "invalid name character"));
      lt_in_text i l';
      add_text i 0x003C;
      add_text_string i (Buffer.contents buf);
      initial_tok i
    end
  end

and start_tag_attrs i l' tag_name attributes =
  if i.c = -1 then begin
    let tag = {name = tag_name; self_closing = false;
               attributes = List.rev attributes} in
    dispatch_token i l' (`Start tag);
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else begin
    let c = i.c in
    if is_whitespace c then begin
      nextc i;
      start_tag_attrs i l' tag_name attributes
    end else if c = 0x003E then begin
      nextc i;
      let tag = {name = tag_name; self_closing = false;
                 attributes = List.rev attributes} in
      dispatch_token i l' (`Start tag);
      i.tok_state <- initial_tok
    end else if c = 0x002F then begin
      let l'' = loc i in
      nextc i;
      close_empty i l' l'' tag_name attributes
    end else begin
      let la = loc i in
      (match parse_attribute i true [0x003E; 0x002F] la with
      | None -> start_tag_attrs i l' tag_name attributes
      | Some (name, value) ->
        start_tag_attrs i l' tag_name ((name, value) :: attributes))
    end
  end

and close_empty i l' l'' name attributes =
  if i.c = -1 then begin
    let tag = {name; self_closing = true; attributes = List.rev attributes} in
    dispatch_token i l' (`Start tag);
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x003E then begin
    nextc i;
    let tag = {name; self_closing = true; attributes = List.rev attributes} in
    dispatch_token i l' (`Start tag);
    i.tok_state <- initial_tok
  end else begin
    i.report l''
      (`Bad_token (Common.char 0x002F, "tag", "should be part of '/>'"));
    start_tag_attrs i l' name attributes
  end

and end_tag i l' =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi "tag");
    lt_in_text i l';
    add_text i 0x003C;
    add_text i 0x002F;
    initial_tok i
  end else begin
    let c = i.c in
    if is_name_start_char c then begin
      let buf = Buffer.create 32 in
      Common.add_utf_8 buf c;
      nextc i;
      end_tag_name i l' buf
    end else begin
      let l = loc i in
      i.report l (`Bad_token (Common.char c, "tag", "invalid start character"));
      lt_in_text i l';
      add_text i 0x003C;
      add_text i 0x002F;
      initial_tok i
    end
  end

and end_tag_name i l' buf =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi "tag");
    lt_in_text i l';
    add_text i 0x003C;
    add_text i 0x002F;
    add_text_string i (Buffer.contents buf);
    initial_tok i
  end else begin
    let c = i.c in
    if c = 0x003E then begin
      nextc i;
      let tag = {name = Buffer.contents buf; self_closing = false; attributes = []} in
      dispatch_token i l' (`End tag);
      i.tok_state <- initial_tok
    end else if is_whitespace c then begin
      nextc i;
      end_tag_ws i false l' (Buffer.contents buf)
    end else if is_name_char c then begin
      Common.add_utf_8 buf c;
      nextc i;
      end_tag_name i l' buf
    end else begin
      let l = loc i in
      i.report l (`Bad_token (Common.char c, "tag", "invalid name character"));
      lt_in_text i l';
      add_text i 0x003C;
      add_text i 0x002F;
      add_text_string i (Buffer.contents buf);
      initial_tok i
    end
  end

and end_tag_ws i reported l' name =
  if i.c = -1 then begin
    let tag = {name; self_closing = false; attributes = []} in
    dispatch_token i l' (`End tag);
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else begin
    let c = i.c in
    if c = 0x003E then begin
      nextc i;
      let tag = {name; self_closing = false; attributes = []} in
      dispatch_token i l' (`End tag);
      i.tok_state <- initial_tok
    end else if is_whitespace c then begin
      nextc i;
      end_tag_ws i reported l' name
    end else begin
      if not reported then
        i.report (loc i)
          (`Bad_token (Common.char c, "tag", "attribute in end tag"));
      nextc i;
      end_tag_ws i true l' name
    end
  end

and comment_cdata_or_doctype i l' =
  if i.c = -1 then begin
    bad_comment_start i "<!" l';
    add_text i 0x003C;
    add_text i 0x0021;
    initial_tok i
  end else begin
    let c = i.c in
    if c = 0x002D then begin
      nextc i; comment_start i l'
    end else if c = 0x005B then begin
      nextc i; cdata_start i l'
    end else if c = 0x0044 then begin
      nextc i; doctype_start i l'
    end else begin
      bad_comment_start i "<!" l';
      add_text i 0x003C;
      add_text i 0x0021;
      initial_tok i
    end
  end

and bad_comment_start i s l =
  i.report l (`Bad_token (s, "comment", "should start with '<!--'"));
  lt_in_text i l

and comment_start i l' =
  if i.c = -1 then begin
    bad_comment_start i "<!-" l';
    add_text i 0x003C;
    add_text i 0x0021;
    add_text i 0x002D;
    initial_tok i
  end else if i.c = 0x002D then begin
    nextc i;
    comment i l' (Buffer.create 256)
  end else begin
    bad_comment_start i "<!-" l';
    add_text i 0x003C;
    add_text i 0x0021;
    add_text i 0x002D;
    initial_tok i
  end

and comment i l' buf =
  if i.c = -1 then begin
    dispatch_token i l' (`Comment (Buffer.contents buf));
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else begin
    let c = i.c in
    if c = 0x002D then begin
      nextc i;
      comment_one_dash i l' buf
    end else begin
      Common.add_utf_8 buf c;
      nextc i;
      comment i l' buf
    end
  end

and comment_one_dash i l' buf =
  if i.c = -1 then begin
    dispatch_token i l' (`Comment (Buffer.contents buf));
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x002D then begin
    nextc i;
    comment_two_dashes i false l' buf
  end else begin
    Common.add_utf_8 buf 0x002D;
    Common.add_utf_8 buf i.c;
    nextc i;
    comment i l' buf
  end

and comment_two_dashes i reported l' buf =
  if i.c = -1 then begin
    dispatch_token i l' (`Comment (Buffer.contents buf));
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x003E then begin
    nextc i;
    dispatch_token i l' (`Comment (Buffer.contents buf));
    i.tok_state <- initial_tok
  end else if i.c = 0x002D then begin
    if not reported then
      i.report l' (`Bad_token ("--", "comment", "should be followed by '>'"));
    Common.add_utf_8 buf 0x002D;
    nextc i;
    comment_two_dashes i true l' buf
  end else begin
    if not reported then
      i.report l' (`Bad_token ("--", "comment", "should be followed by '>'"));
    Common.add_utf_8 buf 0x002D;
    Common.add_utf_8 buf 0x002D;
    Common.add_utf_8 buf i.c;
    nextc i;
    comment i l' buf
  end

and cdata_start i l' =
  (* Expect C D A T A [ (6 chars) *)
  let expected = [| 0x43; 0x44; 0x41; 0x54; 0x41; 0x005B |] in
  let collected = Array.make 6 0 in
  let ok = ref true in
  let n = ref 0 in
  while !n < 6 && i.c >= 0 do
    collected.(!n) <- i.c;
    if i.c <> expected.(!n) then ok := false;
    incr n;
    if !n < 6 then nextc i
  done;
  if !n < 6 then ok := false;
  if !ok then begin
    nextc i; (* advance past [ *)
    cdata i l'
  end else begin
    i.report l' (`Bad_token ("<![", "cdata", "should start with '<![CDATA['"));
    lt_in_text i l';
    add_text i 0x003C;
    add_text i 0x0021;
    add_text i 0x005B;
    (* push collected chars back; skip already-consumed leading chars *)
    (* collected[0..n-1] were consumed; current i.c = collected[n-1] *)
    (* We need to push back collected[1..n-1] since i.c = collected[0] was
       already "replaced" by the array processing. Actually: we read from
       i.c each iteration. After the loop, i.c = collected[n-1] (the last
       one we checked). We need to push back collected[0..n-2] before it.
       But wait - we called nextc i after each iteration except the last.
       So after n iterations: i.c = collected[n-1], and collected[0..n-2]
       have all been advanced through. We need to push them all back. *)
    if !n > 0 then begin
      let pb = ref [] in
      for j = !n - 1 downto 1 do
        pb := collected.(j) :: !pb
      done;
      i.pushback <- !pb @ i.pushback;
      i.c <- collected.(0)
    end;
    initial_tok i
  end

and cdata i l' =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi "cdata");
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x005D then begin
    let l'' = loc i in
    nextc i;
    cdata_one_bracket i l' l''
  end else begin
    add_text i i.c;
    nextc i;
    cdata i l'
  end

and cdata_one_bracket i l' l'' =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi "cdata");
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x005D then begin
    let l''' = loc i in
    nextc i;
    cdata_two_brackets i l' l'' l'''
  end else begin
    add_text i 0x005D;
    add_text i i.c;
    nextc i;
    cdata i l'
  end

and cdata_two_brackets i l' _l'' l''' =
  if i.c = -1 then begin
    i.report (loc i) (`Unexpected_eoi "cdata");
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x003E then begin
    nextc i;
    (* CDATA section ends; l'' was the first bracket, l''' the second.
       Don't add them to text - they were brackets, not content. *)
    initial_tok i
  end else if i.c = 0x005D then begin
    (* Another bracket: the second bracket becomes the first, current = new second *)
    add_text i 0x005D; (* output l'' *)
    let l_new = loc i in
    nextc i;
    cdata_two_brackets i l' l''' l_new
  end else begin
    add_text i 0x005D;
    add_text i 0x005D;
    add_text i i.c;
    nextc i;
    cdata i l'
  end

and doctype_start i l' =
  (* Expect O C T Y P E <ws> = 7 chars; first char 'D' was already consumed *)
  let expected = [| 0x4F; 0x43; 0x54; 0x59; 0x50; 0x45 |] in
  let collected = Array.make 7 0 in
  let ok = ref true in
  let n = ref 0 in
  while !n < 6 && i.c >= 0 do
    collected.(!n) <- i.c;
    if i.c <> expected.(!n) then ok := false;
    incr n;
    if !n < 6 then nextc i
  done;
  (* now read 7th char (must be whitespace) *)
  if !ok && !n = 6 then begin
    nextc i;
    collected.(6) <- i.c;
    n := 7;
    ok := i.c >= 0 && is_whitespace i.c
  end else
    ok := false;
  if !ok then begin
    nextc i; (* consume the whitespace *)
    doctype_body i l' (Buffer.create 512)
  end else begin
    i.report l'
      (`Bad_token ("<!D", "doctype", "should start with '<!DOCTYPE '"));
    lt_in_text i l';
    add_text i 0x003C;
    add_text i 0x0021;
    add_text i 0x0044;
    if !n > 0 then begin
      let pb = ref [] in
      for j = !n - 1 downto 1 do
        pb := collected.(j) :: !pb
      done;
      i.pushback <- !pb @ i.pushback;
      i.c <- collected.(0)
    end;
    initial_tok i
  end

and doctype_body i l' buf =
  if i.c = -1 then begin
    dispatch_doctype i l' buf;
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x003E then begin
    nextc i;
    dispatch_doctype i l' buf;
    i.tok_state <- initial_tok
  end else if i.c = 0x0022 || i.c = 0x0027 then begin
    let q = i.c in
    Common.add_utf_8 buf q;
    nextc i;
    doctype_quoted i l' buf (fun () -> doctype_body i l' buf) q
  end else if i.c = 0x003C then begin
    Common.add_utf_8 buf i.c;
    nextc i;
    doctype_item i l' buf (fun () -> doctype_body i l' buf)
  end else begin
    Common.add_utf_8 buf i.c;
    nextc i;
    doctype_body i l' buf
  end

and doctype_quoted i l' buf state quote =
  if i.c = -1 then begin
    dispatch_doctype i l' buf;
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = quote then begin
    Common.add_utf_8 buf i.c;
    nextc i;
    state ()
  end else begin
    Common.add_utf_8 buf i.c;
    nextc i;
    doctype_quoted i l' buf state quote
  end

and doctype_item i l' buf state =
  if i.c = -1 then begin
    dispatch_doctype i l' buf;
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x0021 then begin
    Common.add_utf_8 buf i.c;
    nextc i;
    doctype_decl i l' buf state
  end else if i.c = 0x003F then begin
    (* PI inside doctype: consume it into buf but don't dispatch *)
    Common.add_utf_8 buf i.c;
    nextc i;
    let rec consume_pi () =
      if i.c = -1 then state ()
      else if i.c = 0x003F then begin
        Common.add_utf_8 buf 0x003F;
        nextc i;
        if i.c = 0x003E then begin
          Common.add_utf_8 buf 0x003E;
          nextc i;
          state ()
        end else consume_pi ()
      end else begin
        Common.add_utf_8 buf i.c;
        nextc i;
        consume_pi ()
      end
    in
    consume_pi ()
  end else begin
    Common.add_utf_8 buf i.c;
    nextc i;
    state ()
  end

and doctype_decl i l' buf state =
  if i.c = -1 then begin
    dispatch_doctype i l' buf;
    dispatch_token i (loc i) `EOF;
    i.tok_state <- initial_tok
  end else if i.c = 0x003E then begin
    Common.add_utf_8 buf i.c;
    nextc i;
    state ()
  end else if i.c = 0x0022 || i.c = 0x0027 then begin
    let q = i.c in
    Common.add_utf_8 buf q;
    nextc i;
    doctype_quoted i l' buf (fun () -> doctype_decl i l' buf state) q
  end else begin
    Common.add_utf_8 buf i.c;
    nextc i;
    doctype_decl i l' buf state
  end

and dispatch_doctype i l' buf =
  let doctype =
    {doctype_name      = None;
     public_identifier = None;
     system_identifier = None;
     raw_text          = Some (Buffer.contents buf);
     force_quirks      = false}
  in
  dispatch_token i l' (`Doctype doctype)

(* ---- Parser states ---- *)

and p_initial i ((_l, tok) as token) =
  match tok with
  | `Xml _ | `Doctype _ | `Start _ | `End _ ->
    i.parse_state <- p_document;
    p_document i token

  | `Chars s when is_whitespace_only s ->
    i.pending_tokens <- i.pending_tokens @ [token]

  | `Comment _ | `PI _ ->
    i.pending_tokens <- i.pending_tokens @ [token]

  | `Chars _ | `EOF ->
    i.is_fragment <- true;
    i.pending_tokens <- i.pending_tokens @ [token];
    i.parse_state <- p_content

and p_document i (l, tok) =
  match tok with
  | `Xml declaration ->
    i.fragment_allowed <- false;
    emit_signal i l (`Xml declaration);
    i.parse_state <- p_doctype

  | _ ->
    i.parse_state <- p_doctype;
    p_doctype i (l, tok)

and p_doctype i (l, tok) =
  match tok with
  | `Doctype d ->
    i.fragment_allowed <- false;
    emit_signal i l (`Doctype d);
    i.parse_state <- p_root

  | `Chars s when is_whitespace_only s -> ()

  | `Comment s -> emit_signal i l (`Comment s)

  | `PI s -> emit_signal i l (`PI s)

  | `Xml _ -> i.report l (`Bad_document "XML declaration must be first")

  | `Chars _ -> i.report l (`Bad_document "text at top level")

  | _ ->
    i.parse_state <- p_root;
    p_root i (l, tok)

and p_root i (l, tok) =
  match tok with
  | `Start t ->
    if t.self_closing then begin
      p_push_emit i l t;
      p_pop i l;
      i.parse_state <- p_after_root
    end else begin
      p_push_emit i l t;
      i.parse_state <- p_content
    end

  | `Chars s when is_whitespace_only s -> ()

  | `Comment s -> emit_signal i l (`Comment s)

  | `PI s -> emit_signal i l (`PI s)

  | `Xml _ -> i.report l (`Bad_document "XML declaration must be first")

  | `EOF ->
    i.report l (`Unexpected_eoi "document before root element");
    emit_done i

  | _ -> i.report l (`Bad_document "expected root element")

and p_after_root i (l, tok) =
  match tok with
  | `Chars s when is_whitespace_only s -> ()

  | `Comment s -> emit_signal i l (`Comment s)

  | `PI s -> emit_signal i l (`PI s)

  | `EOF -> emit_done i

  | (`Chars _ | `Start _ | `End _) when i.fragment_allowed ->
    i.is_fragment <- true;
    i.pending_tokens <- [l, tok];
    i.parse_state <- p_content

  | _ ->
    i.report l (`Bad_document "not allowed after root element");
    i.is_fragment <- true;
    i.pending_tokens <- [l, tok];
    i.parse_state <- p_content

and p_content i (l, tok) =
  match tok with
  | `Start t ->
    p_push_emit i l t;
    if t.self_closing then begin
      p_pop i l;
      if !(i.open_elements) = [] && not i.is_fragment then
        i.parse_state <- p_after_root
      (* else stay in p_content *)
    end
    (* else stay in p_content *)

  | `End {name = raw_name} ->
    let expanded_name =
      Namespace.Parsing.expand_element (fun () -> i.report l) i.namespaces
        raw_name
    in
    let is_on_stack =
      !(i.open_elements)
      |> List.exists (fun (_, name, _) -> name = expanded_name)
    in
    if not is_on_stack then
      i.report l (`Unmatched_end_tag raw_name)
    else begin
      let rec pop_until () =
        match !(i.open_elements) with
        | (_, name, _)::_ when name = expanded_name ->
          p_pop i l;
          if !(i.open_elements) = [] && not i.is_fragment then
            i.parse_state <- p_after_root
        | (l', _, name)::_ ->
          i.report l' (`Unmatched_start_tag name);
          p_pop i l;
          pop_until ()
        | _ -> failwith "impossible"
      in
      pop_until ()
    end

  | `Chars s -> emit_signal i l (`Text s)

  | `PI s -> emit_signal i l (`PI s)

  | `Comment s -> emit_signal i l (`Comment s)

  | `EOF ->
    let rec pop_stack () =
      match !(i.open_elements) with
      | [] -> emit_done i
      | (l', _, raw_name)::_ ->
        i.report l' (`Unmatched_start_tag raw_name);
        p_pop i l;
        pop_stack ()
    in
    pop_stack ()

  | `Xml _ ->
    i.report l (`Bad_document "XML declaration should be at top level")

  | `Doctype _ ->
    i.report l (`Bad_document "doctype should be at top level")

and p_push_emit i l {name = raw_name; attributes} =
  Namespace.Parsing.push (fun () -> i.report l) i.namespaces raw_name attributes
    (fun _ -> ()) (fun (expanded_name, expanded_attributes) ->
    let attributes =
      List.fold_left (fun acc ((n, _) as attr) ->
        if acc |> List.exists (fun (n', _) -> n' = n) then begin
          i.report l (`Bad_token (snd n, "tag", "duplicate attribute"));
          acc
        end else
          attr :: acc
      ) [] expanded_attributes
      |> List.rev
    in
    i.open_elements := (l, expanded_name, raw_name) :: !(i.open_elements);
    emit_signal i l (`Start_element (expanded_name, attributes)))

and p_pop i l =
  match !(i.open_elements) with
  | [] -> ()
  | _::more ->
    Namespace.Parsing.pop i.namespaces;
    i.open_elements := more;
    emit_signal i l `End_element

(* ---- next_signal ---- *)

let next_signal i =
  match i.prepend with
  | x :: rest -> i.prepend <- rest; Some x
  | [] ->
    while Queue.is_empty i.queue && not i.done_ do
      match i.pending_tokens with
      | tok :: rest ->
        i.pending_tokens <- rest;
        i.parse_state i tok
      | [] ->
        i.tok_state i
    done;
    if Queue.is_empty i.queue then None
    else Some (Queue.pop i.queue)

(* ---- make ---- *)

let make ~report ~resolve_reference ~namespace ~context ~decoder =
  let namespaces = Namespace.Parsing.init namespace in
  let is_fragment, fragment_allowed, initial_parse_state =
    match context with
    | None -> false, true, p_initial
    | Some `Document -> false, false, p_document
    | Some `Fragment -> true, true, p_content
  in
  let chk = {active = false; start_c = -1; buf = []} in
  let i = {
    decoder;
    c = -2; (* sentinel: position won't be updated on first nextc *)
    cr = false;
    line = 1;
    col = 1;
    first_char = true;
    is_valid_char = Common.is_valid_xml_char;
    report;
    pushback = [];
    chk;
    tok_state = initial_tok;
    resolve_reference;
    parse_state = initial_parse_state;
    open_elements = ref [];
    namespaces;
    is_fragment;
    fragment_allowed;
    context;
    text_buf = Buffer.create 256;
    text_loc = (1, 1);
    in_text = false;
    prepend = [];
    queue = Queue.create ();
    done_ = false;
    pending_tokens = [];
  } in
  (* Prime the lookahead. c = -2 so position update is skipped. *)
  nextc i;
  i
