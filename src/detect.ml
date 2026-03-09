(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Encoding

(** Make a buffering byte_src wrapper.
    Returns (recording_src, make_replay) where:
    - recording_src reads from src and records bytes
    - make_replay () returns a new byte_src that replays all recorded bytes
      followed by the remaining bytes from src *)
let make_buffered_src src =
  let buf = Buffer.create 8 in
  let recording_src () =
    let b = src () in
    if b >= 0 then Buffer.add_char buf (Char.chr b);
    b
  in
  let make_replay () =
    let s = Buffer.contents buf in
    let i = ref 0 in
    fun () ->
      if !i < String.length s then begin
        let b = Char.code s.[!i] in
        i := !i + 1; b
      end else src ()
  in
  recording_src, make_replay

(** Read up to n bytes from src, returning the list of bytes read (as ints).
    Stops early at EOF. *)
let read_n_bytes n src =
  let acc = ref [] in
  let count = ref 0 in
  while !count < n do
    let b = src () in
    if b = -1 then count := n  (* stop *)
    else begin acc := b :: !acc; incr count end
  done;
  List.rev !acc

(** Given a list of pre-read bytes and the original src, return a new src
    that replays those bytes then continues from src. *)
let make_replay_src bytes src =
  let remaining = ref bytes in
  fun () ->
    match !remaining with
    | [] -> src ()
    | b :: rest -> remaining := rest; b

let name_to_encoding = function
  | "utf-8" -> Some utf_8
  | "utf-16be" -> Some utf_16be
  | "utf-16le" -> Some utf_16le
  | "iso-8859-1" -> Some iso_8859_1
  | "iso-8859-15" -> Some iso_8859_15
  | "us-ascii" -> Some us_ascii
  | "windows-1251" -> Some windows_1251
  | "windows-1252" -> Some windows_1252
  | "ucs-4be" -> Some ucs_4be
  | "ucs-4le" -> Some ucs_4le
  | _ -> None

(* 8.2.2.2. *)
let guess_from_bom_html src =
  let bytes = read_n_bytes 3 src in
  let replay = make_replay_src bytes src in
  let result =
    match bytes with
    | 0xFE :: 0xFF :: _ -> Some "utf-16be"
    | 0xFF :: 0xFE :: _ -> Some "utf-16le"
    | [0xEF; 0xBB; 0xBF] -> Some "utf-8"
    | _ -> None
  in
  result, replay

(* Appendix F.1. *)
let guess_from_bom_xml src =
  let bytes = read_n_bytes 4 src in
  let replay = make_replay_src bytes src in
  let result =
    match bytes with
    | [0x00; 0x00; 0xFE; 0xFF] -> Some "ucs-4be"
    | [0xFF; 0xFE; 0x00; 0x00] -> Some "ucs-4le"
    | [0x00; 0x00; 0xFF; 0xFE] -> Some "ucs-4be-transposed"
    | [0xFE; 0xFF; 0x00; 0x00] -> Some "ucs-4le-transposed"
    | 0xFE :: 0xFF :: _ -> Some "utf-16be"
    | 0xFF :: 0xFE :: _ -> Some "utf-16le"
    | 0xEF :: 0xBB :: 0xBF :: _ -> Some "utf-8"
    | _ -> None
  in
  result, replay

(* Appendix F.1. *)
let guess_family_xml src =
  let bytes = read_n_bytes 4 src in
  let replay = make_replay_src bytes src in
  let result =
    match bytes with
    | [0x00; 0x00; 0x00; 0x3C] -> Some "ucs-4be"
    | [0x3C; 0x00; 0x00; 0x00] -> Some "ucs-4le"
    | [0x00; 0x00; 0x3C; 0x00] -> Some "ucs-4be-transposed"
    | [0x00; 0x3C; 0x00; 0x00] -> Some "ucs-4le-transposed"
    | [0x00; 0x3C; 0x00; 0x3F] -> Some "utf-16be"
    | [0x3C; 0x00; 0x3F; 0x00] -> Some "utf-16le"
    | [0x3C; 0x3F; 0x78; 0x6D] -> Some "utf-8"
    | [0x4C; 0x6F; 0xA7; 0x94] -> Some "ebcdic"
    | _ -> None
  in
  result, replay

(* 5.2 in the Encoding Candidate Recommendation. *)
let normalize_name for_html s =
  match String.lowercase_ascii (trim_string s) with
  | "unicode-1-1-utf-8" | "utf-8" | "utf8" ->
    "utf-8"

  | "866" | "cp866" | "csibm866" | "ibm866" ->
    "ibm866"

  | "csisolatin2" | "iso-8859-2" | "iso-ir-101" | "iso8859-2" | "iso88592"
  | "iso_8859-2" | "iso_8859-2:1987" | "l2" | "latin2" ->
    "iso-8859-2"

  | "csisolatin3" | "iso-8859-3" | "iso-ir-109" | "iso8859-3" | "iso88593"
  | "iso_8859-3" | "iso_8859-3:1988" | "l3" | "latin3" ->
    "iso-8859-3"

  | "csisolatin4" | "iso-8859-4" | "iso-ir-110" | "iso8859-4" | "iso88594"
  | "iso_8859-4" | "iso_8859-4:1988" | "l4" | "latin4" ->
    "iso-8859-4"

  | "csisolatincyrillic" | "cyrillic" | "iso-8859-5" | "iso-ir-144"
  | "iso8859-5" | "iso88595" | "iso_8859-5" | "iso_8859-5:1988" ->
    "iso-8859-5"

  | "arabic" | "asmo-708" | "csiso88596e" | "csiso88596i" | "csisolatinarabic"
  | "ecma-114" | "iso-8859-6" | "iso-8859-6-e" | "iso-8859-6-i" | "iso-ir-127"
  | "iso8859-6" | "iso88596" | "iso_8859-6" | "iso_8859-6:1987" ->
    "iso-8859-6"

  | "csisolatingreek" | "ecma-118" | "elot_928" | "greek" | "greek8"
  | "iso-8859-7" | "iso-ir-126" | "iso8859-7" | "iso88597" | "iso_8859-7"
  | "iso_8859-7:1987" | "sun_eu_greek" ->
    "iso-8859-7"

  | "csiso88598e" | "csisolatinhebrew" | "hebrew" | "iso-8859-8"
  | "iso-8859-8-e" | "iso-ir-138" | "iso8859-8" | "iso88598" | "iso_8859-8"
  | "iso_8859-8:1988" | "visual" ->
    "iso-8859-8"

  | "csiso88598i" | "iso-8859-8-i" | "logical" ->
    "iso-8859-8-i"

  | "csisolatin6" | "iso-8859-10" | "iso-ir-157" | "iso8859-10" | "iso885910"
  | "l6" | "latin6" ->
    "iso-8859-10"

  | "iso-8859-13" | "iso8859-13" | "iso885913" ->
    "iso-8859-13"

  | "iso-8859-14" | "iso8859-14" | "iso885914" ->
    "iso-8859-14"

  | "csisolatin9" | "iso-8859-15" | "iso8859-15" | "iso885915" | "iso_8859-15"
  | "l9" ->
    "iso-8859-15"

  | "iso-8859-16" ->
    "iso-8859-16"

  | "cskoi8r" | "koi" | "koi8" | "koi8-r" | "koi8_r" ->
    "koi8-r"

  | "koi8-ru" | "koi8-u" ->
    "koi8-u"

  | "csmacintosh" | "mac" | "macintosh" | "x-mac-roman" ->
    "macintosh"

  | "dos-874" | "iso-8859-11" | "iso8859-11" | "iso885911" | "tis-620"
  | "windows-874" ->
    "windows-874"

  | "cp1250" | "windows-1250" | "x-cp1250" ->
    "windows-1250"

  | "cp1251" | "windows-1251" | "x-cp1251" ->
    "windows-1251"

  | "ansi_x3.4-1968" | "ascii" | "us-ascii" ->
    if for_html then "windows-1252" else "us-ascii"

  | "cp819" | "csisolatin1" | "ibm819" | "iso-8859-1" | "iso-ir-100"
  | "iso8859-1" | "iso88591" | "iso_8859-1" | "iso_8859-1:1987" | "l1"
  | "latin1" ->
    if for_html then "windows-1252" else "iso-8859-1"

  | "cp1252" | "windows-1252" | "x-cp1252" ->
    "windows-1252"

  | "cp1253" | "windows-1253" | "x-cp1253" ->
    "windows-1253"

  | "cp1254" | "csisolatin5" | "iso-8859-9" | "iso-ir-148" | "iso8859-9"
  | "iso88599" | "iso_8859-9" | "iso_8859-9:1989" | "l5" | "latin5"
  | "windows-1254" | "x-cp1254" ->
    "windows-1254"

  | "cp1255" | "windows-1255" | "x-cp1255" ->
    "windows-1255"

  | "cp1256" | "windows-1256" | "x-cp1256" ->
    "windows-1256"

  | "cp1257" | "windows-1257" | "x-cp1257" ->
    "windows-1257"

  | "cp1258" | "windows-1258" | "x-cp1258" ->
    "windows-1258"

  | "x-mac-cyrillic" | "x-mac-ukrainian" ->
    "x-mac-cyrillic"

  | "chinese" | "csgb2312" | "csiso58gb231280" | "gb2312" | "gb_2312"
  | "gb_2312-80" | "gbk" | "iso-ir-58" | "x-gbk" ->
    "gbk"

  | "gb18030" ->
    "gb18030"

  | "big5" | "big5-hkscs" | "cn-big5" | "csbig5" | "x-x-big5" ->
    "big5"

  | "cseucpkdfmtjapanese" | "euc-jp" | "x-euc-jp" ->
    "euc-jp"

  | "csiso2022jp" | "iso-2022-jp" ->
    "iso-2022-jp"

  | "csshiftjis" | "ms932" | "ms_kanji" | "shift-jis" | "shift_jis" | "sjis"
  | "windows-31j" | "x-sjis" ->
    "shift_jis"

  | "cseuckr" | "csksc56011987" | "euc-kr" | "iso-ir-149" | "korean"
  | "ks_c_5601-1987" | "ks_c_5601-1989" | "ksc5601" | "ksc_5601"
  | "windows-949" ->
    "euc-kr"

  | "csiso2022kr" | "hz-gb-2312" | "iso-2022-cn" | "iso-2022-cn-ext"
  | "iso-2022-kr" ->
    "replacement"

  | "utf-16be" ->
    "utf-16be"

  | "utf-16" | "utf-16le" ->
    "utf-16le"

  | "x-user-defined" ->
    "x-user-defined"

  | s -> s

(** Direct-style meta_tag_prescan.
    Takes byte_src (which will be consumed); the src is NOT rewound after.
    The caller should use make_buffered_src if rewinding is needed. *)

(* 8.2.2.2 - meta tag prescan, implemented in direct style over byte_src *)
let meta_tag_prescan ?supported ?(limit = 1024) src =
  let is_uppercase c = c >= 'A' && c <= 'Z' in
  let is_lowercase c = c >= 'a' && c <= 'z' in
  let is_letter c = is_uppercase c || is_lowercase c in
  let is_whitespace c = String.contains "\t\n\r\x0C " c in

  (* Limit-counting wrapper *)
  let count = ref 0 in
  let lsrc () =
    if !count >= limit then -1
    else begin
      let b = src () in
      if b >= 0 then incr count;
      b
    end
  in

  (* We use a simple single-element pushback *)
  let pushed_back = ref [] in
  let next () =
    match !pushed_back with
    | c :: rest -> pushed_back := rest; c
    | [] -> lsrc ()
  in
  let push c = pushed_back := c :: !pushed_back in
  let push_list l = pushed_back := l @ !pushed_back in

  let next_char () =
    let b = next () in
    if b = -1 then None else Some (Char.chr b)
  in

  let skip_whitespace () =
    let rec loop () =
      match next_char () with
      | None -> ()
      | Some c when is_whitespace c -> loop ()
      | Some c -> push (Char.code c)
    in loop ()
  in

  (* Returns None if EOF reached before closing quote (unterminated), Some value otherwise *)
  let read_quoted_value quote =
    let buffer = Buffer.create 32 in
    let rec iterate () =
      match next_char () with
      | None -> None
      | Some c when c = quote -> Some (Buffer.contents buffer)
      | Some c ->
        add_utf_8 buffer (Char.code (Char.lowercase_ascii c));
        iterate ()
    in iterate ()
  in

  let read_unquoted_value terminator =
    let buffer = Buffer.create 32 in
    let rec iterate () =
      match next_char () with
      | None -> Buffer.contents buffer
      | Some c when is_whitespace c || c = terminator ->
        push (Char.code c);
        Buffer.contents buffer
      | Some c ->
        add_utf_8 buffer (Char.code (Char.lowercase_ascii c));
        iterate ()
    in iterate ()
  in

  (* 2.6.5 - scan for charset=... in a content attribute value *)
  let extract_encoding_from_string s =
    (* Create a byte_src from s *)
    let i = ref 0 in
    let ssrc () =
      if !i >= String.length s then -1
      else begin let b = Char.code s.[!i] in incr i; b end
    in
    let pushed2 = ref [] in
    let next2 () =
      match !pushed2 with
      | b :: rest -> pushed2 := rest; b
      | [] -> ssrc ()
    in
    let next2_char () =
      let b = next2 () in if b = -1 then None else Some (Char.chr b)
    in
    let push2 c = pushed2 := (Char.code c) :: !pushed2 in

    let skip_ws2 () =
      let rec loop () =
        match next2_char () with
        | None -> ()
        | Some c when is_whitespace c -> loop ()
        | Some c -> push2 c
      in loop ()
    in

    let read_qval2 quote =
      let buf = Buffer.create 32 in
      let rec loop () =
        match next2_char () with
        | None -> Buffer.contents buf
        | Some c when c = quote -> Buffer.contents buf
        | Some c ->
          add_utf_8 buf (Char.code (Char.lowercase_ascii c));
          loop ()
      in loop ()
    in

    let read_uval2 term =
      let buf = Buffer.create 32 in
      let rec loop () =
        match next2_char () with
        | None -> Buffer.contents buf
        | Some c when is_whitespace c || c = term ->
          push2 c;
          Buffer.contents buf
        | Some c ->
          add_utf_8 buf (Char.code (Char.lowercase_ascii c));
          loop ()
      in loop ()
    in

    let rec scan () =
      match next2_char () with
      | None -> None
      | Some 'c' ->
        (* Try to read "harset" *)
        let rest = Array.make 6 ' ' in
        let n = ref 0 in
        while !n < 6 do
          match next2_char () with
          | None -> n := 6
          | Some c -> rest.(!n) <- c; incr n
        done;
        let rest_s = String.init 6 (fun i -> rest.(i)) in
        if String.lowercase_ascii rest_s = "harset" then begin
          skip_ws2 ();
          match next2_char () with
          | None -> None
          | Some '=' ->
            skip_ws2 ();
            (match next2_char () with
            | None -> None
            | Some ('"' | '\'' as q) ->
              let v = read_qval2 q in
              if v = "" then None else Some v
            | Some c ->
              push2 c;
              let v = read_uval2 ';' in
              if v = "" then None else Some v)
          | Some c ->
            push2 c;
            scan ()
        end else
          scan ()
      | Some _ -> scan ()
    in
    scan ()
  in

  let get_attribute () =
    (* Skip leading whitespace and '/' *)
    let rec skip_leading () =
      match next_char () with
      | None -> None
      | Some c when is_whitespace c || c = '/' -> skip_leading ()
      | Some c -> push (Char.code c); Some ()
    in
    match skip_leading () with
    | None -> None
    | Some () ->
      (* Read name *)
      let name_buf = Buffer.create 32 in
      let rec read_name () =
        match next_char () with
        | Some '=' when Buffer.length name_buf > 0 ->
          push (Char.code '=');
          Some (Buffer.contents name_buf)
        | Some '/' | Some '>' | None as opt ->
          (match opt with
          | Some c -> push (Char.code c)
          | None -> ());
          if Buffer.length name_buf = 0 then
            None  (* no attr, no value *)
          else
            Some (Buffer.contents name_buf)  (* attr with no value *)
        | Some c when is_whitespace c ->
          Some (Buffer.contents name_buf)
        | Some c ->
          add_utf_8 name_buf (Char.code (Char.lowercase_ascii c));
          read_name ()
      in
      match read_name () with
      | None ->
        (* Saw '/', '>' or None with empty name - signal end of tag *)
        None
      | Some name ->
        skip_whitespace ();
        match next_char () with
        | Some '=' ->
          skip_whitespace ();
          (match next_char () with
          | Some ('"' | '\'' as q) ->
            (match read_quoted_value q with
            | None -> None  (* EOF before closing quote: abort attribute *)
            | Some v -> Some (name, v))
          | Some c ->
            push (Char.code c);
            let v = read_unquoted_value '>' in
            Some (name, v)
          | None ->
            let v = read_unquoted_value '>' in
            Some (name, v))
        | Some c ->
          push (Char.code c);
          Some (name, "")
        | None ->
          Some (name, "")
  in

  let result = ref None in
  let finished = ref false in

  let finish v = result := v; finished := true in

  let everything _ = true in
  let supported_fn = match supported with
    | None -> everything
    | Some f -> f
  in

  let read_attributes () =
    let names = ref [] in
    let got_pragma = ref false in
    let need_pragma = ref None in
    let charset = ref None in
    let cont = ref true in
    while !cont && not !finished do
      match get_attribute () with
      | None -> cont := false
      | Some (name, value) ->
        if list_mem_string name !names then ()
        else begin
          names := name :: !names;
          match name with
          | "http-equiv" ->
            if value = "content-type" then got_pragma := true
          | "content" ->
            if !charset = None then begin
              match extract_encoding_from_string value with
              | None -> ()
              | Some enc ->
                charset := Some enc;
                need_pragma := Some true
            end
          | "charset" ->
            if value <> "" then begin
              charset := Some value;
              need_pragma := Some false
            end
          | _ -> ()
        end
    done;
    match !need_pragma with
    | None -> ()
    | Some np ->
      if np && not !got_pragma then ()
      else
        match !charset with
        | None -> ()
        | Some cs ->
          let cs =
            match normalize_name true cs with
            | "utf-16be" | "utf-16le" | "utf-16" -> "utf-8"
            | s -> s
          in
          if supported_fn cs then
            finish (Some cs)
  in

  (* Close comment: called after consuming '<!-'. Reads until '-->' is found.
     The caller pushes back one '-' before calling, so the source starts with '-'.
     This matches the old kstream-based behavior. *)
  let close_comment () =
    let rec loop () =
      if !finished then ()
      else
        match next_char () with
        | None -> finish None
        | Some '-' ->
          let b2 = next_char () in
          let b3 = next_char () in
          (match b2, b3 with
          | Some '-', Some '>' -> ()
          | _ ->
            (match b2 with Some c -> push (Char.code c) | None -> ());
            (match b3 with Some c -> push (Char.code c) | None -> ());
            loop ())
        | Some _ -> loop ()
    in
    loop ()
  in

  let close_tag () =
    (* Skip to '>' or whitespace, then drain attributes *)
    let rec skip () =
      match next_char () with
      | None -> finish None
      | Some c when is_whitespace c || c = '>' ->
        push (Char.code c);
        let cont = ref true in
        while !cont do
          match get_attribute () with
          | None -> cont := false
          | Some _ -> ()
        done
      | Some _ -> skip ()
    in
    skip ()
  in

  let close_tag_like () =
    let rec loop () =
      match next_char () with
      | None -> finish None
      | Some '>' -> ()
      | Some _ -> loop ()
    in loop ()
  in

  let rec scan () =
    if !finished then ()
    else
      match next_char () with
      | None -> finish None
      | Some '<' ->
        (match next_char () with
        | None -> finish None
        | Some '!' ->
          (* peek next 2 chars to check for <!-- *)
          let c1 = next_char () in
          let c2 = next_char () in
          (match c1, c2 with
          | Some '-', Some '-' ->
            (* Push back '!', '-', '-' to replicate old kstream behavior where
               close_comment was called with '!--...' still in stream. This makes
               '<!-->' work: close_comment reads '!' (skip), '-', then next 2 = '-',
               '>' which matches '-->'. *)
            push_list [Char.code '!'; Char.code '-'; Char.code '-'];
            close_comment (); scan ()
          | _ ->
            (match c1 with Some c -> push (Char.code c) | None -> ());
            (match c2 with Some c -> push (Char.code c) | None -> ());
            close_tag_like (); scan ())
        | Some '/' ->
          (* peek next char *)
          let c1 = next_char () in
          (match c1 with
          | Some c when is_letter c ->
            push (Char.code c);
            close_tag (); scan ()
          | _ ->
            (match c1 with Some c -> push (Char.code c) | None -> ());
            close_tag_like (); scan ())
        | Some '?' ->
          close_tag_like (); scan ()
        | Some 'm' ->
          (* peek 4 more chars to check for "eta" + whitespace *)
          let c1 = next_char () in
          let c2 = next_char () in
          let c3 = next_char () in
          let c4 = next_char () in
          let chars = List.filter_map (fun x -> x) [c1; c2; c3; c4] in
          let s4 = String.init (List.length chars) (List.nth chars) in
          let s4_lower = String.lowercase_ascii s4 in
          if String.length s4_lower >= 4 &&
             s4_lower.[0] = 'e' && s4_lower.[1] = 't' &&
             s4_lower.[2] = 'a' &&
             (is_whitespace s4_lower.[3] || s4_lower.[3] = '/') then begin
            read_attributes (); scan ()
          end else begin
            (* push back the 4 chars we read, plus 'm' *)
            push_list (List.map Char.code chars);
            push (Char.code 'm');
            close_tag (); scan ()
          end
        | Some c when is_letter c ->
          push (Char.code c);
          close_tag (); scan ()
        | Some c ->
          push (Char.code c);
          scan ())
      | Some _ -> scan ()
  in

  scan ();
  !result


let read_xml_encoding_declaration byte_src (family : Encoding.t) =
  let int_ks = Encoding.decoder_to_kstream
    (family ~report:Error.ignore_errors ~byte_src)
  in
  let (processed, _get_loc) =
    Input.preprocess is_valid_xml_char Error.ignore_errors int_ks
  in
  let tokens =
    Xml_tokenizer.tokenize Error.ignore_errors (fun _ -> None)
      (processed, fun () -> (1, 1))
  in

  let result = ref None in
  let cont = ref true in
  while !cont do
    let r = ref None in
    Kstream.next_option tokens raise (fun v -> r := v);
    match !r with
    | None -> cont := false
    | Some (_, `Xml {Common.encoding}) ->
      result := encoding; cont := false
    | Some (_, `Comment _) -> ()
    | Some (_, `Chars ss) when List.for_all is_whitespace_only ss -> ()
    | Some _ -> cont := false
  done;
  !result

let name_to_encoding_or_utf_8 encoding =
  match name_to_encoding encoding with
  | Some e -> e
  | None -> utf_8

let select_html ?limit byte_src =
  let rec_src, make_replay = make_buffered_src byte_src in
  let bom_result, _bom_replay = guess_from_bom_html rec_src in
  (* The rec_src recorded all bytes read by guess_from_bom_html.
     make_replay () will give us a src that replays those bytes + rest of orig. *)
  let replay = make_replay () in
  match bom_result with
  | Some encoding -> name_to_encoding_or_utf_8 encoding, replay
  | None ->
    (* For meta_tag_prescan, we need to buffer everything read *)
    let rec_src2, make_replay2 = make_buffered_src replay in
    let prescan_result = meta_tag_prescan ?limit rec_src2 in
    let replay2 = make_replay2 () in
    match prescan_result with
    | Some encoding -> name_to_encoding_or_utf_8 encoding, replay2
    | None -> utf_8, replay2

let select_xml byte_src =
  let rec_src, make_replay = make_buffered_src byte_src in
  let bom_result, _bom_replay = guess_from_bom_xml rec_src in
  let replay = make_replay () in
  match bom_result with
  | Some encoding -> name_to_encoding_or_utf_8 encoding, replay
  | None ->
    let rec_src2, make_replay2 = make_buffered_src replay in
    let family_result, _fam_replay = guess_family_xml rec_src2 in
    let replay2 = make_replay2 () in
    let name, family =
      match family_result with
      | None -> "utf-8", utf_8
      | Some family_name -> family_name, name_to_encoding_or_utf_8 family_name
    in
    let rec_src3, make_replay3 = make_buffered_src replay2 in
    let enc_decl = read_xml_encoding_declaration rec_src3 family in
    let replay3 = make_replay3 () in
    match enc_decl with
    | None -> name_to_encoding_or_utf_8 name, replay3
    | Some encoding ->
      match name, normalize_name false encoding with
      | "utf-8", "iso-8859-1" -> iso_8859_1, replay3
      | "utf-8", "us-ascii" -> us_ascii, replay3
      | "utf-8", "windows-1251" -> windows_1251, replay3
      | "utf-8", "windows-1252" -> windows_1252, replay3
      | _ -> name_to_encoding_or_utf_8 name, replay3
