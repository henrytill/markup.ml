(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(** Encoding detection for HTML and XML.
    All functions take a byte_src (unit -> int) that may be partially consumed
    during detection, and return a replaying byte_src that makes all consumed
    bytes available again. *)

val select_html :
  ?limit:int ->
  (unit -> int) ->
    Encoding.t * (unit -> int)

val select_xml :
  (unit -> int) ->
    Encoding.t * (unit -> int)

(* The following values are exposed for testing. *)

val normalize_name : bool -> string -> string

val guess_from_bom_html :
  (unit -> int) -> string option * (unit -> int)

val guess_from_bom_xml :
  (unit -> int) -> string option * (unit -> int)

val guess_family_xml :
  (unit -> int) -> string option * (unit -> int)

val meta_tag_prescan :
  ?supported:(string -> bool) ->
  ?limit:int ->
  (unit -> int) ->
    string option

val read_xml_encoding_declaration :
  (unit -> int) -> Encoding.t -> string option
