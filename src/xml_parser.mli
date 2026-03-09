(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val parse :
  [< `Document | `Fragment ] option ->
  (string -> string option) ->
  Error.parse_handler ->
  (location * Xml_tokenizer.token) Kstream.t ->
    (location * signal) Kstream.t

type checkpoint = {
  mutable active   : bool;
  mutable start_c  : int;
  mutable buf      : int list;
}

type xml_input = {
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
  mutable tok_state : xml_input -> unit;
  resolve_reference : string -> string option;
  mutable parse_state : xml_input -> (location * Xml_tokenizer.token) -> unit;
  open_elements    : (location * Common.name * string) list ref;
  namespaces       : Namespace.Parsing.context;
  mutable is_fragment : bool;
  mutable fragment_allowed : bool;
  context          : [ `Document | `Fragment ] option;
  text_buf         : Buffer.t;
  mutable text_loc : location;
  mutable in_text  : bool;
  mutable prepend  : (location * Common.signal) list;
  queue            : (location * Common.signal) Queue.t;
  mutable done_    : bool;
  mutable pending_tokens : (location * Xml_tokenizer.token) list;
}

val make :
  report:Error.parse_handler ->
  resolve_reference:(string -> string option) ->
  namespace:(string -> string option) ->
  context:[ `Document | `Fragment ] option ->
  decoder:(unit -> int) ->
  xml_input

val next_signal : xml_input -> (location * signal) option
